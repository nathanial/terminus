-- TerminusTests.DebugTests: Tests for Debug mode

import Crucible
import Terminus.Reactive
import Staple

namespace TerminusTests.DebugTests

open Terminus
open Terminus.Reactive
open Terminus.Reactive.Debug
open Crucible
open Reactive Reactive.Host

testSuite "Debug Tests"

-- ============================================================================
-- Buffer.toPlainText tests
-- ============================================================================

test "Buffer.toPlainText converts simple text" := do
  let buf := (Buffer.new 10 2).writeString 0 0 "Hello" {}
  let text := buf.toPlainText
  (text.startsWith "Hello") ≡ true

test "Buffer.toPlainText trims trailing spaces" := do
  let buf := (Buffer.new 10 2).writeString 0 0 "Hi" {}
  let lines := buf.toPlainText.splitOn "\n"
  -- First line should be "Hi" (trimmed from "Hi        ")
  match lines.head? with
  | some line => (line == "Hi") ≡ true
  | none => ensure false "expected at least one line"

test "Buffer.toPlainText handles CJK wide characters" := do
  let buf := (Buffer.new 10 2).writeString 0 0 "中文" {}
  let text := buf.toPlainText
  -- Should contain both characters without duplicates from placeholders
  (Staple.String.containsSubstr text "中文") ≡ true
  -- Should not have extra characters (placeholders should be skipped)
  let firstLine := (text.splitOn "\n").head?.getD ""
  (firstLine == "中文") ≡ true

test "Buffer.toPlainText handles multiple rows" := do
  let buf := (Buffer.new 10 3)
    |>.writeString 0 0 "Row1" {}
    |>.writeString 0 1 "Row2" {}
    |>.writeString 0 2 "Row3" {}
  let text := buf.toPlainText
  let lines := text.splitOn "\n"
  lines.length ≡ 3
  match lines with
  | [l1, l2, l3] =>
    l1 ≡ "Row1"
    l2 ≡ "Row2"
    l3 ≡ "Row3"
  | _ => ensure false "expected 3 lines"

-- ============================================================================
-- hasChanged tests
-- ============================================================================

test "hasChanged returns true for first frame (none previous)" := do
  let buf := Buffer.new 10 5
  (hasChanged none buf) ≡ true

test "hasChanged returns false for identical buffers" := do
  let buf := (Buffer.new 10 5).writeString 0 0 "Test" {}
  (hasChanged (some buf) buf) ≡ false

test "hasChanged returns true when content differs" := do
  let buf1 := (Buffer.new 10 5).writeString 0 0 "A" {}
  let buf2 := (Buffer.new 10 5).writeString 0 0 "B" {}
  (hasChanged (some buf1) buf2) ≡ true

-- ============================================================================
-- InputScript tests
-- ============================================================================

test "InputScript.fromString creates key events" := do
  let script := InputScript.fromString "abc"
  script.length ≡ 3
  match script with
  | [e1, e2, e3] =>
    e1.event ≡ .key (KeyEvent.char 'a')
    e2.event ≡ .key (KeyEvent.char 'b')
    e3.event ≡ .key (KeyEvent.char 'c')
  | _ => ensure false "expected 3 events"

test "InputScript.fromKeyCodes creates key events" := do
  let script := InputScript.fromKeyCodes [.enter, .escape, .up]
  script.length ≡ 3
  match script with
  | [e1, e2, e3] =>
    e1.event ≡ .key { code := .enter }
    e2.event ≡ .key { code := .escape }
    e3.event ≡ .key { code := .up }
  | _ => ensure false "expected 3 events"

test "InputScript.push adds events" := do
  let script := InputScript.fromString "a"
  let script := InputScript.pushKey script KeyEvent.enter
  script.length ≡ 2

test "InputScript.pushResize adds resize event" := do
  let script := InputScript.pushResize ([] : InputScript) 100 50
  script.length ≡ 1
  match script.head? with
  | some ev => ev.event ≡ .resize 100 50
  | none => ensure false "expected event"

-- ============================================================================
-- runDebugCapture integration tests
-- ============================================================================

/-- Simple test app that displays a counter incremented by key presses. -/
def counterApp : ReactiveTermM ReactiveAppState := do
  let keyEvents ← useKeyEvent
  let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 keyEvents
  let (_, render) ← runWidget do
    let node ← countDyn.map' (fun n => RNode.text s!"Count: {n}" {})
    emit node
  pure { render }

test "runDebugCapture captures initial frame" := do
  let config : DebugConfig := { width := 20, height := 5, writeFiles := false }
  let script : InputScript := []  -- Empty script = just initial frame
  let state ← runDebugCapture counterApp config script
  -- Should have at least 1 frame (the initial render)
  ensure (state.capturedFrames.size >= 1) "expected at least 1 frame"
  -- First frame should show "Count: 0"
  if h : 0 < state.capturedFrames.size then
    let (_, content) := state.capturedFrames[0]
    (Staple.String.containsSubstr content "Count: 0") ≡ true
  else
    ensure false "expected frame content"

test "runDebugCapture captures frame changes on key input" := do
  let config : DebugConfig := { width := 20, height := 5, writeFiles := false }
  -- Send a key to increment counter
  let script := InputScript.fromString "x"
  let state ← runDebugCapture counterApp config script
  -- Should have 2 frames: initial (Count: 0) and after key (Count: 1)
  ensure (state.capturedFrames.size >= 2) s!"expected at least 2 frames, got {state.capturedFrames.size}"
  -- Check that we have both states captured
  let contents := state.capturedFrames.map (·.2)
  (contents.any (fun c => Staple.String.containsSubstr c "Count: 0")) ≡ true
  (contents.any (fun c => Staple.String.containsSubstr c "Count: 1")) ≡ true

test "runDebugCapture skips unchanged frames" := do
  -- App that doesn't change on key press
  let staticApp : ReactiveTermM ReactiveAppState := do
    let (_, render) ← runWidget do
      text' "Static" {}
    pure { render }
  let config : DebugConfig := { width := 20, height := 5, writeFiles := false }
  -- Send multiple keys - should still only have 1 frame since content doesn't change
  let script := InputScript.fromString "abc"
  let state ← runDebugCapture staticApp config script
  -- Should only have 1 frame since content never changes
  state.framesWritten ≡ 1

/-- App that shows last key pressed. -/
def keyDisplayApp : ReactiveTermM ReactiveAppState := do
  let keyEvents ← useKeyEvent
  let lastKeyDyn ← Reactive.foldDyn (fun kd _ =>
    match kd.event.code with
    | .char c => s!"Key: {c}"
    | _ => "Key: special"
  ) "Key: none" keyEvents
  let (_, render) ← runWidget do
    let node ← lastKeyDyn.map' (fun s => RNode.text s {})
    emit node
  pure { render }

test "runDebugCapture captures multiple distinct frames" := do
  let config : DebugConfig := { width := 20, height := 5, writeFiles := false }
  let script := InputScript.fromString "xy"
  let state ← runDebugCapture keyDisplayApp config script
  -- Should have 3 frames: initial, after 'x', after 'y'
  ensure (state.framesWritten >= 3) s!"expected at least 3 frames, got {state.framesWritten}"
  let contents := state.capturedFrames.map (·.2)
  (contents.any (fun c => Staple.String.containsSubstr c "Key: none")) ≡ true
  (contents.any (fun c => Staple.String.containsSubstr c "Key: x")) ≡ true
  (contents.any (fun c => Staple.String.containsSubstr c "Key: y")) ≡ true

-- ============================================================================
-- DebugConfig tests
-- ============================================================================

test "DebugConfig has sensible defaults" := do
  let config : DebugConfig := {}
  config.width ≡ 80
  config.height ≡ 24
  config.writeFiles ≡ true
  (config.outputDir.toString == ".debug") ≡ true

end TerminusTests.DebugTests
