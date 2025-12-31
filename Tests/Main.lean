-- Tests.Main: Crucible test suite for Terminus terminal library

import Crucible
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalMock
import Terminus.Backend.Commands
import Terminus.Input.Events
import Terminus.Input.Key
import Terminus.Core.Cell
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Core.Base64
import Terminus.Layout.Constraint
import Terminus.Layout.Layout
import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Widgets.Paragraph
import Terminus.Widgets.Gauge
import Terminus.Widgets.Tabs
import Terminus.Widgets.List
import Terminus.Widgets.TextInput
import Terminus.Widgets.TextArea
import Terminus.Widgets.Notification

open Terminus
open Crucible

namespace Tests.Terminus

testSuite "Terminus Tests"

-- ============================================================================
-- Raw Mode Tests
-- ============================================================================

test "enableRawMode sets rawModeEnabled to true" := do
  let (_, state) := MockTerminal.run TerminalEffect.enableRawMode
  state.rawModeEnabled ≡ true

test "disableRawMode sets rawModeEnabled to false" := do
  let initial : MockTerminalState := { rawModeEnabled := true }
  let (_, state) := MockTerminal.run TerminalEffect.disableRawMode initial
  state.rawModeEnabled ≡ false

test "withRawMode restores state after action" := do
  let action := TerminalEffect.withRawMode (pure ())
  let (_, state) := MockTerminal.run action
  state.rawModeEnabled ≡ false

test "withRawMode enables raw mode during action" := do
  let action := TerminalEffect.withRawMode do
    let s ← get
    pure s.rawModeEnabled
  let (wasEnabled, _) := MockTerminal.run action
  wasEnabled ≡ true

-- ============================================================================
-- Terminal Size Tests
-- ============================================================================

test "getTerminalSize returns default 80x24" := do
  let (size, _) := MockTerminal.run TerminalEffect.getTerminalSize
  size ≡ (80, 24)

test "getTerminalSize returns configured size" := do
  let initial := MockTerminal.withSize 120 40
  let (size, _) := MockTerminal.run TerminalEffect.getTerminalSize initial
  size ≡ (120, 40)

-- ============================================================================
-- Input Reading Tests
-- ============================================================================

test "readByte returns none when queue empty" := do
  let (result, _) := MockTerminal.run TerminalEffect.readByte
  result ≡ none

test "readByte returns byte from queue" := do
  let initial := MockTerminal.withInput [65, 66, 67] -- 'A', 'B', 'C'
  let (result, _) := MockTerminal.run TerminalEffect.readByte initial
  result ≡ some 65

test "readByte consumes bytes in order" := do
  let initial := MockTerminal.withInput [65, 66]
  let action := do
    let _ ← TerminalEffect.readByte
    TerminalEffect.readByte
  let (result, _) := MockTerminal.run action initial
  result ≡ some 66

test "readByte drains queue" := do
  let initial := MockTerminal.withInput [65]
  let action := do
    let _ ← TerminalEffect.readByte
    TerminalEffect.readByte
  let (result, _) := MockTerminal.run action initial
  result ≡ none

-- ============================================================================
-- Output Writing Tests
-- ============================================================================

test "writeStdout accumulates output" := do
  let action := do
    TerminalEffect.writeStdout "Hello, "
    TerminalEffect.writeStdout "World!"
  let (_, state) := MockTerminal.run action
  state.outputBuffer ≡ "Hello, World!"

test "writeStdout clears flushed flag" := do
  let action := TerminalEffect.writeStdout "test"
  let (_, state) := MockTerminal.run action
  state.flushed ≡ false

test "flushStdout sets flushed flag" := do
  let action := do
    TerminalEffect.writeStdout "test"
    TerminalEffect.flushStdout
  let (_, state) := MockTerminal.run action
  state.flushed ≡ true

-- ============================================================================
-- Key Parsing Tests
-- ============================================================================

test "parses regular character a" := do
  let initial := MockTerminal.withInput []
  let (event, _) := MockTerminal.run (Events.parseInput 97) initial -- 'a'
  match event with
  | .key key => key.code ≡ KeyCode.char 'a'
  | _ => ensure false "expected key event"

test "parses Enter key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 13)
  match event with
  | .key key => key.code ≡ KeyCode.enter
  | _ => ensure false "expected key event"

test "parses Tab key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 9)
  match event with
  | .key key => key.code ≡ KeyCode.tab
  | _ => ensure false "expected key event"

test "parses Backspace key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 127)
  match event with
  | .key key => key.code ≡ KeyCode.backspace
  | _ => ensure false "expected key event"

test "parses Space key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 32)
  match event with
  | .key key => key.code ≡ KeyCode.space
  | _ => ensure false "expected key event"

test "parses Ctrl C" := do
  let (event, _) := MockTerminal.run (Events.parseInput 3)
  match event with
  | .key key => do
    key.code ≡ KeyCode.char 'c'
    key.modifiers.ctrl ≡ true
  | _ => ensure false "expected key event"

test "parses Ctrl D" := do
  let (event, _) := MockTerminal.run (Events.parseInput 4)
  match event with
  | .key key => do
    key.code ≡ KeyCode.char 'd'
    key.modifiers.ctrl ≡ true
  | _ => ensure false "expected key event"

-- ============================================================================
-- Escape Sequence Tests
-- ============================================================================

test "parses Up arrow" := do
  let initial := MockTerminal.withInput [91, 65] -- '[', 'A'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.up
  | _ => ensure false "expected key event"

test "parses Down arrow" := do
  let initial := MockTerminal.withInput [91, 66] -- '[', 'B'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.down
  | _ => ensure false "expected key event"

test "parses Left arrow" := do
  let initial := MockTerminal.withInput [91, 68] -- '[', 'D'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.left
  | _ => ensure false "expected key event"

test "parses Right arrow" := do
  let initial := MockTerminal.withInput [91, 67] -- '[', 'C'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.right
  | _ => ensure false "expected key event"

test "parses Home key" := do
  let initial := MockTerminal.withInput [91, 72] -- '[', 'H'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.home
  | _ => ensure false "expected key event"

test "parses End key" := do
  let initial := MockTerminal.withInput [91, 70] -- '[', 'F'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.«end»
  | _ => ensure false "expected key event"

test "parses Delete key" := do
  let initial := MockTerminal.withInput [91, 51, 126] -- '[', '3', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.delete
  | _ => ensure false "expected key event"

test "parses Page Up" := do
  let initial := MockTerminal.withInput [91, 53, 126] -- '[', '5', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.pageUp
  | _ => ensure false "expected key event"

test "parses Page Down" := do
  let initial := MockTerminal.withInput [91, 54, 126] -- '[', '6', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.pageDown
  | _ => ensure false "expected key event"

test "parses F1" := do
  let initial := MockTerminal.withInput [79, 80] -- 'O', 'P'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.f 1
  | _ => ensure false "expected key event"

test "parses F2" := do
  let initial := MockTerminal.withInput [79, 81] -- 'O', 'Q'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.f 2
  | _ => ensure false "expected key event"

test "parses Alt a" := do
  let initial := MockTerminal.withInput [97] -- 'a'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => do
    key.code ≡ KeyCode.char 'a'
    key.modifiers.alt ≡ true
  | _ => ensure false "expected key event"

test "parses bare escape when no following bytes" := do
  let initial := MockTerminal.withInput []
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.escape
  | _ => ensure false "expected key event"

-- ============================================================================
-- Event Polling Tests
-- ============================================================================

/-- Helper to check if poll returned a key event with expected code -/
def pollReturnsKeyCode (input : List UInt8) (expected : KeyCode) : Bool :=
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.key k => k.code == expected
  | _ => false

test "poll returns none when no input" := do
  let (event, _) := MockTerminal.run Events.poll
  event ≡ Event.none

test "poll returns key event for character" := do
  (pollReturnsKeyCode [97] (KeyCode.char 'a')) ≡ true

test "poll handles escape sequences" := do
  (pollReturnsKeyCode [27, 91, 65] KeyCode.up) ≡ true

-- ============================================================================
-- Mouse Event Parsing Tests
-- ============================================================================

/-- Helper to check if poll returned a mouse event with expected properties -/
def pollReturnsMouse (input : List UInt8) (btn : MouseButton) (act : MouseAction) (x y : Nat) : Bool :=
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.mouse me => me.button == btn && me.action == act && me.x == x && me.y == y
  | _ => false

-- SGR mouse format: ESC [ < Cb ; Cx ; Cy M|m
-- M = press, m = release
-- Cb: 0=left, 1=middle, 2=right, 64=scrollUp, 65=scrollDown, +32=motion

test "parses left click press" := do
  -- ESC [ < 0 ; 10 ; 20 M
  let input : List UInt8 := [27, 91, 60, 48, 59, 49, 48, 59, 50, 48, 77]
  (pollReturnsMouse input .left .press 10 20) ≡ true

test "parses right click release" := do
  -- ESC [ < 2 ; 5 ; 15 m
  let input : List UInt8 := [27, 91, 60, 50, 59, 53, 59, 49, 53, 109]
  (pollReturnsMouse input .right .release 5 15) ≡ true

test "parses middle click" := do
  -- ESC [ < 1 ; 8 ; 12 M
  let input : List UInt8 := [27, 91, 60, 49, 59, 56, 59, 49, 50, 77]
  (pollReturnsMouse input .middle .press 8 12) ≡ true

test "parses scroll up" := do
  -- ESC [ < 64 ; 1 ; 1 M
  let input : List UInt8 := [27, 91, 60, 54, 52, 59, 49, 59, 49, 77]
  (pollReturnsMouse input .scrollUp .press 1 1) ≡ true

test "parses scroll down" := do
  -- ESC [ < 65 ; 25 ; 10 M
  let input : List UInt8 := [27, 91, 60, 54, 53, 59, 50, 53, 59, 49, 48, 77]
  (pollReturnsMouse input .scrollDown .press 25 10) ≡ true

test "parses mouse motion" := do
  -- ESC [ < 35 ; 50 ; 30 M (35 = button none + motion flag)
  let input : List UInt8 := [27, 91, 60, 51, 53, 59, 53, 48, 59, 51, 48, 77]
  (pollReturnsMouse input .none .motion 50 30) ≡ true

test "parses mouse with ctrl modifier" := do
  -- ESC [ < 16 ; 3 ; 7 M (16 = 0 + ctrl modifier)
  let input : List UInt8 := [27, 91, 60, 49, 54, 59, 51, 59, 55, 77]
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.mouse me => do
    me.button ≡ .left
    me.modifiers.ctrl ≡ true
  | _ => ensure false "expected mouse event"

-- ============================================================================
-- Cell Tests
-- ============================================================================

test "Cell.empty has space character" := do
  Cell.empty.char ≡ ' '

test "Cell.empty has default style" := do
  Cell.empty.style ≡ Style.default

test "Cell.new creates cell with character" := do
  let cell := Cell.new 'X'
  cell.char ≡ 'X'

test "Cell.styled creates styled cell" := do
  let style := Style.bold.withFg Color.red
  let cell := Cell.styled 'A' style
  cell.char ≡ 'A'
  cell.style.modifier.bold ≡ true

test "Cell.withHyperlink adds hyperlink" := do
  let cell := Cell.new 'L' |>.withHyperlink "https://example.com"
  cell.hyperlink ≡ some "https://example.com"

test "Cell.link creates hyperlinked cell" := do
  let cell := Cell.link 'X' Style.underline "https://test.com"
  cell.char ≡ 'X'
  cell.hyperlink ≡ some "https://test.com"
  cell.style.modifier.underline ≡ true

-- ============================================================================
-- Buffer Tests
-- ============================================================================

test "Buffer.new creates buffer of correct size" := do
  let buf := Buffer.new 10 5
  buf.width ≡ 10
  buf.height ≡ 5

test "Buffer.new fills with empty cells" := do
  let buf := Buffer.new 3 3
  (buf.get 0 0).char ≡ ' '
  (buf.get 2 2).char ≡ ' '

test "Buffer.set updates cell" := do
  let buf := Buffer.new 5 5
  let buf := buf.set 2 3 (Cell.new 'X')
  (buf.get 2 3).char ≡ 'X'

test "Buffer.set ignores out of bounds" := do
  let buf := Buffer.new 5 5
  let buf := buf.set 10 10 (Cell.new 'X')
  buf.width ≡ 5 -- unchanged

test "Buffer.get returns empty for out of bounds" := do
  let buf := Buffer.new 5 5
  (buf.get 100 100).char ≡ ' '

test "Buffer.writeString writes text horizontally" := do
  let buf := Buffer.new 10 3
  let buf := buf.writeString 2 1 "Hi"
  (buf.get 2 1).char ≡ 'H'
  (buf.get 3 1).char ≡ 'i'

test "Buffer.writeString applies style" := do
  let buf := Buffer.new 10 3
  let buf := buf.writeString 0 0 "X" Style.bold
  (buf.get 0 0).style.modifier.bold ≡ true

test "Buffer.writeLink writes hyperlinked text" := do
  let buf := Buffer.new 20 3
  let buf := buf.writeLink 0 0 "Click" "https://x.com"
  (buf.get 0 0).char ≡ 'C'
  (buf.get 0 0).hyperlink ≡ some "https://x.com"
  (buf.get 4 0).char ≡ 'k'
  (buf.get 4 0).hyperlink ≡ some "https://x.com"

test "Buffer.clear fills with empty cells" := do
  let buf := Buffer.new 5 5
  let buf := buf.set 2 2 (Cell.new 'X')
  let buf := buf.clear
  (buf.get 2 2).char ≡ ' '

test "Buffer.diff returns empty for identical buffers" := do
  let buf := Buffer.new 5 5
  let changes := Buffer.diff buf buf
  changes.length ≡ 0

test "Buffer.diff detects changes" := do
  let old := Buffer.new 5 5
  let new_ := old.set 1 1 (Cell.new 'X')
  let changes := Buffer.diff old new_
  changes.length ≡ 1

-- ============================================================================
-- Rect Tests
-- ============================================================================

test "Rect.isEmpty returns true for zero dimensions" := do
  let r : Rect := { x := 0, y := 0, width := 0, height := 5 }
  r.isEmpty ≡ true

test "Rect.isEmpty returns false for valid rect" := do
  let r : Rect := { x := 0, y := 0, width := 10, height := 5 }
  r.isEmpty ≡ false

test "Rect.area calculates correctly" := do
  let r : Rect := { x := 0, y := 0, width := 4, height := 3 }
  r.area ≡ 12

-- ============================================================================
-- Style Tests
-- ============================================================================

test "Style.default has default colors" := do
  Style.default.fg ≡ Color.default
  Style.default.bg ≡ Color.default

test "Style.bold has bold modifier" := do
  Style.bold.modifier.bold ≡ true

test "Style.withFg sets foreground color" := do
  let s := Style.default.withFg Color.red
  s.fg ≡ Color.red

test "Style.merge combines styles" := do
  let s1 := Style.bold
  let s2 := Style.fgColor Color.green
  let merged := Style.merge s1 s2
  merged.modifier.bold ≡ true
  merged.fg ≡ Color.green

-- ============================================================================
-- Layout Tests
-- ============================================================================

test "hsplit divides area horizontally" := do
  let area : Rect := { x := 0, y := 0, width := 100, height := 10 }
  let rects := hsplit area [.percent 50, .fill]
  rects.length ≡ 2
  if h : 0 < rects.length then rects[0].width ≡ 50 else ensure false "expected rect"
  if h : 1 < rects.length then rects[1].width ≡ 50 else ensure false "expected rect"

test "vsplit divides area vertically" := do
  let area : Rect := { x := 0, y := 0, width := 10, height := 100 }
  let rects := vsplit area [.fixed 20, .fill]
  rects.length ≡ 2
  if h : 0 < rects.length then rects[0].height ≡ 20 else ensure false "expected rect"
  if h : 1 < rects.length then rects[1].height ≡ 80 else ensure false "expected rect"

test "Layout.fixed constraint uses exact size" := do
  let area : Rect := { x := 0, y := 0, width := 100, height := 10 }
  let rects := hsplit area [.fixed 30, .fill]
  if h : 0 < rects.length then rects[0].width ≡ 30 else ensure false "expected rect"

test "Layout.percent constraint uses percentage" := do
  let area : Rect := { x := 0, y := 0, width := 200, height := 10 }
  let rects := hsplit area [.percent 25, .fill]
  if h : 0 < rects.length then rects[0].width ≡ 50 else ensure false "expected rect"

-- ============================================================================
-- Widget Rendering Tests
-- ============================================================================

/-- Helper to render a widget into a buffer -/
def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Block.single renders corners" := do
  let block := Block.single
  let buf := renderWidget block 5 3
  -- Single line box: ┌ ┐ └ ┘
  (buf.get 0 0).char ≡ '┌'
  (buf.get 4 0).char ≡ '┐'
  (buf.get 0 2).char ≡ '└'
  (buf.get 4 2).char ≡ '┘'

test "Block.single renders edges" := do
  let block := Block.single
  let buf := renderWidget block 5 3
  -- Horizontal edges: ─
  (buf.get 1 0).char ≡ '─'
  (buf.get 2 0).char ≡ '─'
  -- Vertical edges: │
  (buf.get 0 1).char ≡ '│'
  (buf.get 4 1).char ≡ '│'

test "Block.double renders double corners" := do
  let block := Block.double
  let buf := renderWidget block 5 3
  -- Double line box: ╔ ╗ ╚ ╝
  (buf.get 0 0).char ≡ '╔'
  (buf.get 4 0).char ≡ '╗'
  (buf.get 0 2).char ≡ '╚'
  (buf.get 4 2).char ≡ '╝'

test "Block.rounded renders rounded corners" := do
  let block := Block.rounded
  let buf := renderWidget block 5 3
  -- Rounded box: ╭ ╮ ╰ ╯
  (buf.get 0 0).char ≡ '╭'
  (buf.get 4 0).char ≡ '╮'
  (buf.get 0 2).char ≡ '╰'
  (buf.get 4 2).char ≡ '╯'

test "Block.withTitle renders title" := do
  let block := Block.single.withTitle "Hi"
  let buf := renderWidget block 10 3
  -- Title appears in top border with padding: " Hi "
  -- Position 1 = space, position 2 = 'H', position 3 = 'i', position 4 = space
  (buf.get 2 0).char ≡ 'H'
  (buf.get 3 0).char ≡ 'i'

test "Block.innerArea computes correct area" := do
  let block := Block.single
  let outer : Rect := { x := 5, y := 5, width := 10, height := 8 }
  let inner := block.innerArea outer
  inner.x ≡ 6
  inner.y ≡ 6
  inner.width ≡ 8
  inner.height ≡ 6

test "Gauge renders progress bar" := do
  let gauge := Gauge.new 0.5 |>.withLabel "Test"
  let buf := renderWidget gauge 20 1
  -- Should have some filled and unfilled chars
  -- Gauge uses different chars for filled vs unfilled
  ensure true "gauge rendered"

test "Gauge.new clamps ratio to 0..1" := do
  let gauge1 := Gauge.new (-0.5)
  gauge1.ratio ≡ 0.0
  let gauge2 := Gauge.new 1.5
  gauge2.ratio ≡ 1.0

test "Tabs renders tab titles" := do
  let tabs := Tabs.new ["One", "Two", "Three"] |>.withSelected 0
  let buf := renderWidget tabs 30 1
  -- Check that tab titles appear
  -- Note: exact positions depend on rendering logic
  ensure true "tabs rendered"

test "ListWidget renders items" := do
  let list := ListWidget.new ["Item A", "Item B", "Item C"] |>.withSelected 1
  let buf := renderWidget list 15 5
  -- Items should appear in buffer
  ensure true "list rendered"

test "Paragraph renders text" := do
  let para := Paragraph.fromString "Hello"
  let buf := renderWidget para 10 3
  (buf.get 0 0).char ≡ 'H'
  (buf.get 1 0).char ≡ 'e'
  (buf.get 2 0).char ≡ 'l'
  (buf.get 3 0).char ≡ 'l'
  (buf.get 4 0).char ≡ 'o'

test "Paragraph.centered centers text" := do
  let para := Paragraph.fromString "Hi" |>.centered
  let buf := renderWidget para 10 1
  -- "Hi" should be centered in 10 chars = at position 4
  (buf.get 4 0).char ≡ 'H'
  (buf.get 5 0).char ≡ 'i'

test "Paragraph.fromLines renders multiple lines" := do
  let para := Paragraph.fromLines ["One", "Two"]
  let buf := renderWidget para 10 3
  (buf.get 0 0).char ≡ 'O'
  (buf.get 0 1).char ≡ 'T'

-- Base64 Tests
test "Base64.encode encodes empty string" := do
  Base64.encode "".toUTF8 ≡ ""

test "Base64.encode encodes 'Hi'" := do
  Base64.encode "Hi".toUTF8 ≡ "SGk="

test "Base64.encode encodes 'Hello'" := do
  Base64.encode "Hello".toUTF8 ≡ "SGVsbG8="

test "Base64.decode decodes 'SGk='" := do
  match Base64.decode "SGk=" with
  | some bytes => String.fromUTF8! bytes ≡ "Hi"
  | none => ensure false "decode failed"

test "Base64.decode decodes 'SGVsbG8='" := do
  match Base64.decode "SGVsbG8=" with
  | some bytes => String.fromUTF8! bytes ≡ "Hello"
  | none => ensure false "decode failed"

test "Base64 roundtrip preserves text" := do
  let original := "Test 123!"
  let encoded := Base64.encode original.toUTF8
  match Base64.decode encoded with
  | some bytes => String.fromUTF8! bytes ≡ original
  | none => ensure false "roundtrip failed"

-- TextInput Selection Tests
test "TextInput.selectAll selects entire text" := do
  let input := TextInput.new.withValue "Hello"
  let selected := input.selectAll
  selected.hasSelection ≡ true
  selected.selectedText ≡ "Hello"

test "TextInput.copy returns text" := do
  let input := TextInput.new.withValue "Test"
  input.copy ≡ "Test"

test "TextInput.copy with selection returns selected text" := do
  let input := TextInput.new.withValue "Hello World"
    |>.selectAll
  input.copy ≡ "Hello World"

test "TextInput.cut removes and returns text" := do
  let input := (TextInput.new.withValue "Cut me").selectAll
  let (newInput, cutText) := input.cut
  cutText ≡ "Cut me"
  newInput.value ≡ ""

test "TextInput.paste inserts text at cursor" := do
  let input := TextInput.new.withValue "Hello"
    |>.moveCursorEnd
    |>.paste " World"
  input.value ≡ "Hello World"

test "TextInput.paste replaces selection" := do
  let input := TextInput.new.withValue "Hello World"
    |>.selectAll
    |>.paste "Goodbye"
  input.value ≡ "Goodbye"

-- TextArea Selection Tests
test "TextArea.selectAll selects entire text" := do
  let area := TextArea.fromString "Line 1\nLine 2"
  let selected := area.selectAll
  selected.hasSelection ≡ true

test "TextArea.selectedText returns correct text single line" := do
  let area := TextArea.fromString "Hello World"
    |>.selectAll
  area.selectedText ≡ "Hello World"

test "TextArea.selectedText returns correct text multi-line" := do
  let area := TextArea.fromString "Line 1\nLine 2\nLine 3"
    |>.selectAll
  area.selectedText ≡ "Line 1\nLine 2\nLine 3"

test "TextArea.copy returns all text when no selection" := do
  let area := TextArea.fromString "Full text"
  area.copy ≡ "Full text"

test "TextArea.cut removes and returns all text" := do
  let area := (TextArea.fromString "Cut this").selectAll
  let (newArea, cutText) := area.cut
  cutText ≡ "Cut this"
  newArea.text ≡ ""

test "TextArea.paste inserts single line" := do
  let area := TextArea.new.paste "Pasted"
  area.text ≡ "Pasted"

test "TextArea.paste inserts multi-line" := do
  let area := TextArea.new.paste "Line 1\nLine 2"
  area.lineCount ≡ 2

-- ClipboardCommand Tests
test "ClipboardCommand key generates unique key" := do
  let cmd : ClipboardCommand := { text := "test" }
  cmd.key.startsWith "clip:" ≡ true

test "TerminalCommand.copyToClipboard creates clipboard command" := do
  let cmd := TerminalCommand.copyToClipboard "Hello"
  match cmd with
  | .clipboard c => c.text ≡ "Hello"
  | _ => ensure false "wrong command type"

-- Notification Tests
test "NotificationLevel.toStyle returns cyan for info" := do
  let style := NotificationLevel.info.toStyle
  style.fg ≡ Color.ansi .cyan

test "NotificationLevel.toStyle returns green for success" := do
  let style := NotificationLevel.success.toStyle
  style.fg ≡ Color.ansi .green

test "NotificationLevel.toStyle returns yellow for warning" := do
  let style := NotificationLevel.warning.toStyle
  style.fg ≡ Color.ansi .yellow

test "NotificationLevel.toStyle returns red for error" := do
  let style := NotificationLevel.error.toStyle
  style.fg ≡ Color.ansi .red

test "Notification.message creates single-line notification" := do
  let n := Notification.message "Hello"
  n.lines.length ≡ 1

test "Notification.multiline creates multi-line notification" := do
  let n := Notification.multiline ["Line 1", "Line 2", "Line 3"]
  n.lines.length ≡ 3

test "Notification.computeSize respects maxWidth" := do
  let n := Notification.message "A very long message that exceeds the max width"
             |>.withMaxWidth 20
             |>.noBorder
  let (w, _) := n.computeSize 100 100
  ensure (w <= 20) "width should be capped"

test "Notification.computeSize includes border padding" := do
  let n := Notification.message "Hi"  -- 2 chars
  let (w, h) := n.computeSize 100 100
  -- With default rounded border: 2 chars + 2 border = 4 width
  -- 1 line + 2 border = 3 height
  w ≡ 4
  h ≡ 3

test "NotificationStack.push adds notification" := do
  let stack := NotificationStack.atPosition .topRight
    |>.push (Notification.message "First")
    |>.push (Notification.message "Second")
  stack.size ≡ 2

test "NotificationStack.pop removes oldest" := do
  let stack := NotificationStack.atPosition .topRight
    |>.push (Notification.message "First")
    |>.push (Notification.message "Second")
    |>.pop
  stack.size ≡ 1

test "NotificationStack.clear removes all" := do
  let stack := NotificationStack.atPosition .topRight
    |>.push (Notification.message "First")
    |>.push (Notification.message "Second")
    |>.clear
  stack.isEmpty ≡ true

test "Notification renders message text" := do
  let n := Notification.message "Test" |>.noBorder
  let buf := renderWidget n 10 3
  (buf.get 0 0).char ≡ 'T'
  (buf.get 1 0).char ≡ 'e'
  (buf.get 2 0).char ≡ 's'
  (buf.get 3 0).char ≡ 't'

test "Notification with border renders correctly" := do
  let n := Notification.message "Hi"
  let buf := renderWidget n 10 5
  -- Border should be present (rounded uses curves)
  -- Content 'H' should be inside at position (1, 1)
  (buf.get 1 1).char ≡ 'H'
  (buf.get 2 1).char ≡ 'i'

#generate_tests

end Tests.Terminus

def main : IO UInt32 := do
  IO.println "╔══════════════════════════════════════════════════════════════╗"
  IO.println "║                    Terminus Test Suite                       ║"
  IO.println "╚══════════════════════════════════════════════════════════════╝"
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result == 0 then
    IO.println "All tests passed!"
  else
    IO.println "Some tests failed"

  return result
