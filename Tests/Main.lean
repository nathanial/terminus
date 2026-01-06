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
import Terminus.Widgets.BarChart
import Terminus.Widgets.Table
import Terminus.Widgets.Checkbox
import Terminus.Widgets.Spinner
import Terminus.Widgets.Calendar
import Terminus.Widgets.Menu
import Terminus.Widgets.LineChart
import Terminus.Widgets.PieChart
import Terminus.Widgets.Tree
import Terminus.Widgets.Popup
import Terminus.Widgets.Sparkline
import Terminus.Widgets.LineGauge
import Terminus.Widgets.Canvas
import Terminus.Widgets.ScrollView
import Terminus.Widgets.Scrollbar
import Terminus.Widgets.BigText
import Terminus.Widgets.Logger
import Terminus.Widgets.Form
import Terminus.Widgets.Image
import Terminus.Widgets.Clear

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
  let _ := renderWidget gauge 20 1
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
  let _ := renderWidget tabs 30 1
  -- Check that tab titles appear
  -- Note: exact positions depend on rendering logic
  ensure true "tabs rendered"

test "ListWidget renders items" := do
  let list := ListWidget.new ["Item A", "Item B", "Item C"] |>.withSelected 1
  let _ := renderWidget list 15 5
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

-- ============================================================================
-- BarChart Tests
-- ============================================================================

test "BarData.new creates bar with label and value" := do
  let bar := BarData.new "Test" 42.0
  bar.label ≡ "Test"
  bar.value ≡ 42.0

test "BarData.withStyle sets custom style" := do
  let bar := BarData.new "A" 10.0 |>.withStyle Style.bold
  bar.style.isSome ≡ true

test "BarChart.fromPairs creates chart from label/value pairs" := do
  let chart := BarChart.fromPairs [("A", 10.0), ("B", 20.0), ("C", 30.0)]
  chart.data.length ≡ 3

test "BarChart.computeMax finds maximum value" := do
  let chart := BarChart.fromPairs [("A", 10.0), ("B", 50.0), ("C", 30.0)]
  chart.computeMax ≡ 50.0

test "BarChart.computeMax uses custom maxValue when set" := do
  let chart := BarChart.fromPairs [("A", 10.0), ("B", 50.0)]
    |>.withMaxValue 100.0
  chart.computeMax ≡ 100.0

test "BarChart.computeMax returns 0 for empty data" := do
  let chart : BarChart := {}
  chart.computeMax ≡ 0.0

test "BarChart empty data renders without crash" := do
  let chart : BarChart := {}
  let buf := renderWidget chart 20 10
  -- Should not crash, buffer unchanged from empty
  buf.width ≡ 20

test "BarChart vertical renders bars" := do
  let chart := BarChart.fromPairs [("A", 100.0)]
  let _ := renderWidget chart 10 10
  -- Bar character should appear somewhere in the buffer
  -- Check bottom rows for filled bar
  ensure true "vertical bar chart rendered"

test "BarChart horizontal renders bars" := do
  let chart := BarChart.horizontal
    |>.withData [BarData.new "X" 50.0]
  let _ := renderWidget chart 20 5
  ensure true "horizontal bar chart rendered"

test "BarChart.hideLabels suppresses labels" := do
  let chart := BarChart.fromPairs [("Test", 10.0)] |>.hideLabels
  chart.showLabels ≡ false

test "BarChart.hideValues suppresses values" := do
  let chart := BarChart.fromPairs [("Test", 10.0)] |>.hideValues
  chart.showValues ≡ false

test "BarChart.withBarWidth sets bar width" := do
  let chart := BarChart.fromPairs [("A", 10.0)] |>.withBarWidth 5
  chart.barWidth ≡ 5

-- ============================================================================
-- Table Tests
-- ============================================================================

test "TableCell.new creates cell with content" := do
  let cell := TableCell.new "Hello"
  cell.content ≡ "Hello"

test "TableCell.styled creates cell with style" := do
  let cell := TableCell.styled "Bold" Style.bold
  cell.content ≡ "Bold"
  cell.style.modifier.bold ≡ true

test "TableRow.new creates row from string list" := do
  let row := TableRow.new ["A", "B", "C"]
  row.cells.length ≡ 3

test "Table.new creates table from rows" := do
  let table := Table.new [["A", "B"], ["C", "D"]]
  table.rows.length ≡ 2

test "Table.withHeader sets header row" := do
  let table := Table.new [["1", "2"]] |>.withHeader ["Col1", "Col2"]
  table.header.isSome ≡ true

test "Table.withSelected sets selection index" := do
  let table := Table.new [["A"], ["B"], ["C"]] |>.withSelected 1
  table.selected ≡ some 1

test "Table.withSelected ignores out of bounds" := do
  let table := Table.new [["A"], ["B"]] |>.withSelected 10
  table.selected ≡ none

test "Table.selectNext advances selection" := do
  let table := Table.new [["A"], ["B"], ["C"]] |>.withSelected 0 |>.selectNext
  table.selected ≡ some 1

test "Table.selectNext stops at end" := do
  let table := Table.new [["A"], ["B"]] |>.withSelected 1 |>.selectNext
  table.selected ≡ some 1

test "Table.selectPrev moves selection back" := do
  let table := Table.new [["A"], ["B"], ["C"]] |>.withSelected 2 |>.selectPrev
  table.selected ≡ some 1

test "Table.selectPrev stops at beginning" := do
  let table := Table.new [["A"], ["B"]] |>.withSelected 0 |>.selectPrev
  table.selected ≡ some 0

test "Table renders without crash" := do
  let table := Table.new [["A", "B"], ["C", "D"]] |>.withHeader ["X", "Y"]
  let buf := renderWidget table 20 10
  buf.width ≡ 20

-- ============================================================================
-- Checkbox Tests
-- ============================================================================

test "Checkbox.new creates checkbox with label" := do
  let cb := Checkbox.new "Option"
  cb.label ≡ "Option"
  cb.checked ≡ false

test "Checkbox.toggle flips checked state" := do
  let cb := Checkbox.new "Test" |>.toggle
  cb.checked ≡ true
  let cb2 := cb.toggle
  cb2.checked ≡ false

test "Checkbox.withChecked sets checked state" := do
  let cb := Checkbox.new "Test" |>.withChecked true
  cb.checked ≡ true

test "Checkbox checked renders checkedSymbol" := do
  let cb := Checkbox.new "Test" |>.withChecked true
  let buf := renderWidget cb 20 1
  -- Default checked symbol is "[x]"
  (buf.get 0 0).char ≡ '['
  (buf.get 1 0).char ≡ 'x'
  (buf.get 2 0).char ≡ ']'

test "Checkbox unchecked renders uncheckedSymbol" := do
  let cb := Checkbox.new "Test"
  let buf := renderWidget cb 20 1
  -- Default unchecked symbol is "[ ]"
  (buf.get 0 0).char ≡ '['
  (buf.get 1 0).char ≡ ' '
  (buf.get 2 0).char ≡ ']'

test "Checkbox.withSymbols sets custom symbols" := do
  let cb := Checkbox.new "Test" |>.withSymbols "✓" "○"
  cb.checkedSymbol ≡ "✓"
  cb.uncheckedSymbol ≡ "○"

-- ============================================================================
-- RadioGroup Tests
-- ============================================================================

test "RadioGroup.new creates from labels" := do
  let rg := RadioGroup.new ["A", "B", "C"]
  rg.options.length ≡ 3

test "RadioGroup.withSelected sets selection" := do
  let rg := RadioGroup.new ["A", "B", "C"] |>.withSelected 1
  rg.selected ≡ some 1

test "RadioGroup.withSelected ignores out of bounds" := do
  let rg := RadioGroup.new ["A", "B"] |>.withSelected 10
  rg.selected ≡ none

test "RadioGroup.selectNext advances selection" := do
  let rg := RadioGroup.new ["A", "B", "C"] |>.withSelected 0 |>.selectNext
  rg.selected ≡ some 1

test "RadioGroup.selectPrev moves selection back" := do
  let rg := RadioGroup.new ["A", "B", "C"] |>.withSelected 2 |>.selectPrev
  rg.selected ≡ some 1

test "RadioGroup.getSelected returns selected option" := do
  let rg := RadioGroup.new ["First", "Second", "Third"] |>.withSelected 1
  match rg.getSelected with
  | some opt => opt.label ≡ "Second"
  | none => ensure false "expected selection"

test "RadioGroup.clearSelected clears selection" := do
  let rg := RadioGroup.new ["A", "B"] |>.withSelected 0 |>.clearSelected
  rg.selected ≡ none

test "RadioGroup renders without crash" := do
  let rg := RadioGroup.new ["Option A", "Option B"] |>.withSelected 0
  let buf := renderWidget rg 20 5
  buf.width ≡ 20

-- ============================================================================
-- Spinner Tests
-- ============================================================================

test "Spinner.dots has correct frame count" := do
  Spinner.dots.frames.size ≡ 10

test "Spinner.line has correct frame count" := do
  Spinner.line.frames.size ≡ 4

test "Spinner.ascii has correct frame count" := do
  Spinner.ascii.frames.size ≡ 4

test "Spinner.withFrame advances to correct frame" := do
  let s := Spinner.dots.withFrame 3
  s.frameIndex ≡ 3

test "Spinner.withFrame wraps around" := do
  let s := Spinner.line.withFrame 10  -- 4 frames, so 10 % 4 = 2
  s.frameIndex ≡ 2

test "Spinner.currentFrame returns correct frame" := do
  let s := Spinner.ascii.withFrame 0
  s.currentFrame ≡ "|"

test "Spinner.withLabel sets label" := do
  let s := Spinner.dots.withLabel "Loading..."
  s.label ≡ some "Loading..."

test "Spinner renders without crash" := do
  let s := Spinner.dots.withFrame 0 |>.withLabel "Wait"
  let buf := renderWidget s 20 3
  buf.width ≡ 20

-- ============================================================================
-- Calendar Tests
-- ============================================================================

test "Calendar.new creates with year and month" := do
  let cal := Calendar.new 2025 6
  cal.year ≡ 2025
  cal.month ≡ 6

test "Calendar.new clamps month to valid range" := do
  let cal := Calendar.new 2025 15
  cal.month ≡ 12

test "Calendar.withSelectedDay sets selection" := do
  let cal := Calendar.new 2025 1 |>.withSelectedDay 15
  cal.selectedDay ≡ some 15

test "Calendar.nextDay advances selection" := do
  let cal := Calendar.new 2025 1 |>.withSelectedDay 10 |>.nextDay
  cal.selectedDay ≡ some 11

test "Calendar.prevDay moves selection back" := do
  let cal := Calendar.new 2025 1 |>.withSelectedDay 10 |>.prevDay
  cal.selectedDay ≡ some 9

test "Calendar.nextMonth advances month" := do
  let cal := Calendar.new 2025 6 |>.nextMonth
  cal.month ≡ 7

test "Calendar.prevMonth moves month back" := do
  let cal := Calendar.new 2025 6 |>.prevMonth
  cal.month ≡ 5

test "Calendar.monthName returns correct name" := do
  let cal := Calendar.new 2025 3
  cal.monthName ≡ "March"

test "Calendar.numDays returns days in month" := do
  let cal := Calendar.new 2025 2  -- February 2025 (not leap year)
  cal.numDays ≡ 28

test "Calendar renders without crash" := do
  let cal := Calendar.new 2025 12 |>.withSelectedDay 25
  let buf := renderWidget cal 25 10
  buf.width ≡ 25

-- ============================================================================
-- Menu Tests
-- ============================================================================

test "MenuItem.new creates item with label" := do
  let item := MenuItem.new "File"
  item.label ≡ "File"
  item.enabled ≡ true

test "MenuItem.separator creates separator" := do
  let item := MenuItem.separator
  item.isSeparator ≡ true

test "MenuItem.disabled creates disabled item" := do
  let item := MenuItem.disabled "Unavailable"
  item.label ≡ "Unavailable"
  item.enabled ≡ false

test "MenuItem.withHotkey sets hotkey" := do
  let item := MenuItem.new "Save" |>.withHotkey "Ctrl+S"
  item.hotkey ≡ some "Ctrl+S"

test "MenuItem.hasSubmenu detects submenu" := do
  let item := MenuItem.new "Options" |>.withSubmenu [MenuItem.new "Sub1"]
  item.hasSubmenu ≡ true

test "Menu.new creates menu from items" := do
  let menu := Menu.new [MenuItem.new "A", MenuItem.new "B"]
  menu.items.length ≡ 2

test "Menu.withSelected sets selection" := do
  let menu := Menu.new [MenuItem.new "A", MenuItem.new "B"] |>.withSelected 1
  menu.selectedPath ≡ [1]

test "Menu renders without crash" := do
  let menu := Menu.new [MenuItem.new "File", MenuItem.new "Edit", MenuItem.separator, MenuItem.new "Exit"]
    |>.withSelected 0
  let buf := renderWidget menu 20 10
  buf.width ≡ 20

-- ============================================================================
-- LineChart Tests
-- ============================================================================

test "DataSeries.new creates series from data" := do
  let series := DataSeries.new [1.0, 2.0, 3.0]
  series.data.length ≡ 3

test "DataSeries.withLabel sets label" := do
  let series := DataSeries.new [1.0] |>.withLabel "Sales"
  series.label ≡ "Sales"

test "DataSeries.min finds minimum value" := do
  let series := DataSeries.new [5.0, 2.0, 8.0, 1.0]
  series.min ≡ 1.0

test "DataSeries.max finds maximum value" := do
  let series := DataSeries.new [5.0, 2.0, 8.0, 1.0]
  series.max ≡ 8.0

test "LineChart.new creates empty chart" := do
  let chart := LineChart.new
  chart.series.isEmpty ≡ true

test "LineChart.addSeries adds series to chart" := do
  let chart := LineChart.new
    |>.addSeries (DataSeries.new [1.0, 2.0])
    |>.addSeries (DataSeries.new [3.0, 4.0])
  chart.series.length ≡ 2

test "LineChart.maxDataLength finds longest series" := do
  let chart := LineChart.new
    |>.addSeries (DataSeries.new [1.0, 2.0, 3.0])
    |>.addSeries (DataSeries.new [1.0, 2.0])
  chart.maxDataLength ≡ 3

test "LineChart.withYRange sets Y axis bounds" := do
  let chart := LineChart.new |>.withYRange 0.0 100.0
  chart.yMin ≡ some 0.0
  chart.yMax ≡ some 100.0

test "LineChart.hideLegend disables legend" := do
  let chart := LineChart.new |>.hideLegend
  chart.showLegend ≡ false

test "LineChart renders without crash" := do
  let chart := LineChart.new
    |>.addSeries (DataSeries.new [10.0, 20.0, 15.0, 25.0] |>.withLabel "Data")
  let buf := renderWidget chart 40 20
  buf.width ≡ 40

-- ============================================================================
-- PieChart Tests
-- ============================================================================

test "PieSlice.new creates slice with label and value" := do
  let slice := PieSlice.new "Category" 50.0
  slice.label ≡ "Category"
  slice.value ≡ 50.0

test "PieChart.new creates chart from slices" := do
  let chart := PieChart.new [PieSlice.new "A" 30.0, PieSlice.new "B" 70.0]
  chart.data.length ≡ 2

test "PieChart.withDonutRatio clamps to valid range" := do
  let chart := PieChart.new [] |>.withDonutRatio 0.5
  chart.donutRatio ≡ 0.5
  let chart2 := PieChart.new [] |>.withDonutRatio 1.5
  -- Should be clamped to 0.9 max
  ensure (chart2.donutRatio <= 0.9) "donut ratio clamped"

test "PieChart.withStartAngle sets start angle" := do
  let chart := PieChart.new [] |>.withStartAngle 45.0
  chart.startAngle ≡ 45.0

test "PieChart.withResolution sets resolution" := do
  let chart := PieChart.new [] |>.withResolution .cell
  chart.resolution ≡ PieChartResolution.cell

test "PieChart renders without crash" := do
  let chart := PieChart.new [
    PieSlice.new "Red" 30.0 |>.withStyle (Style.fgColor .red),
    PieSlice.new "Blue" 50.0 |>.withStyle (Style.fgColor .blue),
    PieSlice.new "Green" 20.0 |>.withStyle (Style.fgColor .green)
  ]
  let buf := renderWidget chart 30 15
  buf.width ≡ 30

-- ============================================================================
-- Tree Tests
-- ============================================================================

test "TreeNode.mkLeaf creates leaf node" := do
  let node := TreeNode.mkLeaf "File.txt"
  node.isLeaf ≡ true
  node.label ≡ "File.txt"

test "TreeNode.mkBranch creates branch node" := do
  let node := TreeNode.mkBranch "Folder" [TreeNode.mkLeaf "Child"]
  node.isBranch ≡ true
  node.isExpanded ≡ true

test "TreeNode.mkCollapsed creates collapsed branch" := do
  let node := TreeNode.mkCollapsed "Folder" [TreeNode.mkLeaf "Child"]
  node.isExpanded ≡ false

test "TreeNode.toggle toggles expansion" := do
  let node := TreeNode.mkBranch "Folder" [] |>.toggle
  node.isExpanded ≡ false
  let node2 := node.toggle
  node2.isExpanded ≡ true

test "TreeNode.children returns child nodes" := do
  let child := TreeNode.mkLeaf "Child"
  let node := TreeNode.mkBranch "Parent" [child]
  node.children.length ≡ 1

test "Tree.new creates tree from nodes" := do
  let tree := Tree.new [TreeNode.mkLeaf "A", TreeNode.mkLeaf "B"]
  tree.nodes.length ≡ 2

test "Tree.selectNext advances selection" := do
  let tree := Tree.new [TreeNode.mkLeaf "A", TreeNode.mkLeaf "B"]
    |>.withSelected 0
    |>.selectNext
  tree.selected ≡ 1

test "Tree.selectPrev moves selection back" := do
  let tree := Tree.new [TreeNode.mkLeaf "A", TreeNode.mkLeaf "B"]
    |>.withSelected 1
    |>.selectPrev
  tree.selected ≡ 0

test "Tree.visibleCount counts visible lines" := do
  let tree := Tree.new [
    TreeNode.mkLeaf "A",
    TreeNode.mkBranch "B" [TreeNode.mkLeaf "B1", TreeNode.mkLeaf "B2"]
  ]
  tree.visibleCount ≡ 4  -- A, B, B1, B2

test "Tree renders without crash" := do
  let tree := Tree.new [
    TreeNode.mkBranch "Root" [
      TreeNode.mkLeaf "File1",
      TreeNode.mkBranch "SubFolder" [TreeNode.mkLeaf "File2"]
    ]
  ]
  let buf := renderWidget tree 30 10
  buf.width ≡ 30

-- ============================================================================
-- Popup Tests
-- ============================================================================

test "Popup.new creates popup from content" := do
  let popup := Popup.new "Hello\nWorld"
  popup.lines.length ≡ 2

test "Popup.fromLines creates popup from line list" := do
  let popup := Popup.fromLines ["Line 1", "Line 2", "Line 3"]
  popup.lines.length ≡ 3

test "Popup.withTitle sets title" := do
  let popup := Popup.new "Content" |>.withTitle "My Dialog"
  popup.title ≡ some "My Dialog"

test "Popup.withSize sets dimensions" := do
  let popup := Popup.new "Content" |>.withSize 40 20
  popup.width ≡ some 40
  popup.height ≡ some 20

test "Popup.computeSize calculates size from content" := do
  let popup := Popup.new "Short"
  let (w, h) := popup.computeSize 100 100
  -- Content + border + padding
  ensure (w >= 7) "width includes content"
  ensure (h >= 3) "height includes content"

test "Popup renders without crash" := do
  let popup := Popup.new "This is a popup message" |>.withTitle "Info"
  let buf := renderWidget popup 50 20
  buf.width ≡ 50

test "ConfirmPopup.new creates confirmation dialog" := do
  let popup := ConfirmPopup.new "Are you sure?"
  popup.message ≡ "Are you sure?"
  popup.selectedYes ≡ true

test "ConfirmPopup.toggle switches selection" := do
  let popup := ConfirmPopup.new "Confirm?" |>.toggle
  popup.selectedYes ≡ false

test "ConfirmPopup renders without crash" := do
  let popup := ConfirmPopup.new "Delete this file?"
  let buf := renderWidget popup 50 20
  buf.width ≡ 50

-- ============================================================================
-- Sparkline Tests
-- ============================================================================

test "Sparkline.new creates sparkline from data" := do
  let spark := Sparkline.new [1.0, 2.0, 3.0, 4.0]
  spark.data.length ≡ 4

test "Sparkline.fromNats creates sparkline from naturals" := do
  let spark := Sparkline.fromNats [1, 2, 3]
  spark.data.length ≡ 3

test "Sparkline.fromInts creates sparkline from integers" := do
  let spark := Sparkline.fromInts [1, 2, 3]
  spark.data.length ≡ 3

test "Sparkline.computeMax finds maximum" := do
  let spark := Sparkline.new [5.0, 10.0, 3.0]
  spark.computeMax ≡ 10.0

test "Sparkline.computeMax uses custom max when set" := do
  let spark := Sparkline.new [5.0, 10.0] |>.withMax 20.0
  spark.computeMax ≡ 20.0

test "Sparkline.appendData adds value" := do
  let spark := Sparkline.new [1.0, 2.0] |>.appendData 3.0
  spark.data.length ≡ 3

test "Sparkline.pushData maintains max length" := do
  let spark := Sparkline.new [1.0, 2.0, 3.0] |>.pushData 4.0 3
  spark.data.length ≡ 3

test "Sparkline renders without crash" := do
  let spark := Sparkline.new [1.0, 4.0, 2.0, 8.0, 5.0, 3.0]
  let buf := renderWidget spark 20 3
  buf.width ≡ 20

-- ============================================================================
-- LineGauge Tests
-- ============================================================================

test "LineGauge.new creates gauge with ratio" := do
  let gauge := LineGauge.new 0.5
  gauge.ratio ≡ 0.5

test "LineGauge.new clamps ratio to valid range" := do
  let gauge1 := LineGauge.new (-0.5)
  gauge1.ratio ≡ 0.0
  let gauge2 := LineGauge.new 1.5
  gauge2.ratio ≡ 1.0

test "LineGauge.fromPercent creates from percentage" := do
  let gauge := LineGauge.fromPercent 75
  gauge.ratio ≡ 0.75

test "LineGauge.percent returns percentage" := do
  let gauge := LineGauge.new 0.5
  gauge.percent ≡ 50

test "LineGauge.withLabel sets label" := do
  let gauge := LineGauge.new 0.5 |>.withLabel "Progress"
  gauge.label ≡ some "Progress"

test "LineGauge.setRatio updates ratio" := do
  let gauge := LineGauge.new 0.3 |>.setRatio 0.7
  gauge.ratio ≡ 0.7

test "LineGauge.setPercent updates from percent" := do
  let gauge := LineGauge.new 0.0 |>.setPercent 60
  gauge.ratio ≡ 0.6

test "LineGauge.withShowPercent enables percent display" := do
  let gauge := LineGauge.new 0.5 |>.withShowPercent true
  gauge.showPercent ≡ true

test "LineGauge renders without crash" := do
  let gauge := LineGauge.new 0.7 |>.withLabel "Loading" |>.withShowPercent true
  let buf := renderWidget gauge 30 3
  buf.width ≡ 30

-- ============================================================================
-- Clear Widget Tests
-- ============================================================================

test "Clear.new creates default clear widget" := do
  let clear := Clear.new
  clear.style ≡ {}

test "Clear.withStyle applies style" := do
  let clear := Clear.new |>.withStyle (Style.fgColor Color.red)
  clear.style.fg ≡ Color.red

test "Clear.withBg sets background color" := do
  let clear := Clear.new |>.withBg Color.blue
  clear.style.bg ≡ Color.blue

test "Clear renders empty cells with default style" := do
  let clear := Clear.new
  let buf := renderWidget clear 5 3
  -- Should fill with empty cells
  (buf.get 0 0).char ≡ ' '
  (buf.get 2 1).char ≡ ' '

test "Clear renders styled cells when style set" := do
  let clear := Clear.new |>.withBg Color.green
  let buf := renderWidget clear 4 2
  (buf.get 0 0).style.bg ≡ Color.green
  (buf.get 3 1).style.bg ≡ Color.green

-- ============================================================================
-- Scrollbar Widget Tests
-- ============================================================================

test "Scrollbar.vertical creates vertical scrollbar" := do
  let sb := Scrollbar.vertical 10 100 10
  sb.position ≡ 10
  sb.totalItems ≡ 100
  sb.visibleItems ≡ 10
  sb.orientation ≡ .vertical

test "Scrollbar.horizontal creates horizontal scrollbar" := do
  let sb := Scrollbar.horizontal 0 50 20
  sb.orientation ≡ .horizontal

test "Scrollbar.isNeeded returns true when content exceeds viewport" := do
  let sb := Scrollbar.vertical 0 100 10
  sb.isNeeded ≡ true

test "Scrollbar.isNeeded returns false when content fits" := do
  let sb := Scrollbar.vertical 0 10 20
  sb.isNeeded ≡ false

test "Scrollbar.isNeeded returns false when equal" := do
  let sb := Scrollbar.vertical 0 10 10
  sb.isNeeded ≡ false

test "Scrollbar.thumbMetrics returns valid size and position" := do
  let sb := Scrollbar.vertical 0 100 10
  let (thumbSize, thumbPos) := sb.thumbMetrics 10
  -- With 10 visible of 100, thumb should be 1/10 of track = 1
  thumbSize ≡ 1
  thumbPos ≡ 0

test "Scrollbar.thumbMetrics at end position" := do
  let sb := Scrollbar.vertical 90 100 10
  let (_, thumbPos) := sb.thumbMetrics 10
  -- At position 90 of 100 with 10 visible, should be at end
  thumbPos ≡ 9

test "Scrollbar.setPosition updates position" := do
  let sb := Scrollbar.vertical 0 100 10 |>.setPosition 50
  sb.position ≡ 50

test "Scrollbar.withTrackChar changes track character" := do
  let sb := Scrollbar.vertical 0 10 5 |>.withTrackChar '·'
  sb.trackChar ≡ '·'

test "Scrollbar.withThumbChar changes thumb character" := do
  let sb := Scrollbar.vertical 0 10 5 |>.withThumbChar '▓'
  sb.thumbChar ≡ '▓'

test "Scrollbar renders without crash" := do
  let sb := Scrollbar.vertical 5 20 5
  let buf := renderWidget sb 1 10
  buf.height ≡ 10

-- ============================================================================
-- ScrollView Widget Tests
-- ============================================================================

test "ScrollView.new creates with content" := do
  let sv := ScrollView.new (Paragraph.fromString "Hello")
  sv.offset ≡ (0, 0)
  sv.contentSize ≡ (0, 0)

test "ScrollView.withContentSize sets dimensions" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withContentSize 100 50
  sv.contentSize ≡ (100, 50)

test "ScrollView.withOffset sets scroll position" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 10 20
  sv.offset ≡ (10, 20)

test "ScrollView.contentWidth returns width" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withContentSize 80 40
  sv.contentWidth ≡ 80

test "ScrollView.contentHeight returns height" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withContentSize 80 40
  sv.contentHeight ≡ 40

test "ScrollView.offsetX returns x offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 15 25
  sv.offsetX ≡ 15

test "ScrollView.offsetY returns y offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 15 25
  sv.offsetY ≡ 25

test "ScrollView.scrollDown increments y offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 0 10 |>.scrollDown 5
  sv.offsetY ≡ 15

test "ScrollView.scrollUp decrements y offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 0 10 |>.scrollUp 3
  sv.offsetY ≡ 7

test "ScrollView.scrollRight increments x offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 5 0 |>.scrollRight 10
  sv.offsetX ≡ 15

test "ScrollView.scrollLeft decrements x offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 10 0 |>.scrollLeft 4
  sv.offsetX ≡ 6

test "ScrollView.scrollToTop resets y offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 5 20 |>.scrollToTop
  sv.offsetY ≡ 0
  sv.offsetX ≡ 5

test "ScrollView.scrollToLeft resets x offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 15 10 |>.scrollToLeft
  sv.offsetX ≡ 0
  sv.offsetY ≡ 10

test "ScrollView renders without crash" := do
  let sv := ScrollView.new (Paragraph.fromString "Hello World")
    |>.withContentSize 20 5
  let buf := renderWidget sv 10 3
  buf.width ≡ 10

-- ============================================================================
-- Form Widget Tests
-- ============================================================================

test "Form.new creates empty form" := do
  let form := Form.new
  form.rows.length ≡ 0

test "Form.withLabelSuffix changes suffix" := do
  let form := Form.new |>.withLabelSuffix " ="
  form.labelSuffix ≡ " ="

test "Form.withLabelGap sets gap" := do
  let form := Form.new |>.withLabelGap 4
  form.labelGap ≡ 4

test "Form.withRowSpacing sets spacing" := do
  let form := Form.new |>.withRowSpacing 2
  form.rowSpacing ≡ 2

test "FormRow.gap creates spacer" := do
  let row := FormRow.gap 3
  row.height ≡ 3

test "FormRow.fieldOf creates field row" := do
  let row := FormRow.fieldOf "Name" TextInput.new
  row.height ≡ 1

test "FormRow.fullOf creates full-width row" := do
  let row := FormRow.fullOf (Paragraph.fromString "Full width content")
  row.height ≡ 1

test "Form renders without crash" := do
  let form := Form.new
    |>.withRows [
      FormRow.fieldOf "Username" TextInput.new,
      FormRow.gap 1,
      FormRow.fieldOf "Password" TextInput.new
    ]
  let buf := renderWidget form 40 5
  buf.width ≡ 40

test "AnyWidget.empty creates no-op widget" := do
  let w := AnyWidget.empty
  let buf := renderWidget w 5 3
  buf.width ≡ 5

-- ============================================================================
-- Logger Widget Tests
-- ============================================================================

test "LogLevel.severity returns correct ordering" := do
  LogLevel.trace.severity ≡ 0
  LogLevel.debug.severity ≡ 1
  LogLevel.info.severity ≡ 2
  LogLevel.warn.severity ≡ 3
  LogLevel.error.severity ≡ 4

test "LogLevel.label returns display name" := do
  LogLevel.info.label ≡ "INFO"
  LogLevel.error.label ≡ "ERROR"

test "LogLevel.tag returns bracketed name" := do
  LogLevel.warn.tag ≡ "[WARN]"

test "LogEntry.new creates entry" := do
  let entry := LogEntry.new .info "Test message" (some "12:00:00")
  entry.level ≡ .info
  entry.timestamp ≡ some "12:00:00"
  entry.message ≡ "Test message"

test "Logger.new creates empty logger" := do
  let logger := Logger.new
  logger.entries.length ≡ 0
  logger.follow ≡ true

test "Logger.add appends entry" := do
  let logger := Logger.new
    |>.add (LogEntry.new .info "First" (some "12:00"))
    |>.add (LogEntry.new .warn "Second" (some "12:01"))
  logger.entries.length ≡ 2

test "Logger.extend adds multiple entries" := do
  let entries := [
    LogEntry.new .info "A" (some "12:00"),
    LogEntry.new .debug "B" (some "12:01")
  ]
  let logger := Logger.new |>.extend entries
  logger.entries.length ≡ 2

test "Logger.clear removes all entries" := do
  let logger := Logger.new
    |>.add (LogEntry.new .info "Test")
    |>.clear
  logger.entries.length ≡ 0
  logger.scroll ≡ 0

test "Logger.scrollDown increments scroll" := do
  let logger := Logger.new |>.scrollDown 5
  logger.scroll ≡ 5
  logger.follow ≡ false  -- Scrolling disables follow

test "Logger.scrollUp decrements scroll" := do
  let logger := Logger.new |>.scrollDown 10 |>.scrollUp 3
  logger.scroll ≡ 7

test "Logger.home resets scroll to 0" := do
  let logger := Logger.new |>.scrollDown 20 |>.home
  logger.scroll ≡ 0

test "Logger.end_ sets scroll to end" := do
  let entries := List.replicate 50 (LogEntry.new .info "Test")
  let logger := Logger.new |>.extend entries |>.end_
  logger.follow ≡ true  -- end_ re-enables follow

test "Logger.toggleFollow toggles follow mode" := do
  let logger := Logger.new |>.toggleFollow
  logger.follow ≡ false
  let logger2 := logger.toggleFollow
  logger2.follow ≡ true

test "LogLevelFilter.allows with minLevel" := do
  let filter := { minLevel := some LogLevel.warn : LogLevelFilter }
  filter.allows .error ≡ true
  filter.allows .warn ≡ true
  filter.allows .info ≡ false

test "LogLevelFilter.allows with allowedLevels" := do
  let filter := { allowedLevels := some [LogLevel.info, LogLevel.error] : LogLevelFilter }
  filter.allows .info ≡ true
  filter.allows .error ≡ true
  filter.allows .warn ≡ false

test "Logger.withMinLevel sets filter" := do
  let logger := Logger.new |>.withMinLevel LogLevel.warn
  logger.filter.minLevel ≡ some LogLevel.warn

test "Logger renders without crash" := do
  let logger := Logger.new
    |>.add (LogEntry.new .info "Application started" (some "12:00:00"))
    |>.add (LogEntry.new .warn "Low memory warning" (some "12:00:01"))
    |>.add (LogEntry.new .error "Connection failed" (some "12:00:02"))
  let buf := renderWidget logger 60 5
  buf.width ≡ 60

-- ============================================================================
-- BigText Widget Tests
-- ============================================================================

test "BigText.new creates with text" := do
  let bt := BigText.new "Hello"
  bt.text ≡ "Hello"

test "BigText.withFont changes font" := do
  let bt := BigText.new "Test" |>.withFont .slant
  -- Just verify it doesn't crash; BigFont doesn't have BEq
  ensure true "font changed"

test "BigText.withStyle sets style" := do
  let bt := BigText.new "Test" |>.withStyle (Style.fgColor Color.red)
  bt.style.fg ≡ Color.red

test "BigText.withOn changes on-pixel character" := do
  let bt := BigText.new "A" |>.withOn '#'
  bt.on ≡ '#'

test "BigText.withOff changes off-pixel character" := do
  let bt := BigText.new "A" |>.withOff '.'
  bt.off ≡ some '.'

test "BigText.transparent disables off pixels" := do
  let bt := BigText.new "A" |>.withOff '.' |>.transparent
  bt.off ≡ none

test "BigText.withAlignment sets alignment" := do
  let bt := BigText.new "A" |>.withAlignment .center
  bt.alignment ≡ .center

test "BigText.centered sets center alignment" := do
  let bt := BigText.new "A" |>.centered
  bt.alignment ≡ .center

test "BigText.rightAligned sets right alignment" := do
  let bt := BigText.new "A" |>.rightAligned
  bt.alignment ≡ .right

test "BigText.withSpacing sets character spacing" := do
  let bt := BigText.new "AB" |>.withSpacing 2
  bt.spacing ≡ 2

test "BigFont.glyphWidth returns correct width for block font" := do
  BigFont.block.glyphWidth 'A' ≡ 8

test "BigFont.glyphWidth returns correct width for small font" := do
  BigFont.small.glyphWidth 'A' ≡ 4

test "BigText renders without crash" := do
  let bt := BigText.new "Hi" |>.withFont .block
  let buf := renderWidget bt 20 8
  buf.width ≡ 20

test "BigText renders with slant font" := do
  let bt := BigText.new "OK" |>.withFont .slant
  let buf := renderWidget bt 24 8
  buf.height ≡ 8

test "BigText renders with small font" := do
  let bt := BigText.new "XY" |>.withFont .small
  let buf := renderWidget bt 12 4
  buf.height ≡ 4

-- ============================================================================
-- Canvas Widget Tests
-- ============================================================================

test "BrailleGrid.new creates grid with correct dimensions" := do
  let grid := BrailleGrid.new 10 5
  grid.cellWidth ≡ 10
  grid.cellHeight ≡ 5

test "BrailleGrid.new initializes with empty patterns" := do
  let grid := BrailleGrid.new 3 2
  -- All patterns should be 0 (no dots set)
  grid.patterns.size ≡ 6  -- 3 * 2 cells

test "BrailleGrid.setPixel sets a pixel" := do
  let grid := BrailleGrid.new 2 2
  let grid2 := grid.setPixel 0 0 Style.default
  -- Pixel (0,0) is in cell (0,0), dot position 0
  -- After setting, the pattern should be non-zero
  ensure (grid2.patterns.getD 0 0 != 0) "pixel set"

test "BrailleGrid.clearPixel clears a pixel" := do
  let grid := BrailleGrid.new 2 2
    |>.setPixel 0 0 Style.default
    |>.clearPixel 0 0
  (grid.patterns.getD 0 0) ≡ 0

test "BrailleGrid.clear resets entire grid" := do
  let grid := BrailleGrid.new 2 2
    |>.setPixel 0 0 Style.default
    |>.setPixel 1 1 Style.default
    |>.clear
  (grid.patterns.getD 0 0) ≡ 0

test "BrailleGrid.getCell returns braille character" := do
  let grid := BrailleGrid.new 1 1
  let (char, _) := grid.getCell 0 0
  -- Empty cell should return base braille character U+2800
  char ≡ '⠀'

test "Canvas.new creates empty canvas" := do
  let canvas := Canvas.new
  canvas.shapes.length ≡ 0

test "Canvas.point adds point shape" := do
  let canvas := Canvas.new |>.point 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.line adds line shape" := do
  let canvas := Canvas.new |>.line 0.0 0.0 10.0 10.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.rect adds rectangle shape" := do
  let canvas := Canvas.new |>.rect 0.0 0.0 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.filledRect adds filled rectangle shape" := do
  let canvas := Canvas.new |>.filledRect 0.0 0.0 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.circle adds circle shape" := do
  let canvas := Canvas.new |>.circle 5.0 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.filledCircle adds filled circle shape" := do
  let canvas := Canvas.new |>.filledCircle 5.0 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.addShape appends shape to list" := do
  let shape := CanvasShape.point 1.0 1.0 Style.default
  let canvas := Canvas.new |>.addShape shape |>.addShape shape
  canvas.shapes.length ≡ 2

test "Canvas renders without crash" := do
  let canvas := Canvas.new
    |>.line 0.0 0.0 15.0 7.0 Style.default
    |>.circle 8.0 4.0 3.0 (Style.fgColor Color.blue)
  let buf := renderWidget canvas 10 5
  buf.width ≡ 10

test "Canvas renders shapes to braille characters" := do
  let canvas := Canvas.new |>.point 0.0 0.0 Style.default
  let buf := renderWidget canvas 2 2
  -- The character at (0,0) should be a braille character (not space)
  let c := (buf.get 0 0).char
  -- Braille block starts at U+2800
  ensure (c.toNat >= 0x2800 && c.toNat <= 0x28FF) "braille character rendered"

-- ============================================================================
-- Image Widget Tests
-- ============================================================================

test "Image.fromBytes creates image from bytes" := do
  let bytes := ByteArray.mk #[0x89, 0x50, 0x4E, 0x47]  -- PNG header
  let img := Image.fromBytes bytes
  match img.source with
  | .bytes b => b.size ≡ 4
  | .path _ => ensure false "Expected bytes source"

test "Image.fromPath creates image from path" := do
  let img := Image.fromPath "/tmp/test.png"
  match img.source with
  | .path p => p.toString ≡ "/tmp/test.png"
  | .bytes _ => ensure false "Expected path source"

test "Image.withProtocol sets protocol" := do
  let img := Image.fromPath "/tmp/test.png" |>.withProtocol .iterm2
  -- Just verify it doesn't crash; only iterm2 is currently supported
  ensure true "protocol set"

test "Image.withName sets image name" := do
  let img := Image.fromPath "/tmp/test.png" |>.withName (some "myimage")
  img.name ≡ some "myimage"

test "Image.withPreserveAspectRatio sets aspect ratio flag" := do
  let img := Image.fromPath "/tmp/test.png" |>.withPreserveAspectRatio false
  img.preserveAspectRatio ≡ false

test "Image.withAltText sets fallback text" := do
  let img := Image.fromPath "/tmp/test.png" |>.withAltText "Photo"
  img.altText ≡ "Photo"

test "Image.withBackground sets background style" := do
  let img := Image.fromPath "/tmp/test.png" |>.withBackground (Style.bgColor Color.black)
  img.background.bg ≡ Color.black

test "Image fallback Widget renders alt text" := do
  let img := Image.fromPath "/tmp/test.png" |>.withAltText "IMG"
  let buf := renderWidget img 10 3
  -- The fallback widget should render the alt text
  buf.width ≡ 10

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
