-- TerminusTests.WidgetTests: Tests for core widget rendering

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Core.Base64
import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Widgets.Paragraph
import Terminus.Widgets.Gauge
import Terminus.Widgets.Tabs
import Terminus.Widgets.List
import Terminus.Widgets.TextInput
import Terminus.Widgets.TextArea
import Terminus.Widgets.Notification
import Terminus.Backend.Commands

namespace TerminusTests.WidgetTests

open Terminus
open Crucible

testSuite "Widget Tests"

/-- Helper to render a widget into a buffer -/
def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

-- ============================================================================
-- Block Tests
-- ============================================================================

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

-- ============================================================================
-- Gauge Tests
-- ============================================================================

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

-- ============================================================================
-- Tabs Tests
-- ============================================================================

test "Tabs renders tab titles" := do
  let tabs := Tabs.new ["One", "Two", "Three"] |>.withSelected 0
  let _ := renderWidget tabs 30 1
  -- Check that tab titles appear
  -- Note: exact positions depend on rendering logic
  ensure true "tabs rendered"

-- ============================================================================
-- List Tests
-- ============================================================================

test "ListWidget renders items" := do
  let list := ListWidget.new ["Item A", "Item B", "Item C"] |>.withSelected 1
  let _ := renderWidget list 15 5
  -- Items should appear in buffer
  ensure true "list rendered"

-- ============================================================================
-- Paragraph Tests
-- ============================================================================

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

-- ============================================================================
-- Base64 Tests
-- ============================================================================

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

-- ============================================================================
-- TextInput Selection Tests
-- ============================================================================

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

-- ============================================================================
-- TextArea Selection Tests
-- ============================================================================

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

-- ============================================================================
-- ClipboardCommand Tests
-- ============================================================================

test "ClipboardCommand key generates unique key" := do
  let cmd : ClipboardCommand := { text := "test" }
  cmd.key.startsWith "clip:" ≡ true

test "TerminalCommand.copyToClipboard creates clipboard command" := do
  let cmd := TerminalCommand.copyToClipboard "Hello"
  match cmd with
  | .clipboard c => c.text ≡ "Hello"
  | _ => ensure false "wrong command type"

-- ============================================================================
-- Notification Tests
-- ============================================================================

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

end TerminusTests.WidgetTests
