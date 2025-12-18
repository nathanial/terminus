-- TextEditor: Input widgets demo with TextInput, TextArea, and Calendar
-- Tab to switch focus, Ctrl+Q to quit

import Terminus

open Terminus

/-- Focus targets -/
inductive Focus where
  | filename
  | content
  | calendar
  deriving BEq, Inhabited

structure EditorState where
  filename : TextInput := TextInput.new.withValue "untitled.txt"
  content : TextArea := TextArea.fromLines [
    "Hello, world!",
    "",
    "This is a simple text editor demo.",
    "Use Tab to switch between widgets.",
    "",
    "Features:",
    "- Single-line text input (filename)",
    "- Multi-line text area (content)",
    "- Calendar widget (date picker)",
    "",
    "Try typing in each field!"
  ] |>.showNumbers
  calendar : Calendar := Calendar.new 2025 1 |>.withSelectedDay 15
  focus : Focus := .content
  deriving Inhabited

def draw (frame : Frame) (state : EditorState) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  -- Main border
  let mainBlock := Block.double
    |>.withTitle "Simple Editor"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  -- Layout: filename at top, then content + calendar side by side
  let sections := vsplit inner [.fixed 3, .fill, .fixed 1]

  -- Filename input
  if h : 0 < sections.length then
    let filenameArea := sections[0]
    let isFocused := state.focus == .filename
    let borderStyle := if isFocused then Style.fgColor Color.yellow else Style.fgColor Color.white
    let input := { state.filename with focused := isFocused }
      |>.withPlaceholder "Enter filename..."
      |>.withBlock (Block.rounded.withTitle "Filename" |>.withBorderStyle borderStyle)
    f := f.render input filenameArea

  -- Content area
  if h : 1 < sections.length then
    let contentSection := sections[1]
    let panels := hsplit contentSection [.fill, .fixed 24]

    -- Text area
    if hp : 0 < panels.length then
      let textArea := panels[0]
      let isFocused := state.focus == .content
      let borderStyle := if isFocused then Style.fgColor Color.yellow else Style.fgColor Color.white
      let editor := { state.content with focused := isFocused }
        |>.withCursorStyle Style.reversed
        |>.withLineNumberStyle (Style.dim.withFg Color.cyan)
        |>.withBlock (Block.rounded.withTitle "Content" |>.withBorderStyle borderStyle)
      f := f.render editor textArea

    -- Calendar
    if hp : 1 < panels.length then
      let calendarArea := panels[1]
      let isFocused := state.focus == .calendar
      let borderStyle := if isFocused then Style.fgColor Color.yellow else Style.fgColor Color.white
      let cal := state.calendar
        |>.withSelectedStyle (if isFocused then Style.reversed else Style.bold)
        |>.withBlock (Block.rounded.withTitle "Date" |>.withBorderStyle borderStyle)
      f := f.render cal calendarArea

  -- Status bar
  if h : 2 < sections.length then
    let statusArea := sections[2]
    let focusName := match state.focus with
      | .filename => "Filename"
      | .content => "Content"
      | .calendar => "Calendar"

    let statusLeft := s!"Focus: {focusName}"
    let statusRight := s!"Ln {state.content.cursorRow + 1}, Col {state.content.cursorCol + 1}"
    let statusMiddle := "Tab: Switch | Ctrl+Q: Quit"

    f := f.writeString statusArea.x statusArea.y statusLeft (Style.dim.withFg Color.cyan)
    let middleX := statusArea.x + (statusArea.width - statusMiddle.length) / 2
    f := f.writeString middleX statusArea.y statusMiddle Style.dim
    let rightX := statusArea.x + statusArea.width - statusRight.length
    f := f.writeString rightX statusArea.y statusRight (Style.dim.withFg Color.green)

  f

def update (state : EditorState) (key : KeyEvent) : EditorState × Bool :=
  -- Global controls
  if key.isCtrlQ then (state, true)
  else if key.isCtrlC then (state, true)
  else match key.code with
  -- Tab to switch focus
  | .tab =>
    let nextFocus := match state.focus with
      | .filename => .content
      | .content => .calendar
      | .calendar => .filename
    ({ state with focus := nextFocus }, false)

  -- Shift+Tab (backtab) - not easily detectable, use Escape instead
  | .escape =>
    let prevFocus := match state.focus with
      | .filename => .calendar
      | .content => .filename
      | .calendar => .content
    ({ state with focus := prevFocus }, false)

  -- Handle input based on focus
  | _ =>
    match state.focus with
    | .filename =>
      let newFilename := state.filename.handleKey key
      ({ state with filename := newFilename }, false)

    | .content =>
      let newContent := state.content.handleKey key
      ({ state with content := newContent }, false)

    | .calendar =>
      let newCalendar := match key.code with
        | .left => state.calendar.prevDay
        | .right => state.calendar.nextDay
        | .up => state.calendar.prevWeek
        | .down => state.calendar.nextWeek
        | .char 'h' => state.calendar.prevMonth
        | .char 'l' => state.calendar.nextMonth
        | _ => state.calendar
      ({ state with calendar := newCalendar }, false)

def main : IO Unit := do
  let initialState : EditorState := {}
  App.runApp initialState draw update
