/-
  Terminus Reactive - TextArea Component
  Multi-line text editor with scrolling, line numbers, and cursor management.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## TextArea Configuration -/

/-- Configuration for text area appearance and behavior. -/
structure TextAreaConfig where
  /-- Show line numbers in the gutter. -/
  showLineNumbers : Bool := true
  /-- Style for line numbers. -/
  lineNumberStyle : Style := { fg := .ansi .brightBlack }
  /-- Width of line number gutter (auto-calculated if 0). -/
  lineNumberWidth : Nat := 0
  /-- Style when unfocused. -/
  style : Style := {}
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan }
  /-- Cursor character. -/
  cursorChar : Char := '|'
  /-- Style for cursor. -/
  cursorStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Whether to wrap long lines. -/
  wrapLines : Bool := false
  /-- Maximum visible lines (none = show all). -/
  maxVisibleLines : Option Nat := none
  /-- Minimum visible width in characters. -/
  minWidth : Nat := 40
  /-- Tab width in spaces. -/
  tabWidth : Nat := 2
  deriving Repr, Inhabited

/-! ## TextArea Result -/

/-- Result returned by textArea' containing reactive values and events. -/
structure TextAreaResult where
  /-- Current text content as a Dynamic. -/
  content : Reactive.Dynamic Spider String
  /-- Current cursor position (line, column) as a Dynamic. -/
  cursorPos : Reactive.Dynamic Spider (Nat × Nat)
  /-- Event fired on Ctrl+Enter. -/
  onSubmit : Reactive.Event Spider String
  /-- Event fired on Escape. -/
  onCancel : Reactive.Event Spider Unit
  /-- Event fired whenever the text changes. -/
  onChange : Reactive.Event Spider String

/-! ## TextArea State -/

/-- Internal state for text area. -/
structure TextAreaState where
  /-- Lines of text. -/
  lines : Array String := #[""]
  /-- Current line (0-indexed). -/
  line : Nat := 0
  /-- Current column (0-indexed). -/
  column : Nat := 0
  /-- Scroll offset (first visible line). -/
  scrollOffset : Nat := 0
  deriving Repr, Inhabited

namespace TextAreaState

/-- Get the current line content. -/
def currentLine (s : TextAreaState) : String :=
  if h : s.line < s.lines.size then s.lines[s.line] else ""

/-- Get the full text content as a single string. -/
def getText (s : TextAreaState) : String :=
  String.intercalate "\n" s.lines.toList

/-- Create state from a string. -/
def fromString (text : String) : TextAreaState :=
  let lines := if text.isEmpty then #[""] else text.splitOn "\n" |>.toArray
  { lines, line := 0, column := 0, scrollOffset := 0 }

/-- Insert a character at cursor position. -/
def insertChar (s : TextAreaState) (c : Char) : TextAreaState :=
  if s.line < s.lines.size then
    let line := s.lines[s.line]!
    let before := line.take s.column
    let after := line.drop s.column
    let newLine := before ++ c.toString ++ after
    let linesBefore := s.lines.take s.line
    let linesAfter := s.lines.drop (s.line + 1)
    let newLines := linesBefore.push newLine ++ linesAfter
    { s with lines := newLines, column := s.column + 1 }
  else s

/-- Insert a newline at cursor position. -/
def insertNewline (s : TextAreaState) : TextAreaState :=
  if s.line < s.lines.size then
    let line := s.lines[s.line]!
    let before := line.take s.column
    let after := line.drop s.column
    -- Replace current line with before, insert after as new line
    let linesBefore := s.lines.take s.line |>.push before
    let linesAfter := #[after] ++ s.lines.drop (s.line + 1)
    let newLines := linesBefore ++ linesAfter
    { s with lines := newLines, line := s.line + 1, column := 0 }
  else s

/-- Insert a tab (spaces). -/
def insertTab (s : TextAreaState) (tabWidth : Nat) : TextAreaState :=
  let spaces := String.ofList (List.replicate tabWidth ' ')
  if s.line < s.lines.size then
    let line := s.lines[s.line]!
    let before := line.take s.column
    let after := line.drop s.column
    let newLine := before ++ spaces ++ after
    let linesBefore := s.lines.take s.line
    let linesAfter := s.lines.drop (s.line + 1)
    let newLines := linesBefore.push newLine ++ linesAfter
    { s with lines := newLines, column := s.column + tabWidth }
  else s

/-- Delete character before cursor (backspace). -/
def backspace (s : TextAreaState) : TextAreaState :=
  if s.column > 0 then
    -- Delete character on current line
    if s.line < s.lines.size then
      let line := s.lines[s.line]!
      let before := line.take (s.column - 1)
      let after := line.drop s.column
      let newLine := before ++ after
      let linesBefore := s.lines.take s.line
      let linesAfter := s.lines.drop (s.line + 1)
      let newLines := linesBefore.push newLine ++ linesAfter
      { s with lines := newLines, column := s.column - 1 }
    else s
  else if s.line > 0 then
    -- Merge with previous line
    if s.line < s.lines.size && s.line - 1 < s.lines.size then
      let prevLine := s.lines[s.line - 1]!
      let currLine := s.lines[s.line]!
      let mergedLine := prevLine ++ currLine
      -- Rebuild array without the current line
      let linesBefore := s.lines.take (s.line - 1) |>.push mergedLine
      let linesAfter := s.lines.drop (s.line + 1)
      let newLines := linesBefore ++ linesAfter
      { s with lines := newLines, line := s.line - 1, column := prevLine.length }
    else s
  else s

/-- Delete character at cursor (delete key). -/
def delete (s : TextAreaState) : TextAreaState :=
  if s.line < s.lines.size then
    let line := s.lines[s.line]!
    if s.column < line.length then
      -- Delete character on current line
      let before := line.take s.column
      let after := line.drop (s.column + 1)
      let newLine := before ++ after
      let linesBefore := s.lines.take s.line
      let linesAfter := s.lines.drop (s.line + 1)
      let newLines := linesBefore.push newLine ++ linesAfter
      { s with lines := newLines }
    else if s.line + 1 < s.lines.size then
      -- Merge with next line
      let nextLine := s.lines[s.line + 1]!
      let mergedLine := line ++ nextLine
      -- Rebuild array without the next line
      let linesBefore := s.lines.take s.line |>.push mergedLine
      let linesAfter := s.lines.drop (s.line + 2)
      let newLines := linesBefore ++ linesAfter
      { s with lines := newLines }
    else s
  else s

/-- Move cursor left. -/
def moveLeft (s : TextAreaState) : TextAreaState :=
  if s.column > 0 then
    { s with column := s.column - 1 }
  else if s.line > 0 then
    -- Move to end of previous line
    let prevLineLen := if h : s.line - 1 < s.lines.size then s.lines[s.line - 1].length else 0
    { s with line := s.line - 1, column := prevLineLen }
  else s

/-- Move cursor right. -/
def moveRight (s : TextAreaState) : TextAreaState :=
  if h : s.line < s.lines.size then
    let lineLen := s.lines[s.line].length
    if s.column < lineLen then
      { s with column := s.column + 1 }
    else if s.line + 1 < s.lines.size then
      -- Move to start of next line
      { s with line := s.line + 1, column := 0 }
    else s
  else s

/-- Move cursor up. -/
def moveUp (s : TextAreaState) : TextAreaState :=
  if s.line > 0 then
    let newLine := s.line - 1
    let newLineLen := if h : newLine < s.lines.size then s.lines[newLine].length else 0
    let newCol := min s.column newLineLen
    { s with line := newLine, column := newCol }
  else s

/-- Move cursor down. -/
def moveDown (s : TextAreaState) : TextAreaState :=
  if s.line + 1 < s.lines.size then
    let newLine := s.line + 1
    let newLineLen := if h : newLine < s.lines.size then s.lines[newLine].length else 0
    let newCol := min s.column newLineLen
    { s with line := newLine, column := newCol }
  else s

/-- Move cursor to start of line. -/
def moveHome (s : TextAreaState) : TextAreaState :=
  { s with column := 0 }

/-- Move cursor to end of line. -/
def moveEnd (s : TextAreaState) : TextAreaState :=
  if h : s.line < s.lines.size then
    { s with column := s.lines[s.line].length }
  else s

/-- Move cursor up by page. -/
def pageUp (s : TextAreaState) (pageSize : Nat) : TextAreaState :=
  let newLine := if s.line >= pageSize then s.line - pageSize else 0
  let newLineLen := if h : newLine < s.lines.size then s.lines[newLine].length else 0
  let newCol := min s.column newLineLen
  { s with line := newLine, column := newCol }

/-- Move cursor down by page. -/
def pageDown (s : TextAreaState) (pageSize : Nat) : TextAreaState :=
  let newLine := min (s.line + pageSize) (s.lines.size - 1)
  let newLineLen := if h : newLine < s.lines.size then s.lines[newLine].length else 0
  let newCol := min s.column newLineLen
  { s with line := newLine, column := newCol }

/-- Move to start of document. -/
def moveToStart (s : TextAreaState) : TextAreaState :=
  { s with line := 0, column := 0, scrollOffset := 0 }

/-- Move to end of document. -/
def moveToEnd (s : TextAreaState) : TextAreaState :=
  let lastLine := if s.lines.size > 0 then s.lines.size - 1 else 0
  let lastCol := if h : lastLine < s.lines.size then s.lines[lastLine].length else 0
  { s with line := lastLine, column := lastCol }

/-- Update scroll offset to keep cursor visible. -/
def ensureCursorVisible (s : TextAreaState) (visibleLines : Nat) : TextAreaState :=
  if visibleLines == 0 then s
  else
    let newOffset := if s.line < s.scrollOffset then
      s.line
    else if s.line >= s.scrollOffset + visibleLines then
      s.line - visibleLines + 1
    else
      s.scrollOffset
    { s with scrollOffset := newOffset }

end TextAreaState

/-! ## TextArea Widget -/

/-- Create a multi-line text area widget.

    The widget handles:
    - Multi-line text editing
    - Cursor movement (arrows, home, end, page up/down)
    - Deletion (backspace, delete)
    - Submit (Ctrl+Enter) and cancel (Escape) events
    - Line numbers (optional)
    - Scrolling for tall content

    Example:
    ```
    let editor ← textArea' "notes" "" { showLineNumbers := true }
    -- Use editor.content to get current text
    -- Use editor.onSubmit to handle Ctrl+Enter
    ```
-/
def textArea' (name : String) (initial : String := "")
    (config : TextAreaConfig := {}) : WidgetM TextAreaResult := do
  -- Register as focusable input
  let widgetName ← registerComponentW "textArea" (isInput := true) (nameOverride := name)

  -- Compute inputName before calling useFocusedKeyEventsW
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Create trigger events
  let (submitEvent, fireSubmit) ← newTriggerEvent (t := Spider) (a := String)
  let (cancelEvent, fireCancel) ← newTriggerEvent (t := Spider) (a := Unit)
  let (changeEvent, fireChange) ← newTriggerEvent (t := Spider) (a := String)

  -- Track internal state
  let initialState := TextAreaState.fromString initial
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := TextAreaState)
  let stateDyn ← holdDyn initialState stateEvent

  -- Create dynamics for content and cursor position
  let (contentEvent, fireContent) ← newTriggerEvent (t := Spider) (a := String)
  let (cursorEvent, fireCursor) ← newTriggerEvent (t := Spider) (a := Nat × Nat)
  let contentDyn ← holdDyn initial contentEvent
  let cursorDyn ← holdDyn (0, 0) cursorEvent

  -- Get focus state for rendering
  let focusedInput ← useFocusedInputW

  -- Calculate visible lines
  let visibleLines := config.maxVisibleLines.getD 20

  -- Subscribe to key events when focused
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
      let state ← stateRef.get
      let ke := kd.event

      -- Handle key
      let newState ← match ke.code with
        | .char c =>
          if c.val >= 32 then
            pure (state.insertChar c)
          else
            pure state
        | .tab =>
          pure (state.insertTab config.tabWidth)
        | .enter =>
          if ke.modifiers.ctrl then
            fireSubmit state.getText
            pure state
          else
            pure state.insertNewline
        | .backspace => pure state.backspace
        | .delete => pure state.delete
        | .left => pure state.moveLeft
        | .right => pure state.moveRight
        | .up => pure state.moveUp
        | .down => pure state.moveDown
        | .home =>
          if ke.modifiers.ctrl then
            pure state.moveToStart
          else
            pure state.moveHome
        | .end =>
          if ke.modifiers.ctrl then
            pure state.moveToEnd
          else
            pure state.moveEnd
        | .pageUp => pure (state.pageUp visibleLines)
        | .pageDown => pure (state.pageDown visibleLines)
        | .escape =>
          fireCancel ()
          pure state
        | _ => pure state

      -- Ensure cursor is visible
      let newState := newState.ensureCursorVisible visibleLines

      -- Update state and fire events if text changed
      let oldText := state.getText
      let newText := newState.getText
      if newText != oldText then
        stateRef.set newState
        fireState newState
        fireContent newText
        fireChange newText
        fireCursor (newState.line, newState.column)
      else if newState.line != state.line || newState.column != state.column || newState.scrollOffset != state.scrollOffset then
        stateRef.set newState
        fireState newState
        fireCursor (newState.line, newState.column)

  let node ← focusedInput.zipWith' (fun currentFocus state =>
    let isFocused := currentFocus == some inputName

    let lineCount := state.lines.size
    let lineNumWidth := if config.showLineNumbers then
      if config.lineNumberWidth > 0 then config.lineNumberWidth
      else max 2 (toString lineCount).length
    else 0

    let startLine := state.scrollOffset
    let endLine := min (startLine + visibleLines) lineCount

    let lineIndices := List.range' startLine (endLine - startLine)
    let rows := lineIndices.filterMap fun lineIdx =>
      if lineIdx < state.lines.size then
        let lineText := state.lines[lineIdx]!
        let isCurrentLine := lineIdx == state.line

        let lineNumNode := if config.showLineNumbers then
          let numStr := toString (lineIdx + 1)
          let padded := String.ofList (List.replicate (lineNumWidth - numStr.length) ' ') ++ numStr ++ " "
          RNode.text padded config.lineNumberStyle
        else
          RNode.empty

        let lineNode := if isFocused && isCurrentLine then
          let before := lineText.take state.column
          let after := lineText.drop state.column
          let cursorStr := config.cursorChar.toString

          let cursorNode := RNode.text cursorStr config.cursorStyle
          let currentLen := lineText.length + 1
          let padding := if currentLen < config.minWidth then config.minWidth - currentLen else 0

          let nodes := #[]
            |> (if before.isEmpty then id else (·.push (RNode.text before config.focusedStyle)))
            |>.push cursorNode
            |> (if after.isEmpty then id else (·.push (RNode.text after config.focusedStyle)))
            |> (if padding > 0 then (·.push (RNode.text (String.ofList (List.replicate padding ' ')) config.focusedStyle)) else id)
          RNode.row 0 {} nodes
        else
          let style := if isFocused then config.focusedStyle else config.style
          let padding := if lineText.length < config.minWidth then config.minWidth - lineText.length else 0
          let padStr := String.ofList (List.replicate padding ' ')
          RNode.text (lineText ++ padStr) style

        let rowNode := if config.showLineNumbers then
          RNode.row 0 {} #[lineNumNode, lineNode]
        else
          lineNode

        some rowNode
      else
        none

    let finalRows := if rows.isEmpty then
      let cursorNode := if isFocused then
        RNode.text config.cursorChar.toString config.cursorStyle
      else
        RNode.empty
      [cursorNode]
    else
      rows

    RNode.column 0 {} finalRows.toArray
  ) stateDyn
  emit node

  pure {
    content := contentDyn
    cursorPos := cursorDyn
    onSubmit := submitEvent
    onCancel := cancelEvent
    onChange := changeEvent
  }

/-- Create a labeled text area with the label above. -/
def labeledTextArea' (label : String) (name : String) (initial : String := "")
    (config : TextAreaConfig := {}) (theme : Theme := .dark) : WidgetM TextAreaResult := do
  emitStatic (RNode.text label theme.bodyStyle)
  textArea' name initial config

/-- Create a text area that auto-focuses on creation. -/
def focusedTextArea' (name : String) (initial : String := "")
    (config : TextAreaConfig := {}) : WidgetM TextAreaResult := do
  let events ← getEventsW
  let inputName := if name.isEmpty then "textArea-auto" else name
  SpiderM.liftIO <| events.registry.fireFocus (some inputName)
  textArea' name initial config

/-- Create a read-only text display (no editing, but scrollable). -/
def textDisplay' (content : Reactive.Dynamic Spider String)
    (config : TextAreaConfig := {}) : WidgetM Unit := do
  let node ← content.map' fun text =>
    Id.run do
      let lines := if text.isEmpty then #[""] else text.splitOn "\n" |>.toArray
      let lineCount := lines.size

      let lineNumWidth := if config.showLineNumbers then
        if config.lineNumberWidth > 0 then config.lineNumberWidth
        else max 2 (toString lineCount).length
      else 0

      let visibleLines := config.maxVisibleLines.getD lines.size
      let endLine := min visibleLines lineCount

      let mut rows : Array RNode := #[]

      for lineIdx in [:endLine] do
        if h : lineIdx < lines.size then
          let lineText := lines[lineIdx]

          let lineNumNode := if config.showLineNumbers then
            let numStr := toString (lineIdx + 1)
            let padded := String.ofList (List.replicate (lineNumWidth - numStr.length) ' ') ++ numStr ++ " "
            RNode.text padded config.lineNumberStyle
          else
            RNode.empty

          let lineNode := RNode.text lineText config.style

          let rowNode := if config.showLineNumbers then
            RNode.row 0 {} #[lineNumNode, lineNode]
          else
            lineNode

          rows := rows.push rowNode

      if rows.isEmpty then
        return RNode.empty
      else
        return RNode.column 0 {} rows
  emit node

end Terminus.Reactive
