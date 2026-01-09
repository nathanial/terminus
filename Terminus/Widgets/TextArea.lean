-- Terminus.Widgets.TextArea: Multi-line text editor

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Input.Key
import Terminus.Core.Unicode

namespace Terminus

/-- Position in a text area (row, col) -/
structure TextPosition where
  row : Nat
  col : Nat
  deriving BEq, Repr, Inhabited

namespace TextPosition

def compare (a b : TextPosition) : Ordering :=
  match Ord.compare a.row b.row with
  | .eq => Ord.compare a.col b.col
  | other => other

def le (a b : TextPosition) : Bool :=
  a.compare b != .gt

def lt (a b : TextPosition) : Bool :=
  a.compare b == .lt

instance : LE TextPosition where le a b := a.le b
instance : LT TextPosition where lt a b := a.lt b

end TextPosition

/-- Multi-line text area widget -/
structure TextArea where
  lines : Array String := #[""]
  cursorRow : Nat := 0
  cursorCol : Nat := 0
  scrollRow : Nat := 0
  scrollCol : Nat := 0
  selectionAnchor : Option TextPosition := none  -- Start of selection (if any)
  focused : Bool := true
  showLineNumbers : Bool := false
  lineNumberWidth : Nat := 4
  style : Style := Style.default
  cursorStyle : Style := Style.reversed
  selectionStyle : Style := Style.reversed.withBg Color.blue
  lineNumberStyle : Style := Style.dim
  currentLineStyle : Option Style := none  -- Highlight current line
  block : Option Block := none
  deriving Repr, Inhabited

namespace TextArea

/-- Convert a character index to a display column position -/
def displayColumn (s : String) (charIndex : Nat) : Nat :=
  (s.take charIndex).displayWidth

def new : TextArea := {}

def fromString (s : String) : TextArea :=
  { lines := s.splitOn "\n" |>.toArray }

def fromLines (ls : List String) : TextArea :=
  { lines := ls.toArray }

def withLines (t : TextArea) (ls : Array String) : TextArea := { t with lines := ls }
def withStyle (t : TextArea) (s : Style) : TextArea := { t with style := s }
def withCursorStyle (t : TextArea) (s : Style) : TextArea := { t with cursorStyle := s }
def withLineNumberStyle (t : TextArea) (s : Style) : TextArea := { t with lineNumberStyle := s }
def withCurrentLineStyle (t : TextArea) (s : Style) : TextArea := { t with currentLineStyle := some s }
def withBlock (t : TextArea) (b : Block) : TextArea := { t with block := some b }

def showNumbers (t : TextArea) : TextArea := { t with showLineNumbers := true }
def hideNumbers (t : TextArea) : TextArea := { t with showLineNumbers := false }

def focus (t : TextArea) : TextArea := { t with focused := true }
def blur (t : TextArea) : TextArea := { t with focused := false }
def toggleFocus (t : TextArea) : TextArea := { t with focused := !t.focused }

/-- Get the current line -/
def currentLine (t : TextArea) : String :=
  t.lines[t.cursorRow]?.getD ""

/-- Get total line count -/
def lineCount (t : TextArea) : Nat := t.lines.size

/-- Get the text as a single string -/
def text (t : TextArea) : String :=
  String.intercalate "\n" t.lines.toList

/-- Set the text from a string -/
def setText (t : TextArea) (s : String) : TextArea :=
  let newLines := s.splitOn "\n" |>.toArray
  { t with
    lines := if newLines.isEmpty then #[""] else newLines
    cursorRow := 0
    cursorCol := 0
    scrollRow := 0
    scrollCol := 0
  }

/-- Ensure cursor position is valid -/
def clampCursor (t : TextArea) : TextArea :=
  let row := Nat.min t.cursorRow (t.lines.size - 1)
  let lineLen := (t.lines[row]?.getD "").length
  let col := Nat.min t.cursorCol lineLen
  { t with cursorRow := row, cursorCol := col }

/-- Insert a character at cursor position -/
def insertChar (t : TextArea) (c : Char) : TextArea :=
  if !t.focused then t
  else
    match t.lines[t.cursorRow]? with
    | none => t
    | some line =>
      let before := line.take t.cursorCol
      let after := line.drop t.cursorCol
      let newLine := before ++ String.singleton c ++ after
      { t with
        lines := t.lines.modify t.cursorRow (fun _ => newLine)
        cursorCol := t.cursorCol + 1
      }

/-- Insert a new line at cursor position -/
def insertNewline (t : TextArea) : TextArea :=
  if !t.focused then t
  else
    match t.lines[t.cursorRow]? with
    | none => t
    | some line =>
      let before := line.take t.cursorCol
      let after := line.drop t.cursorCol
      let beforeLines := t.lines.extract 0 t.cursorRow
      let afterLines := t.lines.extract (t.cursorRow + 1) t.lines.size
      let newLines := beforeLines.push before |>.push after |>.append afterLines
      { t with
        lines := newLines
        cursorRow := t.cursorRow + 1
        cursorCol := 0
      }

/-- Delete character before cursor (backspace) -/
def deleteBackward (t : TextArea) : TextArea :=
  if !t.focused then t
  else if t.cursorCol > 0 then
    -- Delete within line
    match t.lines[t.cursorRow]? with
    | none => t
    | some line =>
      let before := line.take (t.cursorCol - 1)
      let after := line.drop t.cursorCol
      { t with
        lines := t.lines.modify t.cursorRow (fun _ => before ++ after)
        cursorCol := t.cursorCol - 1
      }
  else if t.cursorRow > 0 then
    -- Merge with previous line
    match (t.lines[t.cursorRow - 1]?, t.lines[t.cursorRow]?) with
    | (some prevLine, some currLine) =>
      let mergedLine := prevLine ++ currLine
      let beforeLines := t.lines.extract 0 (t.cursorRow - 1)
      let afterLines := t.lines.extract (t.cursorRow + 1) t.lines.size
      let newLines := beforeLines.push mergedLine |>.append afterLines
      { t with
        lines := newLines
        cursorRow := t.cursorRow - 1
        cursorCol := prevLine.length
      }
    | _ => t
  else t

/-- Delete character at cursor (delete key) -/
def deleteForward (t : TextArea) : TextArea :=
  if !t.focused then t
  else
    match t.lines[t.cursorRow]? with
    | none => t
    | some line =>
      if t.cursorCol < line.length then
        -- Delete within line
        let before := line.take t.cursorCol
        let after := line.drop (t.cursorCol + 1)
        { t with lines := t.lines.modify t.cursorRow (fun _ => before ++ after) }
      else if t.cursorRow + 1 < t.lines.size then
        -- Merge with next line
        match t.lines[t.cursorRow + 1]? with
        | some nextLine =>
          let mergedLine := line ++ nextLine
          let beforeLines := t.lines.extract 0 t.cursorRow
          let afterLines := t.lines.extract (t.cursorRow + 2) t.lines.size
          { t with lines := beforeLines.push mergedLine |>.append afterLines }
        | none => t
      else t

/-- Move cursor left -/
def moveCursorLeft (t : TextArea) : TextArea :=
  if t.cursorCol > 0 then { t with cursorCol := t.cursorCol - 1 }
  else if t.cursorRow > 0 then
    let prevLineLen := (t.lines[t.cursorRow - 1]?.getD "").length
    { t with cursorRow := t.cursorRow - 1, cursorCol := prevLineLen }
  else t

/-- Move cursor right -/
def moveCursorRight (t : TextArea) : TextArea :=
  let lineLen := (t.lines[t.cursorRow]?.getD "").length
  if t.cursorCol < lineLen then { t with cursorCol := t.cursorCol + 1 }
  else if t.cursorRow + 1 < t.lines.size then
    { t with cursorRow := t.cursorRow + 1, cursorCol := 0 }
  else t

/-- Move cursor up -/
def moveCursorUp (t : TextArea) : TextArea :=
  if t.cursorRow > 0 then
    let newRow := t.cursorRow - 1
    let lineLen := (t.lines[newRow]?.getD "").length
    { t with cursorRow := newRow, cursorCol := Nat.min t.cursorCol lineLen }
  else t

/-- Move cursor down -/
def moveCursorDown (t : TextArea) : TextArea :=
  if t.cursorRow + 1 < t.lines.size then
    let newRow := t.cursorRow + 1
    let lineLen := (t.lines[newRow]?.getD "").length
    { t with cursorRow := newRow, cursorCol := Nat.min t.cursorCol lineLen }
  else t

/-- Move cursor to start of line -/
def moveCursorHome (t : TextArea) : TextArea :=
  { t with cursorCol := 0 }

/-- Move cursor to end of line -/
def moveCursorEnd (t : TextArea) : TextArea :=
  let lineLen := (t.lines[t.cursorRow]?.getD "").length
  { t with cursorCol := lineLen }

/-- Get the current cursor position -/
def cursorPosition (t : TextArea) : TextPosition :=
  { row := t.cursorRow, col := t.cursorCol }

/-- Check if there is an active selection -/
def hasSelection (t : TextArea) : Bool :=
  t.selectionAnchor.isSome

/-- Clear the selection -/
def clearSelection (t : TextArea) : TextArea :=
  { t with selectionAnchor := none }

/-- Start a selection at current cursor position -/
def startSelection (t : TextArea) : TextArea :=
  { t with selectionAnchor := some t.cursorPosition }

/-- Get the selection range as (start, end) where start <= end -/
def selectionRange (t : TextArea) : Option (TextPosition × TextPosition) :=
  t.selectionAnchor.map fun anchor =>
    let cursor := t.cursorPosition
    if anchor.le cursor then (anchor, cursor)
    else (cursor, anchor)

/-- Check if a position is within the selection -/
def isInSelection (t : TextArea) (pos : TextPosition) : Bool :=
  match t.selectionRange with
  | none => false
  | some (start, «end») => start.le pos && pos.lt «end»

/-- Get the selected text -/
def selectedText (t : TextArea) : String :=
  match t.selectionRange with
  | none => ""
  | some (start, «end») =>
    if start.row == «end».row then
      -- Single line selection
      match t.lines[start.row]? with
      | some line => line.drop start.col |>.take («end».col - start.col)
      | none => ""
    else
      -- Multi-line selection
      Id.run do
        let mut result : List String := []
        -- First line (from start.col to end of line)
        match t.lines[start.row]? with
        | some line => result := result ++ [line.drop start.col]
        | none => pure ()
        -- Middle lines (complete lines)
        for row in [start.row + 1 : «end».row] do
          match t.lines[row]? with
          | some line => result := result ++ [line]
          | none => pure ()
        -- Last line (from start to end.col)
        match t.lines[«end».row]? with
        | some line => result := result ++ [line.take «end».col]
        | none => pure ()
        String.intercalate "\n" result

/-- Delete the selected text -/
def deleteSelection (t : TextArea) : TextArea :=
  match t.selectionRange with
  | none => t
  | some (start, «end») =>
    if start.row == «end».row then
      -- Single line deletion
      match t.lines[start.row]? with
      | some line =>
        let before := line.take start.col
        let after := line.drop «end».col
        { t with
          lines := t.lines.modify start.row (fun _ => before ++ after)
          cursorRow := start.row
          cursorCol := start.col
          selectionAnchor := none
        }
      | none => { t with selectionAnchor := none }
    else
      -- Multi-line deletion
      let firstLinePart := match t.lines[start.row]? with
        | some line => line.take start.col
        | none => ""
      let lastLinePart := match t.lines[«end».row]? with
        | some line => line.drop «end».col
        | none => ""
      let mergedLine := firstLinePart ++ lastLinePart
      let beforeLines := t.lines.extract 0 start.row
      let afterLines := t.lines.extract («end».row + 1) t.lines.size
      let newLines := beforeLines.push mergedLine |>.append afterLines
      { t with
        lines := if newLines.isEmpty then #[""] else newLines
        cursorRow := start.row
        cursorCol := start.col
        selectionAnchor := none
      }

/-- Select all text -/
def selectAll (t : TextArea) : TextArea :=
  let lastRow := t.lines.size - 1
  let lastCol := (t.lines[lastRow]?.getD "").length
  { t with
    selectionAnchor := some { row := 0, col := 0 }
    cursorRow := lastRow
    cursorCol := lastCol
  }

/-- Copy selected text (returns the text to copy, or all text if no selection) -/
def copy (t : TextArea) : String :=
  if t.hasSelection then t.selectedText
  else t.text

/-- Cut selected text (returns new state and the cut text) -/
def cut (t : TextArea) : TextArea × String :=
  if t.hasSelection then
    let text := t.selectedText
    (t.deleteSelection, text)
  else
    let text := t.text
    ({ t with lines := #[""], cursorRow := 0, cursorCol := 0, selectionAnchor := none }, text)

/-- Paste text at cursor position (replaces selection if any) -/
def paste (t : TextArea) (text : String) : TextArea :=
  let t := if t.hasSelection then t.deleteSelection else t
  let pasteLines := text.splitOn "\n"
  match pasteLines with
  | [] => t
  | [single] =>
    -- Single line paste: insert at cursor
    match t.lines[t.cursorRow]? with
    | some line =>
      let before := line.take t.cursorCol
      let after := line.drop t.cursorCol
      { t with
        lines := t.lines.modify t.cursorRow (fun _ => before ++ single ++ after)
        cursorCol := t.cursorCol + single.length
      }
    | none => t
  | first :: rest =>
    -- Multi-line paste
    match t.lines[t.cursorRow]? with
    | some line =>
      let before := line.take t.cursorCol
      let after := line.drop t.cursorCol
      let lastPasteLine := rest.getLast?.getD ""
      let middleLines := rest.dropLast
      let firstLine := before ++ first
      let lastLine := lastPasteLine ++ after
      let beforeLines := t.lines.extract 0 t.cursorRow
      let afterLines := t.lines.extract (t.cursorRow + 1) t.lines.size
      let newLines := beforeLines.push firstLine
        |>.append (middleLines.toArray)
        |>.push lastLine
        |>.append afterLines
      { t with
        lines := newLines
        cursorRow := t.cursorRow + rest.length
        cursorCol := lastPasteLine.length
      }
    | none => t

/-- Handle a key event with clipboard support, returning new state and optional text to copy -/
def handleKeyWithClipboard (t : TextArea) (key : KeyEvent) : TextArea × Option String :=
  if !t.focused then (t, none)
  else
    -- Handle Alt key combinations (Alt used instead of Ctrl to avoid terminal signal conflicts)
    if key.modifiers.alt then
      match key.code with
      | .char 'a' => (t.selectAll, none)
      | .char 'c' => (t, some t.copy)
      | .char 'x' =>
        let (newT, text) := t.cut
        (newT, some text)
      | _ => (t, none)
    else
      -- Clear selection on most key presses (except navigation)
      let t := match key.code with
        | .left | .right | .up | .down | .home | .«end» => t  -- Keep selection for navigation
        | _ => if t.hasSelection && key.code != .backspace && key.code != .delete then t.clearSelection else t

      match key.code with
      | .char c =>
        let t := if t.hasSelection then t.deleteSelection else t
        (t.insertChar c, none)
      | .space =>
        let t := if t.hasSelection then t.deleteSelection else t
        (t.insertChar ' ', none)
      | .enter =>
        let t := if t.hasSelection then t.deleteSelection else t
        (t.insertNewline, none)
      | .backspace =>
        if t.hasSelection then (t.deleteSelection, none)
        else (t.deleteBackward, none)
      | .delete =>
        if t.hasSelection then (t.deleteSelection, none)
        else (t.deleteForward, none)
      | .left => (t.moveCursorLeft.clearSelection, none)
      | .right => (t.moveCursorRight.clearSelection, none)
      | .up => (t.moveCursorUp.clearSelection, none)
      | .down => (t.moveCursorDown.clearSelection, none)
      | .home => (t.moveCursorHome.clearSelection, none)
      | .«end» => (t.moveCursorEnd.clearSelection, none)
      | _ => (t, none)

/-- Handle a key event (without clipboard support) -/
def handleKey (t : TextArea) (key : KeyEvent) : TextArea :=
  (t.handleKeyWithClipboard key).1

/-- Adjust scroll to keep cursor visible (display width aware) -/
def adjustScroll (t : TextArea) (visibleHeight visibleWidth : Nat) : TextArea :=
  -- Vertical scroll
  let scrollRow :=
    if visibleHeight > 0 then
      if t.cursorRow < t.scrollRow then t.cursorRow
      else if t.cursorRow >= t.scrollRow + visibleHeight then t.cursorRow - visibleHeight + 1
      else t.scrollRow
    else t.scrollRow

  -- Horizontal scroll (display width aware)
  let currentLine := t.lines.getD t.cursorRow ""
  let cursorDisplayCol := displayColumn currentLine t.cursorCol
  let scrollDisplayCol := displayColumn currentLine t.scrollCol

  let scrollCol :=
    if visibleWidth > 0 then
      if t.cursorCol < t.scrollCol then t.cursorCol
      else if cursorDisplayCol - scrollDisplayCol >= visibleWidth then
        -- Find new scroll offset so cursor is visible
        Id.run do
          let mut offset := t.scrollCol
          while displayColumn currentLine t.cursorCol - displayColumn currentLine offset >= visibleWidth do
            offset := offset + 1
            if offset >= t.cursorCol then break
          offset
      else t.scrollCol
    else t.scrollCol

  { t with scrollRow := scrollRow, scrollCol := scrollCol }

end TextArea

instance : Widget TextArea where
  render t area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner t.block area buf
    if contentArea.isEmpty then return buf'
    let mut result := buf'

    -- Calculate line number width
    let lineNumWidth := if t.showLineNumbers then t.lineNumberWidth else 0
    let textStartX := contentArea.x + lineNumWidth

    -- Calculate visible text area
    let textWidth := if contentArea.width > lineNumWidth then contentArea.width - lineNumWidth else 0
    let visibleHeight := contentArea.height

    -- Adjust scroll (vertical is simple, horizontal needs display width awareness)
    let scrollRow := if t.cursorRow < t.scrollRow then t.cursorRow
                     else if t.cursorRow >= t.scrollRow + visibleHeight then t.cursorRow - visibleHeight + 1
                     else t.scrollRow

    -- Horizontal scroll with display width
    let currentLine := t.lines.getD t.cursorRow ""
    let cursorDisplayCol := TextArea.displayColumn currentLine t.cursorCol
    let scrollDisplayCol := TextArea.displayColumn currentLine t.scrollCol
    let scrollCol := if t.cursorCol < t.scrollCol then t.cursorCol
                     else if cursorDisplayCol - scrollDisplayCol >= textWidth then
                       -- Find scroll offset so cursor is visible
                       Id.run do
                         let mut offset := t.scrollCol
                         while TextArea.displayColumn currentLine t.cursorCol - TextArea.displayColumn currentLine offset >= textWidth do
                           offset := offset + 1
                           if offset >= t.cursorCol then break
                         offset
                     else t.scrollCol

    -- Render visible lines
    for row in [:visibleHeight] do
      let lineIdx := scrollRow + row
      let y := contentArea.y + row

      -- Render line number
      if t.showLineNumbers then
        let numStr := s!"{lineIdx + 1}"
        let numWidth := if t.lineNumberWidth > 0 then t.lineNumberWidth - 1 else 0
        let trimmedNum := if numStr.length > numWidth then numStr.drop (numStr.length - numWidth) else numStr
        let padCount := numWidth - trimmedNum.length
        let sep := if t.lineNumberWidth > 0 then "│" else ""
        let paddedNum := String.ofList (List.replicate padCount ' ') ++ trimmedNum ++ sep
        let paddedNumChars := paddedNum.toList
        for i in [:paddedNumChars.length] do
          let x := contentArea.x + i
          if x < textStartX then
            match paddedNumChars[i]? with
            | some c => result := result.setStyled x y c t.lineNumberStyle
            | none => pure ()

      -- Render line content with display width awareness
      match t.lines[lineIdx]? with
      | none => pure ()
      | some line =>
        let isCurrentLine := lineIdx == t.cursorRow

        -- Apply current line style if set
        let baseStyle := match (isCurrentLine, t.currentLineStyle) with
          | (true, some s) => s
          | _ => t.style

        -- Get selection range for highlight checking
        let selRange := t.selectionRange

        -- Render characters with display width tracking
        let chars := (line.drop scrollCol).toList
        let mut displayCol : Nat := 0
        let mut charIdx : Nat := scrollCol

        for c in chars do
          if displayCol >= textWidth then break

          let charWidth := c.displayWidth
          -- Skip zero-width characters
          if charWidth == 0 then
            charIdx := charIdx + 1
            continue

          -- Check if character would extend past visible area
          if displayCol + charWidth > textWidth then break

          let x := textStartX + displayCol
          let pos : TextPosition := { row := lineIdx, col := charIdx }
          let isCursor := t.focused && isCurrentLine && charIdx == t.cursorCol

          -- Check if this position is within selection
          let isSelected := match selRange with
            | some (start, «end») => start.le pos && pos.lt «end»
            | none => false

          let style := if isCursor then t.cursorStyle
                       else if isSelected then t.selectionStyle
                       else baseStyle

          result := result.setStyled x y c style
          -- For wide characters, mark the next cell as a placeholder
          if charWidth == 2 then
            result := result.set (x + 1) y Cell.placeholder

          displayCol := displayCol + charWidth
          charIdx := charIdx + 1

        -- Render cursor at end of line if needed
        if t.focused && isCurrentLine then
          let cursorDisplayCol := TextArea.displayColumn line t.cursorCol -
                                  TextArea.displayColumn line scrollCol
          if cursorDisplayCol >= displayCol && cursorDisplayCol < textWidth then
            let x := textStartX + cursorDisplayCol
            result := result.setStyled x y ' ' t.cursorStyle

    result
  handleEvent t event :=
    match event with
    | .key k => t.handleKey k
    | _ => t
  focusable _ := true
  setFocused t focused := if focused then t.focus else t.blur

end Terminus
