-- Terminus.Widgets.TextArea: Multi-line text editor

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Input.Key

namespace Terminus

/-- Multi-line text area widget -/
structure TextArea where
  lines : Array String := #[""]
  cursorRow : Nat := 0
  cursorCol : Nat := 0
  scrollRow : Nat := 0
  scrollCol : Nat := 0
  focused : Bool := true
  showLineNumbers : Bool := false
  lineNumberWidth : Nat := 4
  style : Style := Style.default
  cursorStyle : Style := Style.reversed
  lineNumberStyle : Style := Style.dim
  currentLineStyle : Option Style := none  -- Highlight current line
  block : Option Block := none
  deriving Repr, Inhabited

namespace TextArea

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
        lines := t.lines.set! t.cursorRow newLine
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
        lines := t.lines.set! t.cursorRow (before ++ after)
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
        { t with lines := t.lines.set! t.cursorRow (before ++ after) }
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

/-- Handle a key event -/
def handleKey (t : TextArea) (key : KeyEvent) : TextArea :=
  if !t.focused then t
  else match key.code with
  | .char c => t.insertChar c
  | .enter => t.insertNewline
  | .backspace => t.deleteBackward
  | .delete => t.deleteForward
  | .left => t.moveCursorLeft
  | .right => t.moveCursorRight
  | .up => t.moveCursorUp
  | .down => t.moveCursorDown
  | .home => t.moveCursorHome
  | .«end» => t.moveCursorEnd
  | _ => t

/-- Adjust scroll to keep cursor visible -/
def adjustScroll (t : TextArea) (visibleHeight visibleWidth : Nat) : TextArea :=
  -- Vertical scroll
  let scrollRow :=
    if visibleHeight > 0 then
      if t.cursorRow < t.scrollRow then t.cursorRow
      else if t.cursorRow >= t.scrollRow + visibleHeight then t.cursorRow - visibleHeight + 1
      else t.scrollRow
    else t.scrollRow

  -- Horizontal scroll
  let scrollCol :=
    if visibleWidth > 0 then
      if t.cursorCol < t.scrollCol then t.cursorCol
      else if t.cursorCol >= t.scrollCol + visibleWidth then t.cursorCol - visibleWidth + 1
      else t.scrollCol
    else t.scrollCol

  { t with scrollRow := scrollRow, scrollCol := scrollCol }

end TextArea

instance : Widget TextArea where
  render t area buf := Id.run do
    -- Render block if present
    let mut result := match t.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get content area
    let contentArea := match t.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty then return result

    -- Calculate line number width
    let lineNumWidth := if t.showLineNumbers then t.lineNumberWidth else 0
    let textStartX := contentArea.x + lineNumWidth

    -- Calculate visible text area
    let textWidth := if contentArea.width > lineNumWidth then contentArea.width - lineNumWidth else 0
    let visibleHeight := contentArea.height

    -- Adjust scroll
    let scrollRow := if t.cursorRow < t.scrollRow then t.cursorRow
                     else if t.cursorRow >= t.scrollRow + visibleHeight then t.cursorRow - visibleHeight + 1
                     else t.scrollRow
    let scrollCol := if t.cursorCol < t.scrollCol then t.cursorCol
                     else if t.cursorCol >= t.scrollCol + textWidth then t.cursorCol - textWidth + 1
                     else t.scrollCol

    -- Render visible lines
    for row in [:visibleHeight] do
      let lineIdx := scrollRow + row
      let y := contentArea.y + row

      -- Render line number
      if t.showLineNumbers then
        let numStr := s!"{lineIdx + 1}"
        let paddedNum := String.ofList (List.replicate (t.lineNumberWidth - numStr.length - 1) ' ') ++ numStr ++ "│"
        let paddedNumChars := paddedNum.toList
        for hi : i in [:paddedNumChars.length] do
          let x := contentArea.x + i
          if x < textStartX then
            match paddedNumChars[i]? with
            | some c => result := result.setStyled x y c t.lineNumberStyle
            | none => pure ()

      -- Render line content
      match t.lines[lineIdx]? with
      | none => pure ()
      | some line =>
        let visibleLine := line.drop scrollCol |>.take textWidth
        let isCurrentLine := lineIdx == t.cursorRow

        -- Apply current line style if set
        let baseStyle := match (isCurrentLine, t.currentLineStyle) with
          | (true, some s) => s
          | _ => t.style

        let visibleLineChars := visibleLine.toList
        for hc : col in [:visibleLineChars.length] do
          let x := textStartX + col
          let isCursor := t.focused && isCurrentLine && (scrollCol + col) == t.cursorCol
          let style := if isCursor then t.cursorStyle else baseStyle
          match visibleLineChars[col]? with
          | some c => result := result.setStyled x y c style
          | none => pure ()

        -- Render cursor at end of line if needed
        if t.focused && isCurrentLine then
          let cursorScreenCol := t.cursorCol - scrollCol
          if cursorScreenCol >= 0 && cursorScreenCol >= visibleLine.length && cursorScreenCol < textWidth then
            let x := textStartX + cursorScreenCol
            result := result.setStyled x y ' ' t.cursorStyle

    result

end Terminus
