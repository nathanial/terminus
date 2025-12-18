-- Terminus.Widgets.TextInput: Single-line text input field

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Input.Key

namespace Terminus

/-- Single-line text input widget -/
structure TextInput where
  value : String := ""
  cursor : Nat := 0
  scrollOffset : Nat := 0  -- For horizontal scrolling when text exceeds width
  placeholder : String := ""
  maxLength : Option Nat := none
  mask : Option Char := none  -- For password fields
  focused : Bool := true
  style : Style := Style.default
  cursorStyle : Style := Style.reversed
  placeholderStyle : Style := Style.dim
  block : Option Block := none
  deriving Repr, Inhabited

namespace TextInput

def new : TextInput := {}
def withValue (t : TextInput) (v : String) : TextInput := { t with value := v, cursor := v.length }
def withPlaceholder (t : TextInput) (p : String) : TextInput := { t with placeholder := p }
def withMaxLength (t : TextInput) (m : Nat) : TextInput := { t with maxLength := some m }
def withMask (t : TextInput) (c : Char) : TextInput := { t with mask := some c }
def password : TextInput := { mask := some '•' }

def withStyle (t : TextInput) (s : Style) : TextInput := { t with style := s }
def withCursorStyle (t : TextInput) (s : Style) : TextInput := { t with cursorStyle := s }
def withPlaceholderStyle (t : TextInput) (s : Style) : TextInput := { t with placeholderStyle := s }
def withBlock (t : TextInput) (b : Block) : TextInput := { t with block := some b }

def focus (t : TextInput) : TextInput := { t with focused := true }
def blur (t : TextInput) : TextInput := { t with focused := false }
def toggleFocus (t : TextInput) : TextInput := { t with focused := !t.focused }

/-- Get the display text (masked if password mode) -/
def displayText (t : TextInput) : String :=
  match t.mask with
  | some c => String.ofList (t.value.toList.map fun _ => c)
  | none => t.value

/-- Check if at max length -/
def atMaxLength (t : TextInput) : Bool :=
  match t.maxLength with
  | some m => t.value.length >= m
  | none => false

/-- Insert a character at cursor position -/
def insertChar (t : TextInput) (c : Char) : TextInput :=
  if t.atMaxLength then t
  else
    let before := t.value.take t.cursor
    let after := t.value.drop t.cursor
    { t with
      value := before ++ String.singleton c ++ after
      cursor := t.cursor + 1
    }

/-- Insert a string at cursor position -/
def insertString (t : TextInput) (s : String) : TextInput :=
  s.toList.foldl insertChar t

/-- Delete character before cursor (backspace) -/
def deleteBackward (t : TextInput) : TextInput :=
  if t.cursor == 0 then t
  else
    let before := t.value.take (t.cursor - 1)
    let after := t.value.drop t.cursor
    { t with
      value := before ++ after
      cursor := t.cursor - 1
    }

/-- Delete character at cursor (delete key) -/
def deleteForward (t : TextInput) : TextInput :=
  if t.cursor >= t.value.length then t
  else
    let before := t.value.take t.cursor
    let after := t.value.drop (t.cursor + 1)
    { t with value := before ++ after }

/-- Move cursor left -/
def moveCursorLeft (t : TextInput) : TextInput :=
  if t.cursor > 0 then { t with cursor := t.cursor - 1 }
  else t

/-- Move cursor right -/
def moveCursorRight (t : TextInput) : TextInput :=
  if t.cursor < t.value.length then { t with cursor := t.cursor + 1 }
  else t

/-- Move cursor to start -/
def moveCursorStart (t : TextInput) : TextInput :=
  { t with cursor := 0 }

/-- Move cursor to end -/
def moveCursorEnd (t : TextInput) : TextInput :=
  { t with cursor := t.value.length }

/-- Clear all text -/
def clear (t : TextInput) : TextInput :=
  { t with value := "", cursor := 0, scrollOffset := 0 }

/-- Set the value and move cursor to end -/
def setValue (t : TextInput) (v : String) : TextInput :=
  let value := match t.maxLength with
    | some m => v.take m
    | none => v
  { t with value := value, cursor := value.length }

/-- Handle a key event -/
def handleKey (t : TextInput) (key : KeyEvent) : TextInput :=
  if !t.focused then t
  else match key.code with
  | .char c => t.insertChar c
  | .backspace => t.deleteBackward
  | .delete => t.deleteForward
  | .left => t.moveCursorLeft
  | .right => t.moveCursorRight
  | .home => t.moveCursorStart
  | .«end» => t.moveCursorEnd
  | _ => t

/-- Adjust scroll offset to keep cursor visible -/
def adjustScroll (t : TextInput) (visibleWidth : Nat) : TextInput :=
  if visibleWidth == 0 then t
  else if t.cursor < t.scrollOffset then
    { t with scrollOffset := t.cursor }
  else if t.cursor >= t.scrollOffset + visibleWidth then
    { t with scrollOffset := t.cursor - visibleWidth + 1 }
  else t

end TextInput

instance : Widget TextInput where
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

    -- Calculate visible width
    let visibleWidth := contentArea.width
    let y := contentArea.y + contentArea.height / 2

    -- Determine what to display
    let isEmpty := t.value.isEmpty
    let displayStr := if isEmpty then t.placeholder else t.displayText
    let style := if isEmpty then t.placeholderStyle else t.style

    -- Adjust scroll offset
    let scrollOffset := if t.cursor < t.scrollOffset then t.cursor
                        else if t.cursor >= t.scrollOffset + visibleWidth then t.cursor - visibleWidth + 1
                        else t.scrollOffset

    -- Get visible portion of text
    let visibleText := displayStr.drop scrollOffset |>.take visibleWidth

    -- Render text
    let visibleChars := visibleText.toList
    for hi : i in [:visibleChars.length] do
      let x := contentArea.x + i
      let cursorPos := t.cursor - scrollOffset
      let isCursor := t.focused && !isEmpty && i == cursorPos

      match visibleChars[i]? with
      | some c =>
        let charStyle := if isCursor then t.cursorStyle else style
        result := result.setStyled x y c charStyle
      | none => pure ()

    -- Render cursor at end if cursor is past text
    if t.focused && !isEmpty then
      let cursorPos := t.cursor - scrollOffset
      if cursorPos >= visibleText.length && cursorPos < visibleWidth then
        let x := contentArea.x + cursorPos
        result := result.setStyled x y ' ' t.cursorStyle

    -- Render cursor for empty input with placeholder
    if t.focused && isEmpty then
      result := result.setStyled contentArea.x y ' ' t.cursorStyle

    result

end Terminus
