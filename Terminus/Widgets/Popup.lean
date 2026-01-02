-- Terminus.Widgets.Popup: Centered overlay dialog box

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Widgets.Paragraph

namespace Terminus

/-- Popup widget - a centered dialog box overlay -/
structure Popup where
  title : Option String := none
  lines : List String := []
  width : Option Nat := none   -- Auto-size if none
  height : Option Nat := none  -- Auto-size if none
  block : Block := Block.double
  contentStyle : Style := Style.default
  titleStyle : Style := Style.bold
  clearBackground : Bool := true
  backgroundChar : Char := ' '
  deriving Repr, Inhabited

namespace Popup

def new (content : String) : Popup := {
  lines := content.splitOn "\n"
}

def fromLines (lines : List String) : Popup := { lines := lines }

def withTitle (p : Popup) (t : String) : Popup :=
  { p with title := some t, block := p.block.withTitle t }

def withTitleStyle (p : Popup) (s : Style) : Popup :=
  { p with titleStyle := s, block := p.block.withTitleStyle s }

def withWidth (p : Popup) (w : Nat) : Popup := { p with width := some w }
def withHeight (p : Popup) (h : Nat) : Popup := { p with height := some h }
def withSize (p : Popup) (w h : Nat) : Popup := { p with width := some w, height := some h }

def withBlock (p : Popup) (b : Block) : Popup := { p with block := b }
def withContentStyle (p : Popup) (s : Style) : Popup := { p with contentStyle := s }
def withBorderStyle (p : Popup) (s : Style) : Popup := { p with block := p.block.withBorderStyle s }

def noClear (p : Popup) : Popup := { p with clearBackground := false }

/-- Calculate the optimal size based on content -/
def computeSize (p : Popup) (maxWidth maxHeight : Nat) : (Nat × Nat) :=
  let contentWidth := p.lines.foldl (fun acc line => Nat.max acc line.length) 0
  let contentHeight := p.lines.length

  -- Add 2 for border on each side
  let borderWidth := 2
  let borderHeight := 2

  let idealWidth := contentWidth + borderWidth + 2  -- +2 for padding
  let idealHeight := contentHeight + borderHeight

  let w := match p.width with
    | some w => Nat.min w maxWidth
    | none => Nat.min idealWidth maxWidth

  let h := match p.height with
    | some h => Nat.min h maxHeight
    | none => Nat.min idealHeight maxHeight

  (w, h)

/-- Calculate centered position within parent area -/
def computePosition (p : Popup) (parentArea : Rect) : Rect :=
  let (w, h) := p.computeSize parentArea.width parentArea.height
  let x := parentArea.x + (parentArea.width - w) / 2
  let y := parentArea.y + (parentArea.height - h) / 2
  Rect.new x y w h

end Popup

instance : Widget Popup where
  render p parentArea buf := Id.run do
    if parentArea.isEmpty then return buf

    let mut result := buf

    -- Calculate popup position and size
    let popupArea := p.computePosition parentArea

    -- Clear background if requested
    if p.clearBackground then
      for y in [popupArea.y : popupArea.y + popupArea.height] do
        for x in [popupArea.x : popupArea.x + popupArea.width] do
          result := result.setStyled x y p.backgroundChar {}

    -- Render the block border
    result := Widget.render p.block popupArea result

    -- Get content area inside block
    let contentArea := p.block.innerArea popupArea
    if contentArea.isEmpty then return result

    -- Render content lines centered
    let startY := contentArea.y
    for i in [:p.lines.length] do
      let y := startY + i
      if y >= contentArea.y + contentArea.height then break
      match p.lines[i]? with
      | some line =>
        -- Center the line horizontally
        let xOffset := if contentArea.width > line.length
                       then (contentArea.width - line.length) / 2
                       else 0
        let x := contentArea.x + xOffset

        -- Render the line
        for j in [:line.length] do
          let cx := x + j
          if cx < contentArea.x + contentArea.width then
            match line.toList[j]? with
            | some c => result := result.setStyled cx y c p.contentStyle
            | none => pure ()
      | none => pure ()

    result

/-- Confirmation popup with Yes/No buttons -/
structure ConfirmPopup where
  message : String
  yesLabel : String := "Yes"
  noLabel : String := "No"
  selectedYes : Bool := true
  block : Block := Block.double.withTitle "Confirm"
  messageStyle : Style := Style.default
  selectedStyle : Style := Style.reversed
  unselectedStyle : Style := Style.default
  deriving Repr, Inhabited

namespace ConfirmPopup

def new (message : String) : ConfirmPopup := { message := message }

def withYesLabel (p : ConfirmPopup) (l : String) : ConfirmPopup := { p with yesLabel := l }
def withNoLabel (p : ConfirmPopup) (l : String) : ConfirmPopup := { p with noLabel := l }
def selectYes (p : ConfirmPopup) : ConfirmPopup := { p with selectedYes := true }
def selectNo (p : ConfirmPopup) : ConfirmPopup := { p with selectedYes := false }
def toggle (p : ConfirmPopup) : ConfirmPopup := { p with selectedYes := !p.selectedYes }

end ConfirmPopup

instance : Widget ConfirmPopup where
  render p parentArea buf := Id.run do
    if parentArea.isEmpty then return buf

    -- Calculate size: message + buttons
    let buttonLine := s!"  [{p.yesLabel}]  [{p.noLabel}]  "
    let contentWidth := Nat.max p.message.length buttonLine.length
    let contentHeight := 3  -- message + blank + buttons

    let popupWidth := contentWidth + 4  -- borders + padding
    let popupHeight := contentHeight + 2  -- borders

    let x := parentArea.x + (parentArea.width - popupWidth) / 2
    let y := parentArea.y + (parentArea.height - popupHeight) / 2
    let popupArea := Rect.new x y popupWidth popupHeight

    let mut result := buf

    -- Clear background
    for py in [popupArea.y : popupArea.y + popupArea.height] do
      for px in [popupArea.x : popupArea.x + popupArea.width] do
        result := result.setStyled px py ' ' {}

    -- Render block
    result := Widget.render p.block popupArea result

    let contentArea := p.block.innerArea popupArea
    if contentArea.isEmpty then return result

    -- Render message (centered)
    let msgX := contentArea.x + (contentArea.width - p.message.length) / 2
    let msgChars := p.message.toList
    for i in [:msgChars.length] do
      match msgChars[i]? with
      | some c => result := result.setStyled (msgX + i) contentArea.y c p.messageStyle
      | none => pure ()

    -- Render buttons on last line
    let buttonY := contentArea.y + contentArea.height - 1
    let yesStyle := if p.selectedYes then p.selectedStyle else p.unselectedStyle
    let noStyle := if p.selectedYes then p.unselectedStyle else p.selectedStyle

    let yesText := s!"[{p.yesLabel}]"
    let noText := s!"[{p.noLabel}]"
    let totalButtonWidth := yesText.length + 4 + noText.length
    let buttonStartX := contentArea.x + (contentArea.width - totalButtonWidth) / 2

    -- Render Yes button
    let yesChars := yesText.toList
    for i in [:yesChars.length] do
      match yesChars[i]? with
      | some c => result := result.setStyled (buttonStartX + i) buttonY c yesStyle
      | none => pure ()

    -- Render No button
    let noStartX := buttonStartX + yesText.length + 4
    let noChars := noText.toList
    for i in [:noChars.length] do
      match noChars[i]? with
      | some c => result := result.setStyled (noStartX + i) buttonY c noStyle
      | none => pure ()

    result

end Terminus
