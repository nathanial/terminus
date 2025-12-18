-- Terminus.Widgets.Block: Container widget with borders and titles

import Terminus.Widgets.Widget

namespace Terminus

/-- Border style variants -/
inductive BorderType where
  | none
  | single
  | double
  | rounded
  | thick
  deriving Repr, BEq, Inhabited

/-- Border character set -/
structure BorderChars where
  topLeft : Char
  topRight : Char
  bottomLeft : Char
  bottomRight : Char
  horizontal : Char
  vertical : Char
  deriving Repr, Inhabited

namespace BorderChars

def single : BorderChars := {
  topLeft := '┌'
  topRight := '┐'
  bottomLeft := '└'
  bottomRight := '┘'
  horizontal := '─'
  vertical := '│'
}

def double : BorderChars := {
  topLeft := '╔'
  topRight := '╗'
  bottomLeft := '╚'
  bottomRight := '╝'
  horizontal := '═'
  vertical := '║'
}

def rounded : BorderChars := {
  topLeft := '╭'
  topRight := '╮'
  bottomLeft := '╰'
  bottomRight := '╯'
  horizontal := '─'
  vertical := '│'
}

def thick : BorderChars := {
  topLeft := '┏'
  topRight := '┓'
  bottomLeft := '┗'
  bottomRight := '┛'
  horizontal := '━'
  vertical := '┃'
}

def fromType : BorderType → BorderChars
  | .none => single -- Will not be drawn anyway
  | .single => single
  | .double => double
  | .rounded => rounded
  | .thick => thick

end BorderChars

/-- Title position -/
inductive TitlePosition where
  | topLeft
  | topCenter
  | topRight
  | bottomLeft
  | bottomCenter
  | bottomRight
  deriving Repr, BEq, Inhabited

/-- Block widget - a container with optional border and title -/
structure Block where
  title : Option String := none
  titlePosition : TitlePosition := .topLeft
  titleStyle : Style := {}
  borderType : BorderType := .single
  borderStyle : Style := {}
  style : Style := {} -- Background style for the content area
  deriving Repr, Inhabited

namespace Block

def new : Block := {}

def withTitle (b : Block) (t : String) : Block := { b with title := some t }
def withTitlePos (b : Block) (p : TitlePosition) : Block := { b with titlePosition := p }
def withTitleStyle (b : Block) (s : Style) : Block := { b with titleStyle := s }
def withBorder (b : Block) (t : BorderType) : Block := { b with borderType := t }
def withBorderStyle (b : Block) (s : Style) : Block := { b with borderStyle := s }
def withStyle (b : Block) (s : Style) : Block := { b with style := s }

def single : Block := { borderType := .single }
def double : Block := { borderType := .double }
def rounded : Block := { borderType := .rounded }
def thick : Block := { borderType := .thick }
def bordered (t : BorderType) : Block := { borderType := t }

/-- Get the inner area after accounting for borders -/
def innerArea (b : Block) (area : Rect) : Rect :=
  if b.borderType == .none then area else area.innerBorder

/-- Render the block borders and title -/
private def renderBorder (b : Block) (area : Rect) (buf : Buffer) : Buffer := Id.run do
  if b.borderType == .none || area.width < 2 || area.height < 2 then
    return buf

  let chars := BorderChars.fromType b.borderType
  let style := b.borderStyle
  let mut result := buf

  -- Draw corners
  result := result.setStyled area.x area.y chars.topLeft style
  result := result.setStyled (area.x + area.width - 1) area.y chars.topRight style
  result := result.setStyled area.x (area.y + area.height - 1) chars.bottomLeft style
  result := result.setStyled (area.x + area.width - 1) (area.y + area.height - 1) chars.bottomRight style

  -- Draw horizontal borders
  for x in [area.x + 1 : area.x + area.width - 1] do
    result := result.setStyled x area.y chars.horizontal style
    result := result.setStyled x (area.y + area.height - 1) chars.horizontal style

  -- Draw vertical borders
  for y in [area.y + 1 : area.y + area.height - 1] do
    result := result.setStyled area.x y chars.vertical style
    result := result.setStyled (area.x + area.width - 1) y chars.vertical style

  result

private def renderTitle (b : Block) (area : Rect) (buf : Buffer) : Buffer := Id.run do
  match b.title with
  | none => return buf
  | some title =>
    if area.width < 4 then return buf -- Not enough space for title

    let maxLen := area.width - 4 -- Leave space for borders and padding
    let displayTitle := if title.length > maxLen then title.take maxLen else title
    let titleWithPad := " " ++ displayTitle ++ " "

    let (y, x) := match b.titlePosition with
      | .topLeft => (area.y, area.x + 1)
      | .topCenter => (area.y, area.x + (area.width - titleWithPad.length) / 2)
      | .topRight => (area.y, area.x + area.width - titleWithPad.length - 1)
      | .bottomLeft => (area.y + area.height - 1, area.x + 1)
      | .bottomCenter => (area.y + area.height - 1, area.x + (area.width - titleWithPad.length) / 2)
      | .bottomRight => (area.y + area.height - 1, area.x + area.width - titleWithPad.length - 1)

    buf.writeString x y titleWithPad b.titleStyle

/-- Fill the inner area with the block's background style -/
private def renderBackground (b : Block) (area : Rect) (buf : Buffer) : Buffer := Id.run do
  if b.style == Style.default then return buf
  let inner := b.innerArea area
  let mut result := buf
  for y in [inner.y : inner.y + inner.height] do
    for x in [inner.x : inner.x + inner.width] do
      let cell := result.get x y
      result := result.set x y { cell with style := Style.merge b.style cell.style }
  result

end Block

instance : Widget Block where
  render b area buf :=
    buf
    |> Block.renderBorder b area
    |> Block.renderTitle b area
    |> Block.renderBackground b area

end Terminus
