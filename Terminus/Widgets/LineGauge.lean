-- Terminus.Widgets.LineGauge: Thin line-style progress bar widget

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- LineGauge - a thin horizontal progress bar using line characters -/
structure LineGauge where
  ratio : Float := 0.0  -- 0.0 to 1.0
  label : Option String := none
  filledChar : Char := '━'      -- Thick horizontal line
  unfilledChar : Char := '─'    -- Thin horizontal line
  filledStyle : Style := { fg := .green }
  unfilledStyle : Style := { fg := .gray }
  labelStyle : Style := {}
  showPercent : Bool := false
  block : Option Block := none
  deriving Repr, Inhabited

namespace LineGauge

def new (ratio : Float) : LineGauge := { ratio := min 1.0 (max 0.0 ratio) }

def fromPercent (pct : Nat) : LineGauge := { ratio := (min pct 100).toFloat / 100.0 }

def withLabel (g : LineGauge) (l : String) : LineGauge := { g with label := some l }
def withFilledChar (g : LineGauge) (c : Char) : LineGauge := { g with filledChar := c }
def withUnfilledChar (g : LineGauge) (c : Char) : LineGauge := { g with unfilledChar := c }
def withFilledStyle (g : LineGauge) (s : Style) : LineGauge := { g with filledStyle := s }
def withUnfilledStyle (g : LineGauge) (s : Style) : LineGauge := { g with unfilledStyle := s }
def withLabelStyle (g : LineGauge) (s : Style) : LineGauge := { g with labelStyle := s }
def withBlock (g : LineGauge) (b : Block) : LineGauge := { g with block := some b }
def withShowPercent (g : LineGauge) (enabled : Bool := true) : LineGauge := { g with showPercent := enabled }

def setRatio (g : LineGauge) (r : Float) : LineGauge := { g with ratio := min 1.0 (max 0.0 r) }
def setPercent (g : LineGauge) (pct : Nat) : LineGauge := g.setRatio ((min pct 100).toFloat / 100.0)

/-- Get the percentage as an integer 0-100 -/
def percent (g : LineGauge) : Nat := (g.ratio * 100.0).toUInt32.toNat

end LineGauge

instance : Widget LineGauge where
  render g area buf := Id.run do
    -- Render block if present
    let mut result := match g.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get content area
    let contentArea := match g.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty || contentArea.height == 0 then return result

    -- Calculate label text
    let labelText := match g.label with
      | some l =>
        if g.showPercent then s!"{l} {g.percent}%"
        else l
      | none =>
        if g.showPercent then s!"{g.percent}%"
        else ""

    -- Calculate label width (label goes at the start, bar follows)
    let labelWidth := labelText.length
    let labelOffset := if labelWidth > 0 then labelWidth + 1 else 0

    -- Calculate bar width (after label)
    let barWidth := if contentArea.width > labelOffset
                    then contentArea.width - labelOffset
                    else 0

    if barWidth == 0 then return result

    -- Calculate filled portion
    let filledWidth := (g.ratio * barWidth.toFloat).toUInt32.toNat
    let unfilledWidth := if barWidth > filledWidth then barWidth - filledWidth else 0

    -- Render on the middle row if height > 1
    let y := contentArea.y + contentArea.height / 2

    -- Render label at the start
    if labelWidth > 0 then
      result := result.writeStringBounded contentArea.x y labelWidth labelText g.labelStyle

    -- Start position for the bar (after label)
    let barStartX := contentArea.x + labelOffset

    -- Render filled portion
    for x in [barStartX : barStartX + filledWidth] do
      result := result.setStyled x y g.filledChar g.filledStyle

    -- Render unfilled portion
    for x in [barStartX + filledWidth : barStartX + filledWidth + unfilledWidth] do
      result := result.setStyled x y g.unfilledChar g.unfilledStyle

    result

end Terminus
