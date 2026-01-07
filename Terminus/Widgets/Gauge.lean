-- Terminus.Widgets.Gauge: Progress bar widget

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Gauge (progress bar) widget -/
structure Gauge where
  ratio : Float := 0.0  -- 0.0 to 1.0
  label : Option String := none
  filledStyle : Style := { fg := .green }
  unfilledStyle : Style := { fg := .white }
  filledChar : Char := '█'
  unfilledChar : Char := '░'
  showPercent : Bool := true
  block : Option Block := none
  deriving Repr, Inhabited

namespace Gauge

def new (ratio : Float) : Gauge := { ratio := min 1.0 (max 0.0 ratio) }

def fromPercent (pct : Nat) : Gauge := { ratio := (min pct 100).toFloat / 100.0 }

def withLabel (g : Gauge) (l : String) : Gauge := { g with label := some l }
def withFilledStyle (g : Gauge) (s : Style) : Gauge := { g with filledStyle := s }
def withUnfilledStyle (g : Gauge) (s : Style) : Gauge := { g with unfilledStyle := s }
def withFilledChar (g : Gauge) (c : Char) : Gauge := { g with filledChar := c }
def withUnfilledChar (g : Gauge) (c : Char) : Gauge := { g with unfilledChar := c }
def withBlock (g : Gauge) (b : Block) : Gauge := { g with block := some b }
def hidePercent (g : Gauge) : Gauge := { g with showPercent := false }

def setRatio (g : Gauge) (r : Float) : Gauge := { g with ratio := min 1.0 (max 0.0 r) }
def setPercent (g : Gauge) (pct : Nat) : Gauge := g.setRatio ((min pct 100).toFloat / 100.0)

/-- Get the percentage as an integer 0-100 -/
def percent (g : Gauge) : Nat := (g.ratio * 100.0).toUInt32.toNat

end Gauge

instance : Widget Gauge where
  render g area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner g.block area buf
    if contentArea.isEmpty || contentArea.height == 0 then return buf'
    let mut result := buf'

    -- Calculate label text
    let labelText := match g.label with
      | some l =>
        if g.showPercent then s!"{l} {g.percent}%"
        else l
      | none =>
        if g.showPercent then s!"{g.percent}%"
        else ""

    -- Calculate gauge bar width (leave space for label if needed)
    let labelWidth := labelText.length
    let barWidth := if labelWidth > 0 && contentArea.width > labelWidth + 1
                    then contentArea.width - labelWidth - 1
                    else contentArea.width

    -- Calculate filled portion
    let filledWidth := (g.ratio * barWidth.toFloat).toUInt32.toNat
    let unfilledWidth := if barWidth > filledWidth then barWidth - filledWidth else 0

    -- Render on the middle row if height > 1
    let y := contentArea.y + contentArea.height / 2

    -- Render filled portion
    for x in [contentArea.x : contentArea.x + filledWidth] do
      result := result.setStyled x y g.filledChar g.filledStyle

    -- Render unfilled portion
    for x in [contentArea.x + filledWidth : contentArea.x + filledWidth + unfilledWidth] do
      result := result.setStyled x y g.unfilledChar g.unfilledStyle

    -- Render label at the end
    if labelWidth > 0 && contentArea.width > barWidth then
      let labelX := contentArea.x + barWidth + 1
      result := result.writeStringBounded labelX y labelWidth labelText {}

    result

/-- Vertical gauge variant -/
structure VGauge where
  ratio : Float := 0.0
  filledStyle : Style := { fg := .green }
  unfilledStyle : Style := { fg := .white }
  filledChar : Char := '█'
  unfilledChar : Char := '░'
  block : Option Block := none
  deriving Repr, Inhabited

namespace VGauge

def new (ratio : Float) : VGauge := { ratio := min 1.0 (max 0.0 ratio) }
def fromPercent (pct : Nat) : VGauge := { ratio := (min pct 100).toFloat / 100.0 }
def setRatio (g : VGauge) (r : Float) : VGauge := { g with ratio := min 1.0 (max 0.0 r) }

end VGauge

instance : Widget VGauge where
  render g area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner g.block area buf
    if contentArea.isEmpty then return buf'
    let mut result := buf'

    let filledHeight := (g.ratio * contentArea.height.toFloat).toUInt32.toNat
    let startFilledY := contentArea.y + contentArea.height - filledHeight

    -- Render from bottom to top (filled at bottom)
    for y in [contentArea.y : contentArea.y + contentArea.height] do
      let char := if y >= startFilledY then g.filledChar else g.unfilledChar
      let style := if y >= startFilledY then g.filledStyle else g.unfilledStyle
      for x in [contentArea.x : contentArea.x + contentArea.width] do
        result := result.setStyled x y char style

    result

end Terminus
