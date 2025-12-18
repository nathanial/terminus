-- Terminus.Widgets.BarChart: Vertical or horizontal bar chart

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- A single bar in the chart -/
structure BarData where
  label : String
  value : Float
  style : Option Style := none
  deriving Repr, Inhabited

namespace BarData

def new (label : String) (value : Float) : BarData := { label, value }
def withStyle (b : BarData) (s : Style) : BarData := { b with style := some s }

end BarData

/-- Bar chart widget -/
structure BarChart where
  data : List BarData := []
  orientation : Orientation := .vertical
  barWidth : Nat := 3
  gap : Nat := 1
  showValues : Bool := true
  showLabels : Bool := true
  maxValue : Option Float := none
  labelStyle : Style := Style.default
  valueStyle : Style := Style.dim
  defaultBarStyle : Style := Style.fgColor Color.blue
  block : Option Block := none
  deriving Repr, Inhabited

namespace BarChart

def new (data : List BarData) : BarChart := { data }

def fromPairs (pairs : List (String × Float)) : BarChart :=
  { data := pairs.map fun (l, v) => BarData.new l v }

def vertical : BarChart := { orientation := .vertical }
def horizontal : BarChart := { orientation := .horizontal }

def withData (c : BarChart) (data : List BarData) : BarChart := { c with data := data }
def withOrientation (c : BarChart) (o : Orientation) : BarChart := { c with orientation := o }
def withBarWidth (c : BarChart) (w : Nat) : BarChart := { c with barWidth := w }
def withGap (c : BarChart) (g : Nat) : BarChart := { c with gap := g }
def withMaxValue (c : BarChart) (m : Float) : BarChart := { c with maxValue := some m }
def withLabelStyle (c : BarChart) (s : Style) : BarChart := { c with labelStyle := s }
def withValueStyle (c : BarChart) (s : Style) : BarChart := { c with valueStyle := s }
def withDefaultBarStyle (c : BarChart) (s : Style) : BarChart := { c with defaultBarStyle := s }
def withBlock (c : BarChart) (b : Block) : BarChart := { c with block := some b }
def hideValues (c : BarChart) : BarChart := { c with showValues := false }
def hideLabels (c : BarChart) : BarChart := { c with showLabels := false }

/-- Calculate the maximum value from data -/
def computeMax (c : BarChart) : Float :=
  match c.maxValue with
  | some m => m
  | none => c.data.foldl (fun acc d => if d.value > acc then d.value else acc) 0.0

/-- Get bar style (custom or default) -/
def getBarStyle (c : BarChart) (bar : BarData) : Style :=
  bar.style.getD c.defaultBarStyle

end BarChart

/-- Vertical bar chart rendering -/
private def renderVerticalBarChart (c : BarChart) (area : Rect) (buf : Buffer) : Buffer := Id.run do
  if area.isEmpty || c.data.isEmpty then return buf

  let mut result := buf
  let maxVal := c.computeMax

  -- Reserve space for labels at bottom (1 row)
  let labelHeight := if c.showLabels then 1 else 0
  let chartHeight := if area.height > labelHeight then area.height - labelHeight else 0
  if chartHeight == 0 then return result

  -- Calculate available width per bar
  let numBars := c.data.length
  let totalBarSpace := numBars * c.barWidth + (numBars - 1) * c.gap
  let startX := area.x + (area.width - totalBarSpace) / 2

  for hi : i in [:c.data.length] do
    match c.data[i]? with
    | some bar =>
      let barX := startX + i * (c.barWidth + c.gap)
      let barStyle := c.getBarStyle bar

      -- Calculate bar height
      let normalizedValue := if maxVal > 0.0 then bar.value / maxVal else 0.0
      let barHeight := (normalizedValue * Float.ofNat chartHeight).toUInt32.toNat
      let barStartY := area.y + chartHeight - barHeight

      -- Draw the bar (from bottom up)
      for y in [barStartY : area.y + chartHeight] do
        for dx in [:c.barWidth] do
          let x := barX + dx
          if x < area.x + area.width then
            result := result.setStyled x y '█' barStyle

      -- Draw value above bar if requested
      if c.showValues && barHeight > 0 then
        let valStr := s!"{bar.value.toUInt32}"
        let valX := barX + (c.barWidth - valStr.length) / 2
        let valY := if barStartY > area.y then barStartY - 1 else area.y
        let valChars := valStr.toList
        for hj : j in [:valChars.length] do
          if valX + j < area.x + area.width then
            match valChars[j]? with
            | some ch => result := result.setStyled (valX + j) valY ch c.valueStyle
            | none => pure ()

      -- Draw label at bottom
      if c.showLabels then
        let labelY := area.y + chartHeight
        let labelX := barX + (c.barWidth - bar.label.length) / 2
        let labelChars := bar.label.take c.barWidth |>.toList
        for hj : j in [:labelChars.length] do
          if labelX + j >= area.x && labelX + j < area.x + area.width then
            match labelChars[j]? with
            | some ch => result := result.setStyled (labelX + j) labelY ch c.labelStyle
            | none => pure ()
    | none => pure ()

  result

/-- Horizontal bar chart rendering -/
private def renderHorizontalBarChart (c : BarChart) (area : Rect) (buf : Buffer) : Buffer := Id.run do
  if area.isEmpty || c.data.isEmpty then return buf

  let mut result := buf
  let maxVal := c.computeMax

  -- Calculate max label width
  let maxLabelWidth := c.data.foldl (fun acc d => Nat.max acc d.label.length) 0
  let labelWidth := if c.showLabels then Nat.min maxLabelWidth 10 else 0

  -- Calculate available chart width
  let chartStartX := area.x + labelWidth + (if labelWidth > 0 then 1 else 0)
  let chartWidth := if area.width > labelWidth + 1 then area.width - labelWidth - 1 else 0
  if chartWidth == 0 then return result

  -- Calculate bar height (each bar takes barWidth rows)
  let numBars := c.data.length
  let totalBarSpace := numBars * c.barWidth + (numBars - 1) * c.gap

  let startY := area.y + (area.height - totalBarSpace) / 2

  for hi : i in [:c.data.length] do
    match c.data[i]? with
    | some bar =>
      let barY := startY + i * (c.barWidth + c.gap)
      let barStyle := c.getBarStyle bar

      -- Calculate bar width
      let normalizedValue := if maxVal > 0.0 then bar.value / maxVal else 0.0
      let barLen := (normalizedValue * Float.ofNat chartWidth).toUInt32.toNat

      -- Draw label
      if c.showLabels then
        let labelStr := bar.label.take labelWidth
        let labelX := area.x + labelWidth - labelStr.length
        let labelChars := labelStr.toList
        for hj : j in [:labelChars.length] do
          match labelChars[j]? with
          | some ch => result := result.setStyled (labelX + j) barY ch c.labelStyle
          | none => pure ()

      -- Draw the bar
      for x in [chartStartX : chartStartX + barLen] do
        for dy in [:c.barWidth] do
          let y := barY + dy
          if y < area.y + area.height then
            result := result.setStyled x y '█' barStyle

      -- Draw value at end of bar if requested
      if c.showValues && barLen > 0 then
        let valStr := s!" {bar.value.toUInt32}"
        let valX := chartStartX + barLen
        let valChars := valStr.toList
        for hj : j in [:valChars.length] do
          if valX + j < area.x + area.width then
            match valChars[j]? with
            | some ch => result := result.setStyled (valX + j) barY ch c.valueStyle
            | none => pure ()
    | none => pure ()

  result

instance : Widget BarChart where
  render c area buf := Id.run do
    -- Render block if present
    let mut result := match c.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get content area
    let contentArea := match c.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty then return result

    match c.orientation with
    | .vertical => renderVerticalBarChart c contentArea result
    | .horizontal => renderHorizontalBarChart c contentArea result

end Terminus
