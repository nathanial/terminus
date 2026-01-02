-- Terminus.Widgets.LineChart: Line graph with axes and labels

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Widgets.Canvas

namespace Terminus

/-- A data series for the line chart -/
structure DataSeries where
  data : List Float := []
  label : String := ""
  style : Style := Style.default
  marker : Option Char := none  -- Optional marker character at data points
  deriving Repr, Inhabited

namespace DataSeries

def new (data : List Float) : DataSeries := { data }
def withLabel (s : DataSeries) (l : String) : DataSeries := { s with label := l }
def withStyle (s : DataSeries) (st : Style) : DataSeries := { s with style := st }
def withMarker (s : DataSeries) (c : Char) : DataSeries := { s with marker := some c }

def min (s : DataSeries) : Float := s.data.foldl (fun a b => if b < a then b else a) 1.0e38
def max (s : DataSeries) : Float := s.data.foldl (fun a b => if b > a then b else a) (-1.0e38)

end DataSeries

/-- Line chart widget -/
structure LineChart where
  series : List DataSeries := []
  xLabels : List String := []
  yMin : Option Float := none
  yMax : Option Float := none
  showLegend : Bool := true
  showGrid : Bool := false
  showYAxis : Bool := true
  showXAxis : Bool := true
  yAxisWidth : Nat := 6  -- Width reserved for Y-axis labels
  xAxisHeight : Nat := 1  -- Height reserved for X-axis labels
  axisStyle : Style := Style.dim
  gridStyle : Style := Style.dim
  legendStyle : Style := Style.default
  block : Option Block := none
  deriving Repr, Inhabited

namespace LineChart

def new (series : List DataSeries := []) : LineChart := { series }

def withSeries (c : LineChart) (s : List DataSeries) : LineChart := { c with series := s }
def addSeries (c : LineChart) (s : DataSeries) : LineChart := { c with series := c.series ++ [s] }
def withXLabels (c : LineChart) (labels : List String) : LineChart := { c with xLabels := labels }
def withYMin (c : LineChart) (m : Float) : LineChart := { c with yMin := some m }
def withYMax (c : LineChart) (m : Float) : LineChart := { c with yMax := some m }
def withYRange (c : LineChart) (min max : Float) : LineChart := { c with yMin := some min, yMax := some max }

def withShowLegend (c : LineChart) (b : Bool) : LineChart := { c with showLegend := b }
def withShowGrid (c : LineChart) (b : Bool) : LineChart := { c with showGrid := b }
def withShowYAxis (c : LineChart) (b : Bool) : LineChart := { c with showYAxis := b }
def withShowXAxis (c : LineChart) (b : Bool) : LineChart := { c with showXAxis := b }

def withAxisStyle (c : LineChart) (s : Style) : LineChart := { c with axisStyle := s }
def withGridStyle (c : LineChart) (s : Style) : LineChart := { c with gridStyle := s }
def withLegendStyle (c : LineChart) (s : Style) : LineChart := { c with legendStyle := s }
def withBlock (c : LineChart) (b : Block) : LineChart := { c with block := some b }

def hideLegend (c : LineChart) : LineChart := c.withShowLegend false
def showGridLines (c : LineChart) : LineChart := c.withShowGrid true

/-- Compute the Y-axis range from all series -/
def computeYRange (c : LineChart) : (Float × Float) :=
  let allMin := c.series.foldl (fun acc s => if s.min < acc then s.min else acc) 1.0e38
  let allMax := c.series.foldl (fun acc s => if s.max > acc then s.max else acc) (-1.0e38)

  let yMin := c.yMin.getD allMin
  let yMax := c.yMax.getD allMax

  -- Add some padding
  let range := yMax - yMin
  let padding := if range > 0 then range * 0.1 else 1.0
  (yMin - padding, yMax + padding)

/-- Get the maximum data length across all series -/
def maxDataLength (c : LineChart) : Nat :=
  c.series.foldl (fun acc s => Nat.max acc s.data.length) 0

end LineChart

instance : Widget LineChart where
  render c area buf := Id.run do
    -- Render block if present
    let mut result := match c.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get content area
    let contentArea := match c.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty || contentArea.height < 3 then return result

    -- Calculate chart area (excluding axes and legend)
    let yAxisWidth := if c.showYAxis then c.yAxisWidth else 0
    let xAxisHeight := if c.showXAxis then c.xAxisHeight else 0
    let legendHeight := if c.showLegend && !c.series.isEmpty then 1 else 0

    let chartX := contentArea.x + yAxisWidth
    let chartY := contentArea.y
    let chartWidth := if contentArea.width > yAxisWidth then contentArea.width - yAxisWidth else 0
    let chartHeight := if contentArea.height > xAxisHeight + legendHeight
                       then contentArea.height - xAxisHeight - legendHeight
                       else 0

    if chartWidth == 0 || chartHeight == 0 then return result

    let (yMin, yMax) := c.computeYRange
    let yRange := yMax - yMin

    -- Draw Y-axis
    if c.showYAxis then
      let axisX := contentArea.x + yAxisWidth - 1

      -- Draw vertical line
      for row in [:chartHeight] do
        let y := chartY + row
        result := result.setStyled axisX y '│' c.axisStyle

      -- Draw Y-axis labels (top, middle, bottom)
      let topLabel := s!"{yMax.toUInt32}"
      let midLabel := s!"{((yMax + yMin) / 2).toUInt32}"
      let botLabel := s!"{yMin.toUInt32}"

      -- Top label
      let topLabelX := contentArea.x + yAxisWidth - topLabel.length - 1
      let topLabelChars := topLabel.toList
      for i in [:topLabelChars.length] do
        if topLabelX + i >= contentArea.x then
          match topLabelChars[i]? with
          | some ch => result := result.setStyled (topLabelX + i) chartY ch c.axisStyle
          | none => pure ()

      -- Middle label
      if chartHeight > 2 then
        let midY := chartY + chartHeight / 2
        let midLabelX := contentArea.x + yAxisWidth - midLabel.length - 1
        let midLabelChars := midLabel.toList
        for i in [:midLabelChars.length] do
          if midLabelX + i >= contentArea.x then
            match midLabelChars[i]? with
            | some ch => result := result.setStyled (midLabelX + i) midY ch c.axisStyle
            | none => pure ()

      -- Bottom label
      let botY := chartY + chartHeight - 1
      let botLabelX := contentArea.x + yAxisWidth - botLabel.length - 1
      let botLabelChars := botLabel.toList
      for i in [:botLabelChars.length] do
        if botLabelX + i >= contentArea.x then
          match botLabelChars[i]? with
          | some ch => result := result.setStyled (botLabelX + i) botY ch c.axisStyle
          | none => pure ()

    -- Draw X-axis
    if c.showXAxis then
      let axisY := chartY + chartHeight

      -- Draw horizontal line
      for col in [:chartWidth] do
        let x := chartX + col
        result := result.setStyled x axisY '─' c.axisStyle

      -- Draw corner
      if c.showYAxis then
        result := result.setStyled (chartX - 1) axisY '└' c.axisStyle

      -- Draw X-axis labels if provided
      let numLabels := c.xLabels.length
      if numLabels > 0 && contentArea.height > chartHeight then
        let labelY := axisY + 1
        let spacing := chartWidth / Nat.max 1 (numLabels - 1)
        for i in [:c.xLabels.length] do
          match c.xLabels[i]? with
          | some label =>
            let labelX := chartX + i * spacing
            let labelChars := label.take 4 |>.toList
            for j in [:labelChars.length] do
              if labelX + j < contentArea.x + contentArea.width then
                match labelChars[j]? with
                | some ch => result := result.setStyled (labelX + j) labelY ch c.axisStyle
                | none => pure ()
          | none => pure ()

    -- Draw grid lines if enabled
    if c.showGrid && chartHeight > 2 then
      let gridY := chartY + chartHeight / 2
      for col in [:chartWidth] do
        let x := chartX + col
        result := result.setStyled x gridY '·' c.gridStyle

    -- Draw data series using Braille characters
    let grid := BrailleGrid.new chartWidth chartHeight
    let maxLen := c.maxDataLength

    let mut brailleGrid := grid
    for series in c.series do
      let dataLen := series.data.length
      if dataLen < 2 then continue

      -- Draw lines between consecutive points
      for i in [:dataLen - 1] do
        match (series.data[i]?, series.data[i + 1]?) with
        | (some v1, some v2) =>
          -- Map to pixel coordinates
          let x1 := if maxLen > 1 then (Float.ofNat i / Float.ofNat (maxLen - 1)) * Float.ofNat (chartWidth - 1) else 0
          let x2 := if maxLen > 1 then (Float.ofNat (i + 1) / Float.ofNat (maxLen - 1)) * Float.ofNat (chartWidth - 1) else 0

          let normalizedY1 := if yRange > 0 then (v1 - yMin) / yRange else 0.5
          let normalizedY2 := if yRange > 0 then (v2 - yMin) / yRange else 0.5

          -- Invert Y (screen Y increases downward)
          let y1 := (1.0 - normalizedY1) * Float.ofNat (chartHeight - 1)
          let y2 := (1.0 - normalizedY2) * Float.ofNat (chartHeight - 1)

          -- Scale to pixel coordinates (2x horizontal, 4x vertical for braille)
          let px1 := Int.ofNat (x1 * 2).toUInt32.toNat
          let py1 := Int.ofNat (y1 * 4).toUInt32.toNat
          let px2 := Int.ofNat (x2 * 2).toUInt32.toNat
          let py2 := Int.ofNat (y2 * 4).toUInt32.toNat

          brailleGrid := brailleGrid.drawLine px1 py1 px2 py2 series.style
        | _ => pure ()

    -- Render braille grid to buffer
    for cellY in [:chartHeight] do
      for cellX in [:chartWidth] do
        let (char, style) := brailleGrid.getCell cellX cellY
        if char != '⠀' then  -- Only draw non-empty
          let x := chartX + cellX
          let y := chartY + cellY
          result := result.setStyled x y char style

    -- Draw legend
    if c.showLegend && !c.series.isEmpty then
      let legendY := contentArea.y + contentArea.height - 1
      let mut legendX := chartX

      for series in c.series do
        if !series.label.isEmpty then
          -- Draw color marker
          result := result.setStyled legendX legendY '─' series.style
          result := result.setStyled (legendX + 1) legendY '─' series.style
          legendX := legendX + 3

          -- Draw label
          let labelChars := series.label.toList
          for j in [:labelChars.length] do
            if legendX + j < contentArea.x + contentArea.width then
              match labelChars[j]? with
              | some ch => result := result.setStyled (legendX + j) legendY ch c.legendStyle
              | none => pure ()
          legendX := legendX + series.label.length + 2

    result

end Terminus
