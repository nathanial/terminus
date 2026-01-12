/-
  Terminus Reactive - Chart Widgets
  Bar charts, line charts, and pie charts.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Bar Chart Types -/

/-- A single bar in a bar chart. -/
structure BarData where
  /-- Label for the bar. -/
  label : String
  /-- Value of the bar. -/
  value : Float
  /-- Optional custom style. -/
  style : Option Style := none
  deriving Repr, Inhabited

namespace BarData

/-- Create a new bar data item. -/
def new (label : String) (value : Float) : BarData := { label, value }

/-- Create a bar with custom style. -/
def styled (label : String) (value : Float) (style : Style) : BarData :=
  { label, value, style := some style }

end BarData

/-- Bar chart orientation. -/
inductive BarOrientation where
  | vertical
  | horizontal
  deriving Repr, BEq, Inhabited

/-! ## Bar Chart Configuration -/

/-- Configuration for bar chart widget. -/
structure BarChartConfig where
  /-- Chart orientation. -/
  orientation : BarOrientation := .vertical
  /-- Width of each bar (in characters). -/
  barWidth : Nat := 3
  /-- Gap between bars. -/
  gap : Nat := 1
  /-- Maximum value (auto-calculated if none). -/
  maxValue : Option Float := none
  /-- Show value labels. -/
  showValues : Bool := true
  /-- Show bar labels. -/
  showLabels : Bool := true
  /-- Default bar style. -/
  barStyle : Style := { fg := .ansi .blue }
  /-- Label style. -/
  labelStyle : Style := {}
  /-- Value style. -/
  valueStyle : Style := { fg := .ansi .brightBlack }
  /-- Chart height (for vertical) or width (for horizontal). -/
  size : Nat := 10
  deriving Repr, Inhabited

/-! ## Bar Chart Helpers -/

private def computeBarMax (data : Array BarData) (override : Option Float) : Float :=
  match override with
  | some m => m
  | none => data.foldl (fun acc d => if d.value > acc then d.value else acc) 0.0

private def renderVerticalBarChart (data : Array BarData) (config : BarChartConfig) : RNode := Id.run do
  let maxVal := computeBarMax data config.maxValue
  let chartHeight := config.size

  -- Build rows from top to bottom
  let mut rows : Array RNode := #[]

  -- Render value row if enabled
  if config.showValues then
    let mut valueNodes : Array RNode := #[]
    for bar in data do
      let valStr := s!"{bar.value.toUInt32}"
      let padded := valStr.take config.barWidth
      let padding := String.ofList (List.replicate (config.barWidth - padded.length) ' ')
      valueNodes := valueNodes.push (RNode.text (padding ++ padded) config.valueStyle)
      if config.gap > 0 then
        valueNodes := valueNodes.push (RNode.text (String.ofList (List.replicate config.gap ' ')) {})
    rows := rows.push (RNode.row 0 {} valueNodes)

  -- Render bar rows
  for row in [:chartHeight] do
    let threshold := (chartHeight - row).toFloat / chartHeight.toFloat
    let mut barNodes : Array RNode := #[]
    for bar in data do
      let normalizedValue := if maxVal > 0 then bar.value / maxVal else 0.0
      let barStyle := bar.style.getD config.barStyle
      let char := if normalizedValue >= threshold then '█' else ' '
      let barStr := String.ofList (List.replicate config.barWidth char)
      barNodes := barNodes.push (RNode.text barStr (if char == '█' then barStyle else {}))
      if config.gap > 0 then
        barNodes := barNodes.push (RNode.text (String.ofList (List.replicate config.gap ' ')) {})
    rows := rows.push (RNode.row 0 {} barNodes)

  -- Render label row if enabled
  if config.showLabels then
    let mut labelNodes : Array RNode := #[]
    for bar in data do
      let lbl := bar.label.take config.barWidth
      let padding := String.ofList (List.replicate (config.barWidth - lbl.length) ' ')
      labelNodes := labelNodes.push (RNode.text (lbl ++ padding) config.labelStyle)
      if config.gap > 0 then
        labelNodes := labelNodes.push (RNode.text (String.ofList (List.replicate config.gap ' ')) {})
    rows := rows.push (RNode.row 0 {} labelNodes)

  RNode.column 0 {} rows

private def renderHorizontalBarChart (data : Array BarData) (config : BarChartConfig) : RNode := Id.run do
  let maxVal := computeBarMax data config.maxValue
  let chartWidth := config.size

  -- Find max label width for alignment
  let maxLabelLen := data.foldl (fun acc d => Nat.max acc d.label.length) 0
  let labelWidth := Nat.min maxLabelLen 10

  let mut rows : Array RNode := #[]

  for bar in data do
    let normalizedValue := if maxVal > 0 then bar.value / maxVal else 0.0
    let barLen := (normalizedValue * chartWidth.toFloat).toUInt32.toNat
    let barStyle := bar.style.getD config.barStyle

    let mut rowNodes : Array RNode := #[]

    -- Label
    if config.showLabels then
      let lbl := bar.label.take labelWidth
      let padding := String.ofList (List.replicate (labelWidth - lbl.length) ' ')
      rowNodes := rowNodes.push (RNode.text (padding ++ lbl ++ " ") config.labelStyle)

    -- Bar
    let barStr := String.ofList (List.replicate barLen '█')
    rowNodes := rowNodes.push (RNode.text barStr barStyle)

    -- Value
    if config.showValues then
      rowNodes := rowNodes.push (RNode.text s!" {bar.value.toUInt32}" config.valueStyle)

    rows := rows.push (RNode.row 0 {} rowNodes)

    -- Gap between bars
    if config.gap > 0 then
      for _ in [:config.gap] do
        rows := rows.push RNode.empty

  RNode.column 0 {} rows

private def renderBarChart (data : Array BarData) (config : BarChartConfig) : RNode :=
  if data.isEmpty then
    RNode.empty
  else
    match config.orientation with
    | .vertical => renderVerticalBarChart data config
    | .horizontal => renderHorizontalBarChart data config

/-! ## Bar Chart Widget -/

/-- Create a vertical bar chart widget.

    Example:
    ```
    barChart' #[
      BarData.new "A" 10,
      BarData.new "B" 25,
      BarData.new "C" 15
    ] { size := 8 }
    ```
-/
def barChart' (data : Array BarData) (config : BarChartConfig := {}) : WidgetM Unit := do
  emitStatic (renderBarChart data config)

/-- Create a dynamic bar chart widget. -/
def dynBarChart' (data : Reactive.Dynamic Spider (Array BarData)) (config : BarChartConfig := {})
    : WidgetM Unit := do
  let node ← data.map' (fun arr =>
    if arr.isEmpty then
      RNode.empty
    else
      -- For simplicity, just render horizontal bars in dynamic mode
      renderHorizontalBarChart arr config
  )
  emit node

/-! ## Line Chart Types -/

/-- A data series for line charts. -/
structure DataSeries where
  /-- Data points. -/
  data : Array Float := #[]
  /-- Series label. -/
  label : String := ""
  /-- Series style. -/
  style : Style := {}
  /-- Marker character for data points. -/
  marker : Char := '●'
  deriving Repr, Inhabited

namespace DataSeries

/-- Create a new data series. -/
def new (data : Array Float) : DataSeries := { data }

/-- Create a labeled data series. -/
def labeled (label : String) (data : Array Float) : DataSeries := { data, label }

/-- Create a styled data series. -/
def styled (data : Array Float) (style : Style) : DataSeries := { data, style }

/-- Get minimum value in series. -/
def min (s : DataSeries) : Float :=
  s.data.foldl (fun a b => if b < a then b else a) 1.0e38

/-- Get maximum value in series. -/
def max (s : DataSeries) : Float :=
  s.data.foldl (fun a b => if b > a then b else a) (-1.0e38)

end DataSeries

/-! ## Line Chart Configuration -/

/-- Configuration for line chart widget. -/
structure LineChartConfig where
  /-- Chart width in characters. -/
  width : Nat := 40
  /-- Chart height in characters. -/
  height : Nat := 10
  /-- Minimum Y value (auto if none). -/
  yMin : Option Float := none
  /-- Maximum Y value (auto if none). -/
  yMax : Option Float := none
  /-- Show Y axis. -/
  showYAxis : Bool := true
  /-- Show X axis. -/
  showXAxis : Bool := true
  /-- Show legend. -/
  showLegend : Bool := true
  /-- Y axis width. -/
  yAxisWidth : Nat := 6
  /-- Axis style. -/
  axisStyle : Style := { fg := .ansi .brightBlack }
  /-- Legend style. -/
  legendStyle : Style := {}
  deriving Repr, Inhabited

private def renderLineChart (series : Array DataSeries) (config : LineChartConfig) : RNode := Id.run do
  if series.isEmpty || series.all (·.data.isEmpty) then
    return RNode.empty

  -- Compute Y range
  let allMin := series.foldl (fun acc s => if s.min < acc then s.min else acc) 1.0e38
  let allMax := series.foldl (fun acc s => if s.max > acc then s.max else acc) (-1.0e38)
  let yMin := config.yMin.getD allMin
  let yMax := config.yMax.getD allMax
  let yRange := yMax - yMin
  let yRange := if yRange <= 0 then 1.0 else yRange

  -- Chart dimensions
  let chartWidth := config.width - (if config.showYAxis then config.yAxisWidth else 0)
  let chartHeight := config.height - (if config.showXAxis then 1 else 0) -
                     (if config.showLegend && series.any (!·.label.isEmpty) then 1 else 0)

  -- Find max data length
  let maxLen := series.foldl (fun acc s => Nat.max acc s.data.size) 0

  -- Build character grid
  let mut grid : Array (Array (Char × Style)) := #[]
  for _ in [:chartHeight] do
    grid := grid.push (Array.replicate chartWidth (' ', {}))

  -- Plot each series
  for s in series do
    if s.data.isEmpty then continue
    for i in [:s.data.size] do
      if let some v := s.data[i]? then
        let x := if maxLen > 1 then (i.toFloat / (maxLen - 1).toFloat) * (chartWidth - 1).toFloat
                 else 0.0
        let normalizedY := (v - yMin) / yRange
        let y := ((1.0 - normalizedY) * (chartHeight - 1).toFloat)
        let xi := x.toUInt32.toNat
        let yi := y.toUInt32.toNat
        if yi < chartHeight && xi < chartWidth then
          grid := grid.modify yi (fun row => row.modify xi (fun _ => (s.marker, s.style)))

  -- Build rows
  let mut rows : Array RNode := #[]

  -- Y axis labels and chart rows
  for y in [:chartHeight] do
    let mut rowNodes : Array RNode := #[]

    if config.showYAxis then
      let yVal := yMax - (y.toFloat / (chartHeight - 1).toFloat) * yRange
      let label := s!"{yVal.toUInt32}"
      let padded := String.ofList (List.replicate (config.yAxisWidth - label.length - 1) ' ') ++ label ++ "│"
      rowNodes := rowNodes.push (RNode.text padded config.axisStyle)

    -- Chart row
    if let some row := grid[y]? then
      for (char, style) in row do
        rowNodes := rowNodes.push (RNode.text (String.singleton char) style)

    rows := rows.push (RNode.row 0 {} rowNodes)

  -- X axis
  if config.showXAxis then
    let mut axisNodes : Array RNode := #[]
    if config.showYAxis then
      axisNodes := axisNodes.push (RNode.text (String.ofList (List.replicate (config.yAxisWidth - 1) ' ') ++ "└") config.axisStyle)
    axisNodes := axisNodes.push (RNode.text (String.ofList (List.replicate chartWidth '─')) config.axisStyle)
    rows := rows.push (RNode.row 0 {} axisNodes)

  -- Legend
  if config.showLegend then
    let legendSeries := series.filter (!·.label.isEmpty)
    if !legendSeries.isEmpty then
      let mut legendNodes : Array RNode := #[]
      for s in legendSeries do
        legendNodes := legendNodes.push (RNode.text (String.singleton s.marker ++ " ") s.style)
        legendNodes := legendNodes.push (RNode.text (s.label ++ "  ") config.legendStyle)
      rows := rows.push (RNode.row 0 {} legendNodes)

  return RNode.column 0 {} rows

/-! ## Line Chart Widget -/

/-- Create a simple line chart widget using ASCII characters.

    Example:
    ```
    lineChart' #[DataSeries.labeled "Sales" #[10, 20, 15, 30, 25]] {
      width := 30
      height := 8
    }
    ```
-/
def lineChart' (series : Array DataSeries) (config : LineChartConfig := {}) : WidgetM Unit := do
  emitStatic (renderLineChart series config)

/-- Create a dynamic line chart widget. -/
def dynLineChart' (series : Reactive.Dynamic Spider (Array DataSeries))
    (config : LineChartConfig := {}) : WidgetM Unit := do
  let node ← series.map' (fun arr =>
    renderLineChart arr config
  )
  emit node

/-! ## Pie Chart Types -/

/-- A slice in a pie chart. -/
structure PieSlice where
  /-- Label for the slice. -/
  label : String
  /-- Value of the slice. -/
  value : Float
  /-- Slice style. -/
  style : Style := {}
  deriving Repr, Inhabited

namespace PieSlice

/-- Create a new pie slice. -/
def new (label : String) (value : Float) : PieSlice := { label, value }

/-- Create a styled pie slice. -/
def styled (label : String) (value : Float) (style : Style) : PieSlice := { label, value, style }

end PieSlice

/-! ## Pie Chart Configuration -/

/-- Configuration for pie chart widget. -/
structure PieChartConfig where
  /-- Radius of the pie (in characters). -/
  radius : Nat := 5
  /-- Show legend. -/
  showLegend : Bool := true
  /-- Show percentage values. -/
  showPercent : Bool := true
  /-- Marker character for legend. -/
  marker : Char := '■'
  /-- Label style. -/
  labelStyle : Style := {}
  /-- Percent style. -/
  percentStyle : Style := { fg := .ansi .brightBlack }
  /-- Donut ratio (0.0 = pie, 0.5 = donut). -/
  donutRatio : Float := 0.0
  deriving Repr, Inhabited

private def renderPieChart (data : Array PieSlice) (config : PieChartConfig) : RNode := Id.run do
  let validData := data.filter (·.value > 0)
  if validData.isEmpty then
    return RNode.empty
  else
    let total := validData.foldl (fun acc s => acc + s.value) 0.0
    if total <= 0 then
      return RNode.empty
    else
      -- Simple pie representation using proportional bars
      let barWidth := config.radius * 2

      let mut rows : Array RNode := #[]

      -- Title row with pie symbol
      rows := rows.push (RNode.text "◐ Distribution" config.labelStyle)
      rows := rows.push RNode.empty

      -- Render each slice as a proportional bar
      for slice in validData do
        let pct := (slice.value / total * 100.0).toUInt32.toNat
        let barLen := (slice.value / total * barWidth.toFloat).toUInt32.toNat

        let mut rowNodes : Array RNode := #[]

        -- Marker
        rowNodes := rowNodes.push (RNode.text (String.singleton config.marker ++ " ") slice.style)

        -- Label
        let label := slice.label.take 10
        let labelPad := String.ofList (List.replicate (10 - label.length) ' ')
        rowNodes := rowNodes.push (RNode.text (label ++ labelPad ++ " ") config.labelStyle)

        -- Bar
        let barStr := String.ofList (List.replicate barLen '█')
        rowNodes := rowNodes.push (RNode.text barStr slice.style)

        -- Percentage
        if config.showPercent then
          rowNodes := rowNodes.push (RNode.text s!" {pct}%" config.percentStyle)

        rows := rows.push (RNode.row 0 {} rowNodes)

      return RNode.column 0 {} rows

/-! ## Pie Chart Widget -/

/-- Create a pie chart widget using ASCII art.

    Note: Terminal pie charts are limited in resolution. This uses a simple
    representation with legend showing proportions.

    Example:
    ```
    pieChart' #[
      PieSlice.styled "A" 30 { fg := .ansi .red },
      PieSlice.styled "B" 50 { fg := .ansi .green },
      PieSlice.styled "C" 20 { fg := .ansi .blue }
    ] {}
    ```
-/
def pieChart' (data : Array PieSlice) (config : PieChartConfig := {}) : WidgetM Unit := do
  emitStatic (renderPieChart data config)

/-- Create a dynamic pie chart widget. -/
def dynPieChart' (data : Reactive.Dynamic Spider (Array PieSlice))
    (config : PieChartConfig := {}) : WidgetM Unit := do
  let node ← data.map' (fun arr => renderPieChart arr config)
  emit node

end Terminus.Reactive
