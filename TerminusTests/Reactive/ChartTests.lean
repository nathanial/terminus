-- TerminusTests.Reactive.ChartTests: Tests for visualization widgets

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.ChartTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Chart Widget Tests"

-- ============================================================================
-- Gauge Tests
-- ============================================================================

test "GaugeConfig has sensible defaults" := do
  let config : GaugeConfig := {}
  config.width ≡ 20
  config.filledChar ≡ '█'
  config.unfilledChar ≡ '░'
  config.showPercent ≡ true
  config.minValue ≡ 0.0
  config.maxValue ≡ 1.0

test "gauge' renders filled and unfilled portions" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      gauge' 0.5 { width := 10, showPercent := false }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected filled char")
    SpiderM.liftIO (ensure (rnodeContainsText node "░") "expected unfilled char")

test "gauge' shows percentage when enabled" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      gauge' 0.75 { width := 10, showPercent := true }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "75%") "expected percentage")

test "gauge' clamps values to valid range" := do
  runSpider do
    let (events, _) ← createInputs

    -- Value above max
    let (_, render1) ← (runWidget do
      gauge' 1.5 { width := 10, showPercent := true }
    ).run events

    let node1 ← SpiderM.liftIO render1.sample
    SpiderM.liftIO (ensure (rnodeContainsText node1 "100%") "expected 100% for value > max")

    -- Value below min
    let (_, render2) ← (runWidget do
      gauge' (-0.5) { width := 10, showPercent := true }
    ).run events

    let node2 ← SpiderM.liftIO render2.sample
    SpiderM.liftIO (ensure (rnodeContainsText node2 "0%") "expected 0% for value < min")

test "gauge' shows label when provided" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      gauge' 0.5 { width := 10, label := some "Progress", showPercent := false }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Progress") "expected label")

-- ============================================================================
-- LineGauge Tests
-- ============================================================================

test "LineGaugeConfig has sensible defaults" := do
  let config : LineGaugeConfig := {}
  config.width ≡ 20
  config.filledChar ≡ '━'
  config.unfilledChar ≡ '─'
  config.showPercent ≡ false

test "lineGauge' renders thin progress bar" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      lineGauge' 0.6 { width := 10 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "━") "expected filled char")

-- ============================================================================
-- VGauge Tests
-- ============================================================================

test "VGaugeConfig has sensible defaults" := do
  let config : VGaugeConfig := {}
  config.height ≡ 10
  config.filledChar ≡ '█'
  config.unfilledChar ≡ '░'

test "vGauge' renders vertical gauge" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      vGauge' 0.5 { height := 6 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected filled char")

-- ============================================================================
-- Sparkline Tests
-- ============================================================================

test "SparklineConfig has sensible defaults" := do
  let config : SparklineConfig := {}
  config.showBaseline ≡ true

test "sparkline' renders data as bars" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      sparkline' #[1.0, 5.0, 3.0, 8.0, 2.0] {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain some sparkline characters
    SpiderM.liftIO (ensure (rnodeContainsText node "▁") "expected low bar char")

test "sparkline' handles empty data" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      sparkline' #[] {}
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Should be empty node

test "sparklineInt' converts int data" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      sparklineInt' #[1, 5, 3, 8, 2] {}
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Should render something

test "sparklineNat' converts nat data" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      sparklineNat' #[1, 5, 3, 8, 2] {}
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Should render something

test "labeledSparkline' shows label and value" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      labeledSparkline' "CPU" #[30.0, 45.0, 60.0, 55.0] { showValue := true }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "CPU") "expected label")

-- ============================================================================
-- BarChart Tests
-- ============================================================================

test "BarChartConfig has sensible defaults" := do
  let config : BarChartConfig := {}
  config.orientation ≡ BarOrientation.vertical
  config.barWidth ≡ 3
  config.gap ≡ 1
  config.showValues ≡ true
  config.showLabels ≡ true
  config.size ≡ 10

test "Terminus.Reactive.BarData.new creates bar data" := do
  let bar := Terminus.Reactive.BarData.new "Test" 42.0
  bar.label ≡ "Test"
  bar.value ≡ 42.0

test "Terminus.Reactive.BarData.styled creates styled bar" := do
  let style : Style := { fg := .ansi .red }
  let bar := Terminus.Reactive.BarData.styled "Test" 42.0 style
  bar.label ≡ "Test"
  (bar.style.isSome) ≡ true

test "barChart' renders vertical bars" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      barChart' #[
        Terminus.Reactive.BarData.new "A" 10,
        Terminus.Reactive.BarData.new "B" 20,
        Terminus.Reactive.BarData.new "C" 15
      ] { size := 5 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected bar char")
    SpiderM.liftIO (ensure (rnodeContainsText node "A") "expected label A")
    SpiderM.liftIO (ensure (rnodeContainsText node "B") "expected label B")

test "barChart' renders horizontal bars" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      barChart' #[
        Terminus.Reactive.BarData.new "Alpha" 10,
        Terminus.Reactive.BarData.new "Beta" 20
      ] { orientation := .horizontal, size := 20 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected bar char")

test "barChart' handles empty data" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      barChart' #[] {}
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Should be empty

-- ============================================================================
-- LineChart Tests
-- ============================================================================

test "LineChartConfig has sensible defaults" := do
  let config : LineChartConfig := {}
  config.width ≡ 40
  config.height ≡ 10
  config.showYAxis ≡ true
  config.showXAxis ≡ true
  config.showLegend ≡ true
  config.yAxisWidth ≡ 6

test "DataSeries.new creates series" := do
  let series := DataSeries.new #[1.0, 2.0, 3.0]
  series.data.size ≡ 3

test "DataSeries.labeled creates labeled series" := do
  let series := DataSeries.labeled "Sales" #[1.0, 2.0, 3.0]
  series.label ≡ "Sales"
  series.data.size ≡ 3

test "DataSeries.min returns minimum" := do
  let series := DataSeries.new #[5.0, 2.0, 8.0, 1.0]
  (series.min == 1.0) ≡ true

test "DataSeries.max returns maximum" := do
  let series := DataSeries.new #[5.0, 2.0, 8.0, 1.0]
  (series.max == 8.0) ≡ true

test "lineChart' renders chart with axes" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      lineChart' #[DataSeries.labeled "Data" #[10.0, 20.0, 15.0, 25.0]] {
        width := 30
        height := 8
      }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "│") "expected Y axis")
    SpiderM.liftIO (ensure (rnodeContainsText node "─") "expected X axis")

test "lineChart' shows legend" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      lineChart' #[DataSeries.labeled "Sales" #[10.0, 20.0, 15.0]] {
        width := 30
        height := 8
        showLegend := true
      }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Sales") "expected legend label")

test "lineChart' handles empty series" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      lineChart' #[] {}
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Should be empty

-- ============================================================================
-- PieChart Tests
-- ============================================================================

test "PieChartConfig has sensible defaults" := do
  let config : PieChartConfig := {}
  config.radius ≡ 5
  config.showLegend ≡ true
  config.showPercent ≡ true
  config.marker ≡ '■'
  config.donutRatio ≡ 0.0

test "Terminus.Reactive.PieSlice.new creates slice" := do
  let slice := Terminus.Reactive.PieSlice.new "Category" 42.0
  slice.label ≡ "Category"
  slice.value ≡ 42.0

test "Terminus.Reactive.PieSlice.styled creates styled slice" := do
  let style : Style := { fg := .ansi .red }
  let slice := Terminus.Reactive.PieSlice.styled "Category" 42.0 style
  slice.label ≡ "Category"

test "pieChart' renders distribution" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      pieChart' #[
        Terminus.Reactive.PieSlice.new "A" 30,
        Terminus.Reactive.PieSlice.new "B" 50,
        Terminus.Reactive.PieSlice.new "C" 20
      ] {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Distribution") "expected title")
    SpiderM.liftIO (ensure (rnodeContainsText node "A") "expected label A")
    SpiderM.liftIO (ensure (rnodeContainsText node "B") "expected label B")
    SpiderM.liftIO (ensure (rnodeContainsText node "C") "expected label C")

test "pieChart' shows percentages" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      pieChart' #[
        Terminus.Reactive.PieSlice.new "Half" 50,
        Terminus.Reactive.PieSlice.new "Quarter" 25,
        Terminus.Reactive.PieSlice.new "Quarter2" 25
      ] { showPercent := true }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "50%") "expected 50%")
    SpiderM.liftIO (ensure (rnodeContainsText node "25%") "expected 25%")

test "pieChart' handles empty data" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      pieChart' #[] {}
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Should be empty

test "pieChart' filters zero values" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      pieChart' #[
        Terminus.Reactive.PieSlice.new "Valid" 50,
        Terminus.Reactive.PieSlice.new "Zero" 0,
        Terminus.Reactive.PieSlice.new "Also Valid" 50
      ] {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Valid") "expected Valid label")
    -- Zero value slice should not appear



end TerminusTests.Reactive.ChartTests
