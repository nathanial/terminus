-- TerminusTests.LineChartTests: Tests for LineChart widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Widgets.Widget
import Terminus.Widgets.LineChart

namespace TerminusTests.LineChartTests

open Terminus
open Crucible

testSuite "LineChart Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "DataSeries.new creates series from data" := do
  let series := DataSeries.new [1.0, 2.0, 3.0]
  series.data.length ≡ 3

test "DataSeries.withLabel sets label" := do
  let series := DataSeries.new [1.0] |>.withLabel "Sales"
  series.label ≡ "Sales"

test "DataSeries.min finds minimum value" := do
  let series := DataSeries.new [5.0, 2.0, 8.0, 1.0]
  series.min ≡ 1.0

test "DataSeries.max finds maximum value" := do
  let series := DataSeries.new [5.0, 2.0, 8.0, 1.0]
  series.max ≡ 8.0

test "LineChart.new creates empty chart" := do
  let chart := LineChart.new
  chart.series.isEmpty ≡ true

test "LineChart.addSeries adds series to chart" := do
  let chart := LineChart.new
    |>.addSeries (DataSeries.new [1.0, 2.0])
    |>.addSeries (DataSeries.new [3.0, 4.0])
  chart.series.length ≡ 2

test "LineChart.maxDataLength finds longest series" := do
  let chart := LineChart.new
    |>.addSeries (DataSeries.new [1.0, 2.0, 3.0])
    |>.addSeries (DataSeries.new [1.0, 2.0])
  chart.maxDataLength ≡ 3

test "LineChart.withYRange sets Y axis bounds" := do
  let chart := LineChart.new |>.withYRange 0.0 100.0
  chart.yMin ≡ some 0.0
  chart.yMax ≡ some 100.0

test "LineChart.hideLegend disables legend" := do
  let chart := LineChart.new |>.hideLegend
  chart.showLegend ≡ false

test "LineChart renders without crash" := do
  let chart := LineChart.new
    |>.addSeries (DataSeries.new [10.0, 20.0, 15.0, 25.0] |>.withLabel "Data")
  let buf := renderWidget chart 40 20
  buf.width ≡ 40

#generate_tests

end TerminusTests.LineChartTests
