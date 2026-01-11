-- TerminusTests.PieChartTests: Tests for PieChart widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Widgets.Widget
import Terminus.Widgets.PieChart

namespace TerminusTests.PieChartTests

open Terminus
open Crucible

testSuite "PieChart Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "PieSlice.new creates slice with label and value" := do
  let slice := PieSlice.new "Category" 50.0
  slice.label ≡ "Category"
  slice.value ≡ 50.0

test "PieChart.new creates chart from slices" := do
  let chart := PieChart.new [PieSlice.new "A" 30.0, PieSlice.new "B" 70.0]
  chart.data.length ≡ 2

test "PieChart.withDonutRatio clamps to valid range" := do
  let chart := PieChart.new [] |>.withDonutRatio 0.5
  chart.donutRatio ≡ 0.5
  let chart2 := PieChart.new [] |>.withDonutRatio 1.5
  -- Should be clamped to 0.9 max
  ensure (chart2.donutRatio <= 0.9) "donut ratio clamped"

test "PieChart.withStartAngle sets start angle" := do
  let chart := PieChart.new [] |>.withStartAngle 45.0
  chart.startAngle ≡ 45.0

test "PieChart.withResolution sets resolution" := do
  let chart := PieChart.new [] |>.withResolution .cell
  chart.resolution ≡ PieChartResolution.cell

test "PieChart renders without crash" := do
  let chart := PieChart.new [
    PieSlice.new "Red" 30.0 |>.withStyle (Style.fgColor .red),
    PieSlice.new "Blue" 50.0 |>.withStyle (Style.fgColor .blue),
    PieSlice.new "Green" 20.0 |>.withStyle (Style.fgColor .green)
  ]
  let buf := renderWidget chart 30 15
  buf.width ≡ 30

#generate_tests

end TerminusTests.PieChartTests
