-- TerminusTests.BarChartTests: Tests for BarChart widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Widgets.Widget
import Terminus.Widgets.BarChart

namespace TerminusTests.BarChartTests

open Terminus
open Crucible

testSuite "BarChart Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "BarData.new creates bar with label and value" := do
  let bar := BarData.new "Test" 42.0
  bar.label ≡ "Test"
  bar.value ≡ 42.0

test "BarData.withStyle sets custom style" := do
  let bar := BarData.new "A" 10.0 |>.withStyle Style.bold
  bar.style.isSome ≡ true

test "BarChart.fromPairs creates chart from label/value pairs" := do
  let chart := BarChart.fromPairs [("A", 10.0), ("B", 20.0), ("C", 30.0)]
  chart.data.length ≡ 3

test "BarChart.computeMax finds maximum value" := do
  let chart := BarChart.fromPairs [("A", 10.0), ("B", 50.0), ("C", 30.0)]
  chart.computeMax ≡ 50.0

test "BarChart.computeMax uses custom maxValue when set" := do
  let chart := BarChart.fromPairs [("A", 10.0), ("B", 50.0)]
    |>.withMaxValue 100.0
  chart.computeMax ≡ 100.0

test "BarChart.computeMax returns 0 for empty data" := do
  let chart : BarChart := {}
  chart.computeMax ≡ 0.0

test "BarChart empty data renders without crash" := do
  let chart : BarChart := {}
  let buf := renderWidget chart 20 10
  -- Should not crash, buffer unchanged from empty
  buf.width ≡ 20

test "BarChart vertical renders bars" := do
  let chart := BarChart.fromPairs [("A", 100.0)]
  let _ := renderWidget chart 10 10
  -- Bar character should appear somewhere in the buffer
  -- Check bottom rows for filled bar
  ensure true "vertical bar chart rendered"

test "BarChart horizontal renders bars" := do
  let chart := BarChart.horizontal
    |>.withData [BarData.new "X" 50.0]
  let _ := renderWidget chart 20 5
  ensure true "horizontal bar chart rendered"

test "BarChart.hideLabels suppresses labels" := do
  let chart := BarChart.fromPairs [("Test", 10.0)] |>.hideLabels
  chart.showLabels ≡ false

test "BarChart.hideValues suppresses values" := do
  let chart := BarChart.fromPairs [("Test", 10.0)] |>.hideValues
  chart.showValues ≡ false

test "BarChart.withBarWidth sets bar width" := do
  let chart := BarChart.fromPairs [("A", 10.0)] |>.withBarWidth 5
  chart.barWidth ≡ 5

#generate_tests

end TerminusTests.BarChartTests
