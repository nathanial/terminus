-- TerminusTests.LineGaugeTests: Tests for LineGauge widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.LineGauge

namespace TerminusTests.LineGaugeTests

open Terminus
open Crucible

testSuite "LineGauge Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "LineGauge.new creates gauge with ratio" := do
  let gauge := LineGauge.new 0.5
  gauge.ratio ≡ 0.5

test "LineGauge.new clamps ratio to valid range" := do
  let gauge1 := LineGauge.new (-0.5)
  gauge1.ratio ≡ 0.0
  let gauge2 := LineGauge.new 1.5
  gauge2.ratio ≡ 1.0

test "LineGauge.fromPercent creates from percentage" := do
  let gauge := LineGauge.fromPercent 75
  gauge.ratio ≡ 0.75

test "LineGauge.percent returns percentage" := do
  let gauge := LineGauge.new 0.5
  gauge.percent ≡ 50

test "LineGauge.withLabel sets label" := do
  let gauge := LineGauge.new 0.5 |>.withLabel "Progress"
  gauge.label ≡ some "Progress"

test "LineGauge.setRatio updates ratio" := do
  let gauge := LineGauge.new 0.3 |>.setRatio 0.7
  gauge.ratio ≡ 0.7

test "LineGauge.setPercent updates from percent" := do
  let gauge := LineGauge.new 0.0 |>.setPercent 60
  gauge.ratio ≡ 0.6

test "LineGauge.withShowPercent enables percent display" := do
  let gauge := LineGauge.new 0.5 |>.withShowPercent true
  gauge.showPercent ≡ true

test "LineGauge renders without crash" := do
  let gauge := LineGauge.new 0.7 |>.withLabel "Loading" |>.withShowPercent true
  let buf := renderWidget gauge 30 3
  buf.width ≡ 30

#generate_tests

end TerminusTests.LineGaugeTests
