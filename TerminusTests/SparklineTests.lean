-- TerminusTests.SparklineTests: Tests for Sparkline widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Sparkline

namespace TerminusTests.SparklineTests

open Terminus
open Crucible

testSuite "Sparkline Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Sparkline.new creates sparkline from data" := do
  let spark := Sparkline.new [1.0, 2.0, 3.0, 4.0]
  spark.data.length ≡ 4

test "Sparkline.fromNats creates sparkline from naturals" := do
  let spark := Sparkline.fromNats [1, 2, 3]
  spark.data.length ≡ 3

test "Sparkline.fromInts creates sparkline from integers" := do
  let spark := Sparkline.fromInts [1, 2, 3]
  spark.data.length ≡ 3

test "Sparkline.computeMax finds maximum" := do
  let spark := Sparkline.new [5.0, 10.0, 3.0]
  spark.computeMax ≡ 10.0

test "Sparkline.computeMax uses custom max when set" := do
  let spark := Sparkline.new [5.0, 10.0] |>.withMax 20.0
  spark.computeMax ≡ 20.0

test "Sparkline.appendData adds value" := do
  let spark := Sparkline.new [1.0, 2.0] |>.appendData 3.0
  spark.data.length ≡ 3

test "Sparkline.pushData maintains max length" := do
  let spark := Sparkline.new [1.0, 2.0, 3.0] |>.pushData 4.0 3
  spark.data.length ≡ 3

test "Sparkline renders without crash" := do
  let spark := Sparkline.new [1.0, 4.0, 2.0, 8.0, 5.0, 3.0]
  let buf := renderWidget spark 20 3
  buf.width ≡ 20

#generate_tests

end TerminusTests.SparklineTests
