-- TerminusTests.SpinnerTests: Tests for Spinner widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Spinner

namespace TerminusTests.SpinnerTests

open Terminus
open Crucible

testSuite "Spinner Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Spinner.dots has correct frame count" := do
  Spinner.dots.frames.size ≡ 10

test "Spinner.line has correct frame count" := do
  Spinner.line.frames.size ≡ 4

test "Spinner.ascii has correct frame count" := do
  Spinner.ascii.frames.size ≡ 4

test "Spinner.withFrame advances to correct frame" := do
  let s := Spinner.dots.withFrame 3
  s.frameIndex ≡ 3

test "Spinner.withFrame wraps around" := do
  let s := Spinner.line.withFrame 10  -- 4 frames, so 10 % 4 = 2
  s.frameIndex ≡ 2

test "Spinner.currentFrame returns correct frame" := do
  let s := Spinner.ascii.withFrame 0
  s.currentFrame ≡ "|"

test "Spinner.withLabel sets label" := do
  let s := Spinner.dots.withLabel "Loading..."
  s.label ≡ some "Loading..."

test "Spinner renders without crash" := do
  let s := Spinner.dots.withFrame 0 |>.withLabel "Wait"
  let buf := renderWidget s 20 3
  buf.width ≡ 20

#generate_tests

end TerminusTests.SpinnerTests
