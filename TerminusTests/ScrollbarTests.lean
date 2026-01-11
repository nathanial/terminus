-- TerminusTests.ScrollbarTests: Tests for Scrollbar widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Scrollbar

namespace TerminusTests.ScrollbarTests

open Terminus
open Crucible

testSuite "Scrollbar Widget Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Scrollbar.vertical creates vertical scrollbar" := do
  let sb := Scrollbar.vertical 10 100 10
  sb.position ≡ 10
  sb.totalItems ≡ 100
  sb.visibleItems ≡ 10
  sb.orientation ≡ .vertical

test "Scrollbar.horizontal creates horizontal scrollbar" := do
  let sb := Scrollbar.horizontal 0 50 20
  sb.orientation ≡ .horizontal

test "Scrollbar.isNeeded returns true when content exceeds viewport" := do
  let sb := Scrollbar.vertical 0 100 10
  sb.isNeeded ≡ true

test "Scrollbar.isNeeded returns false when content fits" := do
  let sb := Scrollbar.vertical 0 10 20
  sb.isNeeded ≡ false

test "Scrollbar.isNeeded returns false when equal" := do
  let sb := Scrollbar.vertical 0 10 10
  sb.isNeeded ≡ false

test "Scrollbar.thumbMetrics returns valid size and position" := do
  let sb := Scrollbar.vertical 0 100 10
  let (thumbSize, thumbPos) := sb.thumbMetrics 10
  -- With 10 visible of 100, thumb should be 1/10 of track = 1
  thumbSize ≡ 1
  thumbPos ≡ 0

test "Scrollbar.thumbMetrics at end position" := do
  let sb := Scrollbar.vertical 90 100 10
  let (_, thumbPos) := sb.thumbMetrics 10
  -- At position 90 of 100 with 10 visible, should be at end
  thumbPos ≡ 9

test "Scrollbar.setPosition updates position" := do
  let sb := Scrollbar.vertical 0 100 10 |>.setPosition 50
  sb.position ≡ 50

test "Scrollbar.withTrackChar changes track character" := do
  let sb := Scrollbar.vertical 0 10 5 |>.withTrackChar '·'
  sb.trackChar ≡ '·'

test "Scrollbar.withThumbChar changes thumb character" := do
  let sb := Scrollbar.vertical 0 10 5 |>.withThumbChar '▓'
  sb.thumbChar ≡ '▓'

test "Scrollbar renders without crash" := do
  let sb := Scrollbar.vertical 5 20 5
  let buf := renderWidget sb 1 10
  buf.height ≡ 10

#generate_tests

end TerminusTests.ScrollbarTests
