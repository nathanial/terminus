-- TerminusTests.ScrollViewTests: Tests for ScrollView widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Paragraph
import Terminus.Widgets.ScrollView

namespace TerminusTests.ScrollViewTests

open Terminus
open Crucible

testSuite "ScrollView Widget Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "ScrollView.new creates with content" := do
  let sv := ScrollView.new (Paragraph.fromString "Hello")
  sv.offset ≡ (0, 0)
  sv.contentSize ≡ (0, 0)

test "ScrollView.withContentSize sets dimensions" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withContentSize 100 50
  sv.contentSize ≡ (100, 50)

test "ScrollView.withOffset sets scroll position" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 10 20
  sv.offset ≡ (10, 20)

test "ScrollView.contentWidth returns width" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withContentSize 80 40
  sv.contentWidth ≡ 80

test "ScrollView.contentHeight returns height" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withContentSize 80 40
  sv.contentHeight ≡ 40

test "ScrollView.offsetX returns x offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 15 25
  sv.offsetX ≡ 15

test "ScrollView.offsetY returns y offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 15 25
  sv.offsetY ≡ 25

test "ScrollView.scrollDown increments y offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 0 10 |>.scrollDown 5
  sv.offsetY ≡ 15

test "ScrollView.scrollUp decrements y offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 0 10 |>.scrollUp 3
  sv.offsetY ≡ 7

test "ScrollView.scrollRight increments x offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 5 0 |>.scrollRight 10
  sv.offsetX ≡ 15

test "ScrollView.scrollLeft decrements x offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 10 0 |>.scrollLeft 4
  sv.offsetX ≡ 6

test "ScrollView.scrollToTop resets y offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 5 20 |>.scrollToTop
  sv.offsetY ≡ 0
  sv.offsetX ≡ 5

test "ScrollView.scrollToLeft resets x offset" := do
  let sv := ScrollView.new (Paragraph.fromString "Test") |>.withOffset 15 10 |>.scrollToLeft
  sv.offsetX ≡ 0
  sv.offsetY ≡ 10

test "ScrollView renders without crash" := do
  let sv := ScrollView.new (Paragraph.fromString "Hello World")
    |>.withContentSize 20 5
  let buf := renderWidget sv 10 3
  buf.width ≡ 10

#generate_tests

end TerminusTests.ScrollViewTests
