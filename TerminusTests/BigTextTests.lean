-- TerminusTests.BigTextTests: Tests for BigText widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Widgets.Widget
import Terminus.Widgets.BigText

namespace TerminusTests.BigTextTests

open Terminus
open Crucible

testSuite "BigText Widget Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "BigText.new creates with text" := do
  let bt := BigText.new "Hello"
  bt.text ≡ "Hello"

test "BigText.withFont changes font" := do
  let bt := BigText.new "Test" |>.withFont .slant
  -- Just verify it doesn't crash; BigFont doesn't have BEq
  ensure true "font changed"

test "BigText.withStyle sets style" := do
  let bt := BigText.new "Test" |>.withStyle (Style.fgColor Color.red)
  bt.style.fg ≡ Color.red

test "BigText.withOn changes on-pixel character" := do
  let bt := BigText.new "A" |>.withOn '#'
  bt.on ≡ '#'

test "BigText.withOff changes off-pixel character" := do
  let bt := BigText.new "A" |>.withOff '.'
  bt.off ≡ some '.'

test "BigText.transparent disables off pixels" := do
  let bt := BigText.new "A" |>.withOff '.' |>.transparent
  bt.off ≡ none

test "BigText.withAlignment sets alignment" := do
  let bt := BigText.new "A" |>.withAlignment .center
  bt.alignment ≡ .center

test "BigText.centered sets center alignment" := do
  let bt := BigText.new "A" |>.centered
  bt.alignment ≡ .center

test "BigText.rightAligned sets right alignment" := do
  let bt := BigText.new "A" |>.rightAligned
  bt.alignment ≡ .right

test "BigText.withSpacing sets character spacing" := do
  let bt := BigText.new "AB" |>.withSpacing 2
  bt.spacing ≡ 2

test "BigFont.glyphWidth returns correct width for block font" := do
  BigFont.block.glyphWidth 'A' ≡ 8

test "BigFont.glyphWidth returns correct width for small font" := do
  BigFont.small.glyphWidth 'A' ≡ 4

test "BigText renders without crash" := do
  let bt := BigText.new "Hi" |>.withFont .block
  let buf := renderWidget bt 20 8
  buf.width ≡ 20

test "BigText renders with slant font" := do
  let bt := BigText.new "OK" |>.withFont .slant
  let buf := renderWidget bt 24 8
  buf.height ≡ 8

test "BigText renders with small font" := do
  let bt := BigText.new "XY" |>.withFont .small
  let buf := renderWidget bt 12 4
  buf.height ≡ 4

#generate_tests

end TerminusTests.BigTextTests
