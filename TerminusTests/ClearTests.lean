-- TerminusTests.ClearTests: Tests for Clear widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Widgets.Widget
import Terminus.Widgets.Clear

namespace TerminusTests.ClearTests

open Terminus
open Crucible

testSuite "Clear Widget Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Clear.new creates default clear widget" := do
  let clear := Clear.new
  clear.style ≡ {}

test "Clear.withStyle applies style" := do
  let clear := Clear.new |>.withStyle (Style.fgColor Color.red)
  clear.style.fg ≡ Color.red

test "Clear.withBg sets background color" := do
  let clear := Clear.new |>.withBg Color.blue
  clear.style.bg ≡ Color.blue

test "Clear renders empty cells with default style" := do
  let clear := Clear.new
  let buf := renderWidget clear 5 3
  -- Should fill with empty cells
  (buf.get 0 0).char ≡ ' '
  (buf.get 2 1).char ≡ ' '

test "Clear renders styled cells when style set" := do
  let clear := Clear.new |>.withBg Color.green
  let buf := renderWidget clear 4 2
  (buf.get 0 0).style.bg ≡ Color.green
  (buf.get 3 1).style.bg ≡ Color.green

#generate_tests

end TerminusTests.ClearTests
