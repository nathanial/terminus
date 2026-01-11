-- TerminusTests.CanvasTests: Tests for Canvas widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Widgets.Widget
import Terminus.Widgets.Canvas

namespace TerminusTests.CanvasTests

open Terminus
open Crucible

testSuite "Canvas Widget Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "BrailleGrid.new creates grid with correct dimensions" := do
  let grid := BrailleGrid.new 10 5
  grid.cellWidth ≡ 10
  grid.cellHeight ≡ 5

test "BrailleGrid.new initializes with empty patterns" := do
  let grid := BrailleGrid.new 3 2
  -- All patterns should be 0 (no dots set)
  grid.patterns.size ≡ 6  -- 3 * 2 cells

test "BrailleGrid.setPixel sets a pixel" := do
  let grid := BrailleGrid.new 2 2
  let grid2 := grid.setPixel 0 0 Style.default
  -- Pixel (0,0) is in cell (0,0), dot position 0
  -- After setting, the pattern should be non-zero
  ensure (grid2.patterns.getD 0 0 != 0) "pixel set"

test "BrailleGrid.clearPixel clears a pixel" := do
  let grid := BrailleGrid.new 2 2
    |>.setPixel 0 0 Style.default
    |>.clearPixel 0 0
  (grid.patterns.getD 0 0) ≡ 0

test "BrailleGrid.clear resets entire grid" := do
  let grid := BrailleGrid.new 2 2
    |>.setPixel 0 0 Style.default
    |>.setPixel 1 1 Style.default
    |>.clear
  (grid.patterns.getD 0 0) ≡ 0

test "BrailleGrid.getCell returns braille character" := do
  let grid := BrailleGrid.new 1 1
  let (char, _) := grid.getCell 0 0
  -- Empty cell should return base braille character U+2800
  char ≡ '⠀'

test "Canvas.new creates empty canvas" := do
  let canvas := Canvas.new
  canvas.shapes.length ≡ 0

test "Canvas.point adds point shape" := do
  let canvas := Canvas.new |>.point 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.line adds line shape" := do
  let canvas := Canvas.new |>.line 0.0 0.0 10.0 10.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.rect adds rectangle shape" := do
  let canvas := Canvas.new |>.rect 0.0 0.0 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.filledRect adds filled rectangle shape" := do
  let canvas := Canvas.new |>.filledRect 0.0 0.0 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.circle adds circle shape" := do
  let canvas := Canvas.new |>.circle 5.0 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.filledCircle adds filled circle shape" := do
  let canvas := Canvas.new |>.filledCircle 5.0 5.0 3.0 Style.default
  canvas.shapes.length ≡ 1

test "Canvas.addShape appends shape to list" := do
  let shape := CanvasShape.point 1.0 1.0 Style.default
  let canvas := Canvas.new |>.addShape shape |>.addShape shape
  canvas.shapes.length ≡ 2

test "Canvas renders without crash" := do
  let canvas := Canvas.new
    |>.line 0.0 0.0 15.0 7.0 Style.default
    |>.circle 8.0 4.0 3.0 (Style.fgColor Color.blue)
  let buf := renderWidget canvas 10 5
  buf.width ≡ 10

test "Canvas renders shapes to braille characters" := do
  let canvas := Canvas.new |>.point 0.0 0.0 Style.default
  let buf := renderWidget canvas 2 2
  -- The character at (0,0) should be a braille character (not space)
  let c := (buf.get 0 0).char
  -- Braille block starts at U+2800
  ensure (c.toNat >= 0x2800 && c.toNat <= 0x28FF) "braille character rendered"

#generate_tests

end TerminusTests.CanvasTests
