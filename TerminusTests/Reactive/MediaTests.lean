-- TerminusTests.Reactive.MediaTests: Tests for media widgets (Canvas, Image, BigText)

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.MediaTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Media Widget Tests"

-- ============================================================================
-- BrailleGrid Tests
-- ============================================================================

test "BrailleGrid.new creates empty grid" := do
  let grid := BrailleGrid.new 10 5
  grid.cellWidth ≡ 10
  grid.cellHeight ≡ 5
  grid.patterns.size ≡ 50  -- 10 * 5
  grid.styles.size ≡ 50

test "BrailleGrid pixel dimensions are 2x width, 4x height" := do
  let grid := BrailleGrid.new 20 10
  grid.pixelWidth ≡ 40  -- 20 * 2
  grid.pixelHeight ≡ 40  -- 10 * 4

test "BrailleGrid.setPixel sets correct Braille dot" := do
  let grid := BrailleGrid.new 5 5
  let grid' := grid.setPixel 0 0 {}
  let (char, _) := grid'.getCell 0 0
  -- Should have at least one dot set
  (char != '⠀') ≡ true

test "BrailleGrid.clearPixel removes dot" := do
  let grid := BrailleGrid.new 5 5
  let grid' := grid.setPixel 0 0 {}
  let grid'' := grid'.clearPixel 0 0
  let (char, _) := grid''.getCell 0 0
  -- Should be empty now
  (char == '⠀') ≡ true

test "BrailleGrid.clear resets entire grid" := do
  let grid := BrailleGrid.new 5 5
  let grid' := grid.setPixel 0 0 {}
  let grid' := grid'.setPixel 5 5 {}
  let grid'' := grid'.clear
  -- All patterns should be 0
  let allZero := grid''.patterns.all (· == 0)
  allZero ≡ true

test "BrailleGrid.setPixel ignores out-of-bounds" := do
  let grid := BrailleGrid.new 5 5
  -- Pixel coordinates: width = 10 (5*2), height = 20 (5*4)
  let grid' := grid.setPixel 100 100 {}  -- Way out of bounds
  -- Grid should be unchanged
  let allZero := grid'.patterns.all (· == 0)
  allZero ≡ true

test "BrailleGrid.drawLine draws between two points" := do
  let grid := BrailleGrid.new 10 5
  let grid' := grid.drawLine 0 0 10 10 {}
  -- Should have some non-empty cells along the diagonal
  let hasContent := grid'.patterns.any (· != 0)
  hasContent ≡ true

test "BrailleGrid.drawRect draws rectangle outline" := do
  let grid := BrailleGrid.new 10 10
  let grid' := grid.drawRect 2 2 6 6 {}
  -- Should have content
  let hasContent := grid'.patterns.any (· != 0)
  hasContent ≡ true

test "BrailleGrid.fillRect fills rectangle" := do
  let grid := BrailleGrid.new 10 10
  let grid' := grid.fillRect 0 0 4 4 {}
  -- Should have multiple non-empty cells
  let filledCount := grid'.patterns.filter (· != 0) |>.size
  (filledCount > 0) ≡ true

test "BrailleGrid.drawCircle draws circle outline" := do
  let grid := BrailleGrid.new 10 10
  let grid' := grid.drawCircle 10 10 5 {}
  -- Should have content
  let hasContent := grid'.patterns.any (· != 0)
  hasContent ≡ true

test "BrailleGrid.toRNodes creates row nodes" := do
  let grid := BrailleGrid.new 5 3
  let rows := grid.toRNodes
  rows.size ≡ 3  -- One RNode per cell row

-- ============================================================================
-- Canvas Widget Tests
-- ============================================================================

test "canvas' creates canvas with drawing operations" := do
  runSpider do
    let (events, _) ← createInputs

    let (canvas, render) ← (runWidget do
      canvas' { width := 20, height := 10 }
    ).run events

    -- Should have drawing operations available
    -- Just test that it compiles and runs
    let node ← SpiderM.liftIO render.sample
    -- Canvas renders something
    pure ()

test "staticCanvas' renders a pre-built grid" := do
  runSpider do
    let (events, _) ← createInputs

    let grid := BrailleGrid.new 10 5
      |>.setPixel 0 0 {}
      |>.setPixel 5 5 {}

    let (_, render) ← (runWidget do
      staticCanvas' grid
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should render the grid

-- ============================================================================
-- Image Widget Tests
-- ============================================================================

test "ImageConfig has sensible defaults" := do
  let config : ImageConfig := {}
  config.width ≡ 40
  config.height ≡ 20
  config.preserveAspect ≡ true
  config.altText ≡ "Image"

test "image' creates image node" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      image' "/path/to/image.png" { width := 30, height := 15 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should create an image node

test "imageFromBytes' creates image from bytes" := do
  runSpider do
    let (events, _) ← createInputs

    let bytes : ByteArray := ByteArray.mk #[0x89, 0x50, 0x4E, 0x47]  -- PNG magic

    let (_, render) ← (runWidget do
      imageFromBytes' bytes { width := 40, height := 20, altText := "Test Image" }
    ).run events

    let node ← SpiderM.liftIO render.sample

test "imagePlaceholder' shows alt text" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      imagePlaceholder' "Loading..." {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Loading") "expected alt text")

-- ============================================================================
-- BigFont Tests
-- ============================================================================

test "BigFont.block creates 8x8 font" := do
  let font := BigFont.block
  font.height ≡ 8

test "BigFont.slant creates slanted 8x8 font" := do
  let font := BigFont.slant
  font.height ≡ 8

test "BigFont.small creates 4x4 font" := do
  let font := BigFont.small
  font.height ≡ 4

test "BigFont glyph for 'A' has content" := do
  let font := BigFont.block
  let glyph := font.glyph 'A'
  glyph.size ≡ 8
  -- Should have some non-space content
  let hasContent := glyph.any (fun row => row.any (· != ' '))
  hasContent ≡ true

test "BigFont glyph for space is empty" := do
  let font := BigFont.block
  let glyph := font.glyph ' '
  -- Space should be all spaces
  let isEmpty := glyph.all (fun row => row.all (· == ' '))
  isEmpty ≡ true

test "BigFont.glyphWidth returns width" := do
  let font := BigFont.block
  let width := BigFont.glyphWidth font 'A'
  width ≡ 8  -- 8x8 font

-- ============================================================================
-- BigText Widget Tests
-- ============================================================================

test "BigTextConfig has sensible defaults" := do
  let config : BigTextConfig := {}
  config.font ≡ BigTextFont.block
  config.onChar ≡ '█'
  config.spacing ≡ 1
  config.alignment ≡ Alignment.left

test "bigText' renders big text" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      bigText' "HI" { font := .block }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain the on character
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected on char")

test "bigText' with small font" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      bigText' "AB" { font := .small }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain the on character
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected on char")

test "bigText' with slant font" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      bigText' "XY" { font := .slant }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain the on character
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected on char")

test "bigText' with custom on/off chars" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      bigText' "A" { onChar := '#', offChar := some '.' }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "#") "expected custom on char")

test "bigTextMultiline' renders multiple lines" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      bigTextMultiline' "A\nB" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain the on character
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected on char")

test "bigText' handles empty string" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      bigText' "" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should not crash

-- ============================================================================
-- RNode Image Variant Tests
-- ============================================================================

test "RNode.image creates image node" := do
  let node := RNode.image (.path "/test.png") .iterm2 40 20 true "Test"
  -- Should be an image node (just verify it compiles)
  pure ()

test "naturalWidth for image node" := do
  let node := RNode.image (.path "/test.png") .iterm2 40 20 true "Test"
  let width := naturalWidth node
  width ≡ 40

test "naturalHeight for image node" := do
  let node := RNode.image (.path "/test.png") .iterm2 40 20 true "Test"
  let height := naturalHeight node
  height ≡ 20



end TerminusTests.Reactive.MediaTests
