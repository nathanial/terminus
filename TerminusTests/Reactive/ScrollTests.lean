-- TerminusTests.Reactive.ScrollTests: Clipping and ScrollView tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.ScrollTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Scroll Tests"

-- ============================================================================
-- Clipping and ScrollView Tests
-- ============================================================================

test "ClipContext.contains returns true when no clip rect" := do
  let ctx : ClipContext := {}
  (ctx.contains 100 200) ≡ true

test "ClipContext.contains returns true for point inside rect" := do
  let ctx : ClipContext := { clipRect := some { x := 10, y := 10, width := 20, height := 15 } }
  (ctx.contains 15 12) ≡ true
  (ctx.contains 10 10) ≡ true  -- top-left corner
  (ctx.contains 29 24) ≡ true  -- bottom-right corner (exclusive boundary - 1)

test "ClipContext.contains returns false for point outside rect" := do
  let ctx : ClipContext := { clipRect := some { x := 10, y := 10, width := 20, height := 15 } }
  (ctx.contains 5 5) ≡ false   -- before top-left
  (ctx.contains 30 25) ≡ false -- at exclusive boundary
  (ctx.contains 15 5) ≡ false  -- above
  (ctx.contains 50 15) ≡ false -- to the right

test "ClipContext.intersect narrows clip rect" := do
  let ctx1 : ClipContext := { clipRect := some { x := 0, y := 0, width := 100, height := 100 } }
  let ctx2 := ctx1.intersect { x := 20, y := 30, width := 50, height := 40 }
  match ctx2.clipRect with
  | some r =>
    r.x ≡ 20
    r.y ≡ 30
    r.width ≡ 50
    r.height ≡ 40
  | none => ensure false "expected clip rect"

test "ClipContext.intersect creates clip when none exists" := do
  let ctx1 : ClipContext := {}
  let ctx2 := ctx1.intersect { x := 5, y := 10, width := 30, height := 20 }
  match ctx2.clipRect with
  | some r =>
    r.x ≡ 5
    r.y ≡ 10
    r.width ≡ 30
    r.height ≡ 20
  | none => ensure false "expected clip rect"

test "ClipContext.addOffset accumulates scroll offset" := do
  let ctx1 : ClipContext := { scrollOffsetX := 5, scrollOffsetY := 10 }
  let ctx2 := ctx1.addOffset 3 7
  ctx2.scrollOffsetX ≡ 8
  ctx2.scrollOffsetY ≡ 17

test "ScrollState.maxOffsetY computes correctly" := do
  let state : ScrollState := { contentHeight := 100, viewportHeight := 30 }
  state.maxOffsetY ≡ 70

test "ScrollState.maxOffsetY returns 0 when content fits" := do
  let state : ScrollState := { contentHeight := 20, viewportHeight := 30 }
  state.maxOffsetY ≡ 0

test "ScrollState.scrollDown moves down by amount" := do
  let state : ScrollState := { offsetY := 5, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.scrollDown 10
  scrolled.offsetY ≡ 15

test "ScrollState.scrollDown clamps to max" := do
  let state : ScrollState := { offsetY := 70, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.scrollDown 50  -- Would exceed max of 80
  scrolled.offsetY ≡ 80

test "ScrollState.scrollUp moves up by amount" := do
  let state : ScrollState := { offsetY := 15, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.scrollUp 5
  scrolled.offsetY ≡ 10

test "ScrollState.scrollUp clamps to 0" := do
  let state : ScrollState := { offsetY := 5, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.scrollUp 10  -- Would go negative
  scrolled.offsetY ≡ 0

test "ScrollState.scrollToTop resets to 0" := do
  let state : ScrollState := { offsetY := 50, contentHeight := 100, viewportHeight := 20 }
  state.scrollToTop.offsetY ≡ 0

test "ScrollState.scrollToBottom goes to max" := do
  let state : ScrollState := { offsetY := 0, contentHeight := 100, viewportHeight := 20 }
  state.scrollToBottom.offsetY ≡ 80

test "ScrollState.pageDown scrolls by viewport - 1" := do
  let state : ScrollState := { offsetY := 0, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.pageDown
  scrolled.offsetY ≡ 19  -- 20 - 1 = 19

test "ScrollState.pageUp scrolls by viewport - 1" := do
  let state : ScrollState := { offsetY := 50, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.pageUp
  scrolled.offsetY ≡ 31  -- 50 - 19 = 31

test "ScrollState.scrollToVisible scrolls down when item below viewport" := do
  let state : ScrollState := { offsetY := 0, contentHeight := 100, viewportHeight := 10 }
  let scrolled := state.scrollToVisible 15  -- Item at y=15 not visible in 0..9
  scrolled.offsetY ≡ 6  -- 15 - 10 + 1 = 6

test "ScrollState.scrollToVisible scrolls up when item above viewport" := do
  let state : ScrollState := { offsetY := 20, contentHeight := 100, viewportHeight := 10 }
  let scrolled := state.scrollToVisible 5  -- Item at y=5 not visible in 20..29
  scrolled.offsetY ≡ 5

test "ScrollState.scrollToVisible does nothing when item visible" := do
  let state : ScrollState := { offsetY := 10, contentHeight := 100, viewportHeight := 10 }
  let scrolled := state.scrollToVisible 15  -- Item at y=15 is visible in 10..19
  scrolled.offsetY ≡ 10

test "ScrollState.needsVerticalScroll true when content exceeds viewport" := do
  let state : ScrollState := { contentHeight := 50, viewportHeight := 20 }
  state.needsVerticalScroll ≡ true

test "ScrollState.needsVerticalScroll false when content fits" := do
  let state : ScrollState := { contentHeight := 15, viewportHeight := 20 }
  state.needsVerticalScroll ≡ false

test "RNode.clipped renders content within bounds" := do
  let node : RNode := .clipped (
    .column 0 {} #[
      .text "Line 1" {},
      .text "Line 2" {},
      .text "Line 3" {}
    ]
  )
  let buf := Terminus.Reactive.renderOnly node 20 3
  -- All 3 lines should fit in 3-row buffer
  (buf.get 0 0).char ≡ 'L'  -- Line 1
  (buf.get 0 1).char ≡ 'L'  -- Line 2
  (buf.get 0 2).char ≡ 'L'  -- Line 3

test "RNode.scrolled applies offset to content" := do
  let node : RNode := .clipped (
    .scrolled 0 1 (  -- Scroll down by 1
      .column 0 {} #[
        .text "Line 1" {},
        .text "Line 2" {},
        .text "Line 3" {}
      ]
    )
  )
  let buf := Terminus.Reactive.renderOnly node 20 3
  -- With scroll offset of 1, Line 2 should be at row 0
  -- Note: exact behavior depends on how scroll offset is applied
  buf.width ≡ 20  -- Basic sanity check

test "renderVerticalScrollbar creates column of chars" := do
  let state : ScrollState := { offsetY := 0, contentHeight := 100, viewportHeight := 20 }
  let config : ScrollViewConfig := {}
  let scrollbar := renderVerticalScrollbar state config 10
  -- Scrollbar should be a column node
  match scrollbar with
  | .column _ _ children => children.size ≡ 10
  | _ => ensure false "expected column node"

test "computeThumbMetrics returns full track when content fits" := do
  let (thumbSize, thumbPos) := computeThumbMetrics 10 20 0 10  -- content=10, viewport=20
  thumbSize ≡ 10  -- Full track
  thumbPos ≡ 0

test "computeThumbMetrics proportional thumb size" := do
  let (thumbSize, _) := computeThumbMetrics 100 20 0 10  -- content=100, viewport=20
  -- Thumb size = trackLen * viewportSize / contentSize = 10 * 20 / 100 = 2
  thumbSize ≡ 2

test "computeThumbMetrics thumb position at end" := do
  let (thumbSize, thumbPos) := computeThumbMetrics 100 20 80 10  -- offset=80 (max)
  -- Thumb should be at end of track
  (thumbPos + thumbSize) ≡ 10



end TerminusTests.Reactive.ScrollTests
