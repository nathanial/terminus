-- TerminusTests.Reactive.UtilityTests: Tests for utility widgets (Scrollbar, Paragraph, Popup, Clear)

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.UtilityTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Utility Widget Tests"

-- ============================================================================
-- Scrollbar Tests
-- ============================================================================

test "ScrollbarConfig has sensible defaults" := do
  let config : ScrollbarConfig := {}
  config.orientation ≡ ScrollbarOrientation.vertical
  config.trackChar ≡ '░'
  config.thumbChar ≡ '█'
  config.length ≡ 10

test "scrollbar' renders vertical scrollbar" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      scrollbar' 0 100 20 { length := 5 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain thumb character
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected thumb char")

test "scrollbar' renders horizontal scrollbar" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      hScrollbar' 0 50 10 20
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected thumb char")

-- ============================================================================
-- Paragraph Tests
-- ============================================================================

test "TextSpan.new creates plain span" := do
  let span := TextSpan.new "Hello"
  span.content ≡ "Hello"

test "TextSpan.styled creates styled span" := do
  let style : Style := { fg := .ansi .red }
  let span := TextSpan.styled "Error" style
  span.content ≡ "Error"

test "TextLine.new creates line from text" := do
  let line := TextLine.new "Hello world"
  line.width ≡ 11

test "TextLine.text returns full content" := do
  let line := TextLine.fromSpans #[
    TextSpan.new "Hello ",
    TextSpan.new "world"
  ]
  line.text ≡ "Hello world"

test "ParagraphConfig has sensible defaults" := do
  let config : ParagraphConfig := {}
  config.alignment ≡ TextAlignment.left
  config.wrapMode ≡ WrapMode'.noWrap

test "paragraph' renders text" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      paragraph' "Hello\nWorld" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Hello") "expected Hello")
    SpiderM.liftIO (ensure (rnodeContainsText node "World") "expected World")

test "centeredParagraph' centers text" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      centeredParagraph' "Test" { maxWidth := some 20 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Test") "expected Test")

-- ============================================================================
-- Popup Tests
-- ============================================================================

test "PopupConfig has sensible defaults" := do
  let config : PopupConfig := {}
  config.borderType ≡ BorderType.double
  config.title ≡ none

test "popup' returns control functions" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, _) ← (runWidget do
      popup' "test-popup" {} do
        text' "Content" {}
    ).run events

    -- Verify visible dynamic starts false
    let visible ← SpiderM.liftIO result.visible.sample
    SpiderM.liftIO (visible ≡ false)

test "popup' renders nothing when hidden" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      popup' "test-popup" {} do
        text' "Hidden Content" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- When hidden, should be empty
    SpiderM.liftIO (ensure (not (rnodeContainsText node "Hidden Content")) "popup should be hidden")

test "messagePopup' creates titled popup" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, _) ← (runWidget do
      messagePopup' "msg" "Title" "Message text" {}
    ).run events

    -- Just verify it created successfully
    let visible ← SpiderM.liftIO result.visible.sample
    SpiderM.liftIO (visible ≡ false)

-- ============================================================================
-- Clear Widget Tests
-- ============================================================================

test "ClearConfig has sensible defaults" := do
  let config : ClearConfig := {}
  config.fillChar ≡ ' '
  config.width ≡ 1
  config.height ≡ 1

test "clear' renders filled area" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      clear' { width := 5, height := 3 }
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Should render something (spaces)
    -- Just verify we got a node (not testing equality)

test "hSpace' creates horizontal space" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      hSpace' 10
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Just verify we got a node (not testing equality)

test "vSpace' creates vertical space" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      vSpace' 5
    ).run events

    let _node ← SpiderM.liftIO render.sample
    -- Just verify we got a node (not testing equality)

test "filledRect' creates filled rectangle" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      filledRect' 10 5 '█' { fg := .ansi .blue }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "█") "expected fill char")

test "hSeparator' creates horizontal line" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      hSeparator' 20 '─' {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "─") "expected separator char")

test "vSeparator' creates vertical line" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      vSeparator' 10 '│' {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "│") "expected separator char")

-- ============================================================================
-- Unicode Display Width Tests
-- ============================================================================

test "TextLine.width counts ASCII correctly" := do
  let line := TextLine.new "Hello"
  line.width ≡ 5

test "TextLine.width counts CJK as width 2" := do
  let line := TextLine.new "世界"
  -- Each CJK character is width 2, so "世界" = 4
  line.width ≡ 4

test "TextLine.width counts mixed ASCII and CJK" := do
  let line := TextLine.new "Hi世界"
  -- "Hi" = 2, "世界" = 4, total = 6
  line.width ≡ 6

test "TextLine.width counts emoji as width 2" := do
  let line := TextLine.new "Hello 🎉"
  -- "Hello " = 6, "🎉" = 2, total = 8
  line.width ≡ 8

test "String.displayWidth handles ASCII" := do
  "hello".displayWidth ≡ 5

test "String.displayWidth handles CJK" := do
  "你好".displayWidth ≡ 4

test "String.displayWidth handles mixed content" := do
  "Hello 世界!".displayWidth ≡ 11  -- 6 + 4 + 1

-- ============================================================================
-- Word Wrap Unicode Tests
-- ============================================================================

test "paragraph' with wrap handles CJK correctly" := do
  runSpider do
    let (events, _) ← createInputs
    -- "Hello 世界" with maxWidth=8 should wrap after "Hello" (width 5+1+4=10 > 8)
    let (_, render) ← (runWidget do
      paragraph' "Hello 世界" { wrapMode := .wrap, maxWidth := some 8 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Hello") "expected Hello")
    SpiderM.liftIO (ensure (rnodeContainsText node "世界") "expected 世界")

-- ============================================================================
-- sliceByDisplayWidth Tests
-- ============================================================================

test "sliceByDisplayWidth extracts ASCII substring" := do
  let result := sliceByDisplayWidth "Hello World" 0 5
  result ≡ "Hello"

test "sliceByDisplayWidth handles offset" := do
  let result := sliceByDisplayWidth "Hello World" 6 5
  result ≡ "World"

test "sliceByDisplayWidth handles CJK" := do
  -- "世界" is 4 columns wide
  let result := sliceByDisplayWidth "世界你好" 0 4
  result ≡ "世界"

test "sliceByDisplayWidth handles CJK with offset" := do
  -- Skip first 4 columns (世界), get next 4 (你好)
  let result := sliceByDisplayWidth "世界你好" 4 4
  result ≡ "你好"

test "sliceByDisplayWidth handles mixed content" := do
  -- "Hi世界" = Hi(2) + 世(2) + 界(2) = 6 total
  let result := sliceByDisplayWidth "Hi世界" 2 4
  result ≡ "世界"

test "sliceByDisplayWidth returns empty for zero width" := do
  let result := sliceByDisplayWidth "Hello" 0 0
  result ≡ ""

test "sliceByDisplayWidth includes partial overlap characters" := do
  -- If a wide char starts before endCol but extends past startCol, include it
  let result := sliceByDisplayWidth "世界" 1 2
  -- "世" starts at col 0, ends at 2; "界" starts at 2, ends at 4
  -- With startCol=1, endCol=3, both overlap, so both included
  result ≡ "世界"

-- ============================================================================
-- ParagraphScrollState Tests
-- ============================================================================

test "ParagraphScrollState.scrollRight clamps at max" := do
  let state : ParagraphScrollState := { offsetX := 0, contentWidth := 100, viewportWidth := 20 }
  let scrolled := state.scrollRight 100  -- Try to scroll way past end
  scrolled.offsetX ≡ 80  -- Max is contentWidth - viewportWidth = 80

test "ParagraphScrollState.scrollLeft clamps at zero" := do
  let state : ParagraphScrollState := { offsetX := 5, contentWidth := 100, viewportWidth := 20 }
  let scrolled := state.scrollLeft 10  -- Try to scroll past start
  scrolled.offsetX ≡ 0

test "ParagraphScrollState.scrollToStart resets offset" := do
  let state : ParagraphScrollState := { offsetX := 50, contentWidth := 100, viewportWidth := 20 }
  let scrolled := state.scrollToStart
  scrolled.offsetX ≡ 0

test "ParagraphScrollState.scrollToEnd goes to max" := do
  let state : ParagraphScrollState := { offsetX := 0, contentWidth := 100, viewportWidth := 20 }
  let scrolled := state.scrollToEnd
  scrolled.offsetX ≡ 80

test "ParagraphScrollState.isScrollable true when content wider" := do
  let state : ParagraphScrollState := { offsetX := 0, contentWidth := 100, viewportWidth := 20 }
  state.isScrollable ≡ true

test "ParagraphScrollState.isScrollable false when content fits" := do
  let state : ParagraphScrollState := { offsetX := 0, contentWidth := 15, viewportWidth := 20 }
  state.isScrollable ≡ false

test "ParagraphScrollState.canScrollLeft true when offset > 0" := do
  let state : ParagraphScrollState := { offsetX := 10, contentWidth := 100, viewportWidth := 20 }
  state.canScrollLeft ≡ true

test "ParagraphScrollState.canScrollRight true when more content" := do
  let state : ParagraphScrollState := { offsetX := 0, contentWidth := 100, viewportWidth := 20 }
  state.canScrollRight ≡ true

test "ParagraphScrollState.canScrollRight false at end" := do
  let state : ParagraphScrollState := { offsetX := 80, contentWidth := 100, viewportWidth := 20 }
  state.canScrollRight ≡ false

-- ============================================================================
-- Scrollable Paragraph Widget Tests
-- ============================================================================

test "ScrollableParagraphConfig has sensible defaults" := do
  let config : ScrollableParagraphConfig := {}
  config.viewportWidth ≡ 80
  config.scrollStep ≡ 4
  config.showScrollIndicators ≡ true

test "scrollableParagraph' creates widget with scroll state" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, _) ← (runWidget do
      scrollableParagraph' "test-scroll" "This is a very long text that should be scrollable" { viewportWidth := 20 }
    ).run events

    let state ← SpiderM.liftIO result.scrollState.sample
    state.offsetX ≡ 0
    state.viewportWidth ≡ 20

test "scrollableParagraph' renders visible portion" := do
  runSpider do
    let (events, _) ← createInputs

    let (_, render) ← (runWidget do
      scrollableParagraph' "test-scroll" "Hello World" { viewportWidth := 5 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Hello") "expected Hello in viewport")

end TerminusTests.Reactive.UtilityTests
