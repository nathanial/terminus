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



end TerminusTests.Reactive.UtilityTests
