-- TerminusTests.Reactive.LayoutTests: Tests for split pane layouts

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.LayoutTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Layout Tests"

-- ============================================================================
-- Horizontal split tests
-- ============================================================================

test "horizontalSplit' creates row with children" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← horizontalSplit' 50 {}
        (do text' "Left" {})
        (do text' "Right" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .row _ _ children =>
      -- Should have 3 children: left, divider, right (when showDivider is true by default)
      SpiderM.liftIO (children.size ≡ 3)
    | _ => SpiderM.liftIO (ensure false "expected row node")

test "horizontalSplit' without divider creates row with two children" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← horizontalSplit' 50 { showDivider := false }
        (do text' "Left" {})
        (do text' "Right" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .row _ _ children =>
      SpiderM.liftIO (children.size ≡ 2)
    | _ => SpiderM.liftIO (ensure false "expected row node")

test "horizontalSplit' renders left pane content" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← horizontalSplit' 50 {}
        (do text' "Left Content" {})
        (do text' "Right Content" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Left Content") "should contain left content")

test "horizontalSplit' renders right pane content" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← horizontalSplit' 50 {}
        (do text' "Left Content" {})
        (do text' "Right Content" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Right Content") "should contain right content")

-- ============================================================================
-- Vertical split tests
-- ============================================================================

test "verticalSplit' creates column with children" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← verticalSplit' 50 {}
        (do text' "Top" {})
        (do text' "Bottom" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .column _ _ children =>
      -- Should have 3 children: top, divider, bottom
      SpiderM.liftIO (children.size ≡ 3)
    | _ => SpiderM.liftIO (ensure false "expected column node")

test "verticalSplit' without divider creates column with two children" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← verticalSplit' 50 { showDivider := false }
        (do text' "Top" {})
        (do text' "Bottom" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .column _ _ children =>
      SpiderM.liftIO (children.size ≡ 2)
    | _ => SpiderM.liftIO (ensure false "expected column node")

test "verticalSplit' renders top pane content" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← verticalSplit' 50 {}
        (do text' "Top Content" {})
        (do text' "Bottom Content" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Top Content") "should contain top content")

test "verticalSplit' renders bottom pane content" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← verticalSplit' 50 {}
        (do text' "Top Content" {})
        (do text' "Bottom Content" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Bottom Content") "should contain bottom content")

-- ============================================================================
-- splitPane' generic tests
-- ============================================================================

test "splitPane' with horizontal direction creates row" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← splitPane' .horizontal 50 { showDivider := false }
        (do text' "A" {})
        (do text' "B" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .row _ _ _ => SpiderM.liftIO (pure ())
    | _ => SpiderM.liftIO (ensure false "expected row node")

test "splitPane' with vertical direction creates column" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← splitPane' .vertical 50 { showDivider := false }
        (do text' "A" {})
        (do text' "B" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .column _ _ _ => SpiderM.liftIO (pure ())
    | _ => SpiderM.liftIO (ensure false "expected column node")

-- ============================================================================
-- Three-column split tests
-- ============================================================================

test "threeColumnSplit' creates row with three panes" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← threeColumnSplit' 33 33 { showDivider := false }
        (do text' "Left" {})
        (do text' "Center" {})
        (do text' "Right" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .row _ _ children =>
      SpiderM.liftIO (children.size ≡ 3)
    | _ => SpiderM.liftIO (ensure false "expected row node")

test "threeColumnSplit' with dividers creates row with five elements" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← threeColumnSplit' 33 33 {}
        (do text' "Left" {})
        (do text' "Center" {})
        (do text' "Right" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .row _ _ children =>
      -- 3 panes + 2 dividers = 5 children
      SpiderM.liftIO (children.size ≡ 5)
    | _ => SpiderM.liftIO (ensure false "expected row node")

-- ============================================================================
-- Sidebar layout tests
-- ============================================================================

test "sidebarLayout' left position contains both panes" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← sidebarLayout' 20 .left {}
        (do text' "Sidebar" {})
        (do text' "Content" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Sidebar") "should contain sidebar")
    SpiderM.liftIO (ensure (rnodeContainsText node "Content") "should contain content")

test "sidebarLayout' right position contains both panes" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← sidebarLayout' 20 .right {}
        (do text' "Sidebar" {})
        (do text' "Content" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Sidebar") "should contain sidebar")
    SpiderM.liftIO (ensure (rnodeContainsText node "Content") "should contain content")

-- ============================================================================
-- Header/Footer layout tests
-- ============================================================================

test "headerLayout' renders header and content" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← headerLayout' 3 {}
        (do text' "Header" {})
        (do text' "Main Content" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Header") "should contain header")
    SpiderM.liftIO (ensure (rnodeContainsText node "Main Content") "should contain main content")

test "footerLayout' renders content and footer" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← footerLayout' 2 {}
        (do text' "Main Content" {})
        (do text' "Footer" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Main Content") "should contain main content")
    SpiderM.liftIO (ensure (rnodeContainsText node "Footer") "should contain footer")

test "headerFooterLayout' renders all three sections" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← headerFooterLayout' {}
        (do text' "Header" {})
        (do text' "Body" {})
        (do text' "Footer" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Header") "should contain header")
    SpiderM.liftIO (ensure (rnodeContainsText node "Body") "should contain body")
    SpiderM.liftIO (ensure (rnodeContainsText node "Footer") "should contain footer")

-- ============================================================================
-- Nested split tests
-- ============================================================================

test "nested splits work correctly" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← horizontalSplit' 50 { showDivider := false }
        (do
          let _ ← verticalSplit' 50 { showDivider := false }
            (do text' "TopLeft" {})
            (do text' "BottomLeft" {})
        )
        (do text' "Right" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "TopLeft") "should contain TopLeft")
    SpiderM.liftIO (ensure (rnodeContainsText node "BottomLeft") "should contain BottomLeft")
    SpiderM.liftIO (ensure (rnodeContainsText node "Right") "should contain Right")



end TerminusTests.Reactive.LayoutTests
