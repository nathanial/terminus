-- TerminusTests.Reactive.WidgetTests: WidgetM and container tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.WidgetTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Widget Tests"

-- ============================================================================
-- WidgetM and emit tests
-- ============================================================================

test "emit adds render function to state" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      emitStatic (RNode.text "Test" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .text content _ => SpiderM.liftIO (content ≡ "Test")
    | _ => SpiderM.liftIO (ensure false "expected text node")

test "multiple emit creates column of nodes" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      emitStatic (RNode.text "First" {})
      emitStatic (RNode.text "Second" {})
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .column _ _ children =>
      SpiderM.liftIO (children.size ≡ 2)
    | _ => SpiderM.liftIO (ensure false "expected column node")

test "text' emits text node" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      text' "Hello" { fg := .ansi .red }
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .text content style =>
      SpiderM.liftIO (content ≡ "Hello")
      SpiderM.liftIO (style.fg ≡ .ansi .red)
    | _ => SpiderM.liftIO (ensure false "expected text node")

-- ============================================================================
-- Container tests
-- ============================================================================

test "column' collects children renders" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      column' (gap := 1) (style := {}) do
        text' "Line1" {}
        text' "Line2" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .column gap _ children =>
      SpiderM.liftIO (gap ≡ 1)
      SpiderM.liftIO (children.size ≡ 2)
    | _ => SpiderM.liftIO (ensure false "expected column node")

test "row' collects children renders" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      row' (gap := 2) (style := {}) do
        text' "A" {}
        text' "B" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    match node with
    | .row gap _ children =>
      SpiderM.liftIO (gap ≡ 2)
      SpiderM.liftIO (children.size ≡ 2)
    | _ => SpiderM.liftIO (ensure false "expected row node")



end TerminusTests.Reactive.WidgetTests
