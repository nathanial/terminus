-- TerminusTests.Reactive.AsyncTests: Tests for async utilities

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.AsyncTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Async Tests"

-- ============================================================================
-- AsyncResult structure tests (using WidgetM versions)
-- ============================================================================

test "useAsyncW returns AsyncResult with loading false initially" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let (trigger, _fireTrigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let asyncResult ← useAsyncW (pure "test") trigger
      let loading ← asyncResult.loading.sample
      if loading then
        text' "loading" {}
      else
        text' "not loading" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "not loading") "should initially not be loading")

test "useAsyncW result is none before trigger" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let (trigger, _fireTrigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let asyncResult ← useAsyncW (pure "test") trigger
      let result ← asyncResult.result.sample
      match result with
      | none => text' "none" {}
      | some data => text' data {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "none") "result should be none initially")

test "useAsyncOnceW creates AsyncResult" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let asyncResult ← useAsyncOnceW (pure "data")
      -- Check structure exists
      let _loading ← asyncResult.loading.sample
      let _result ← asyncResult.result.sample
      text' "ok" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "ok") "should create AsyncResult")

-- ============================================================================
-- StreamResult structure tests
-- ============================================================================

test "useStreamW returns StreamResult with empty chunks initially" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let (trigger, _fireTrigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let streamAction : IO (Option String) := pure none
      let streamResult ← useStreamW streamAction trigger
      let chunks ← streamResult.chunks.sample
      if chunks.isEmpty then
        text' "empty" {}
      else
        text' "has data" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "empty") "chunks should be empty initially")

test "useStreamW is not streaming initially" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let (trigger, _fireTrigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let streamAction : IO (Option String) := pure none
      let streamResult ← useStreamW streamAction trigger
      let streaming ← streamResult.streaming.sample
      if streaming then
        text' "streaming" {}
      else
        text' "not streaming" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "not streaming") "should not be streaming initially")

-- ============================================================================
-- Debounce async tests
-- ============================================================================

test "useDebounceAsyncW creates event" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let (inputEvent, _fireInput) ← Reactive.newTriggerEvent (t := Spider) (a := String)
      let action := fun (s : String) => pure s.toUpper
      let _resultEvent ← useDebounceAsyncW 10 action inputEvent
      text' "ok" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "ok") "debounce event should be created")

-- ============================================================================
-- Integration tests with emitDynamic
-- ============================================================================

test "useAsyncW loading state can be rendered with emitDynamic" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let (trigger, _fireTrigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let asyncResult ← useAsyncW (pure "widget test") trigger
      let node ← asyncResult.loading.map' (fun loading =>
        if loading then
          RNode.text "loading" {}
        else
          RNode.text "idle" {}
      )
      emit node
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "idle") "should show idle state")

test "useAsyncOnceW result can be rendered with emitDynamic" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let asyncResult ← useAsyncOnceW (pure "once test")
      let node ← asyncResult.result.map' (fun result =>
        match result with
        | none => RNode.text "pending" {}
        | some data => RNode.text data {}
      )
      emit node
    ).run events

    -- The async may or may not have completed by now - just verify structure works
    let _node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (pure ())

test "StreamResult chunks can be displayed dynamically" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let (trigger, _) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let streamAction : IO (Option String) := pure none
      let streamResult ← useStreamW streamAction trigger
      let node ← streamResult.chunks.map' (fun chunks =>
        if chunks.isEmpty then
          RNode.text "no chunks" {}
        else
          let chunkText := String.intercalate ", " chunks.toList
          RNode.text chunkText {}
      )
      emit node
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "no chunks") "should show no chunks initially")



end TerminusTests.Reactive.AsyncTests
