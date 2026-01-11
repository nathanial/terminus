-- TerminusTests.Reactive.OverlayTests: Overlay and Modal tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.OverlayTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Overlay Tests"

-- ============================================================================
-- Overlay/Modal Tests
-- ============================================================================

test "overlayWhen' renders nothing when not visible" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, _trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn false event

    let (_, render) ← (runWidget do
      overlayWhen' visible {} do
        text' "Overlay content" {}
    ).run events

    let node ← SpiderM.liftIO render
    -- Should be empty when not visible
    match node with
    | .empty => pure ()
    | _ => SpiderM.liftIO (ensure false "expected empty node when not visible")

test "overlayWhen' renders content when visible" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn false event

    let (_, render) ← (runWidget do
      overlayWhen' visible {} do
        text' "Overlay content" {}
    ).run events

    -- Make visible
    SpiderM.liftIO (trigger true)

    let node ← SpiderM.liftIO render
    SpiderM.liftIO (ensure (rnodeHasText node "Overlay content") "expected overlay content")

test "modal' renders with title and border" := do
  runSpider do
    let (events, _) ← createInputs
    let theme := Theme.dark

    let (_, render) ← (runWidget do
      let _ ← modal' "Test Modal" theme {} do
        text' "Modal body" {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render
    -- Modal wraps in a block with title
    match node with
    | .block title _ _ _ child =>
      SpiderM.liftIO (title ≡ some "Test Modal")
      SpiderM.liftIO (ensure (rnodeHasText child "Modal body") "expected modal body")
    | _ => SpiderM.liftIO (ensure false "expected block node")

test "confirmDialog' emits confirmed event on Y key" := do
  let env ← SpiderEnv.new
  let (confirmResult, inputs, _visible, _setVisible) ← (do
    let (events, inputs) ← createInputs
    let (visEvent, setVis) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn true visEvent
    let (result, _render) ← (runWidget do
      confirmDialog' "Are you sure?" visible Theme.dark
    ).run events
    pure (result, inputs, visible, setVis)
  ).run env

  env.postBuildTrigger ()

  -- Track if confirmed was fired
  let confirmedRef ← IO.mkRef false
  let _unsub ← confirmResult.confirmed.subscribe fun () =>
    confirmedRef.set true

  -- Press 'y'
  inputs.fireKey { event := KeyEvent.char 'y', focusedWidget := none }

  let wasConfirmed ← confirmedRef.get
  wasConfirmed ≡ true

  env.currentScope.dispose

test "confirmDialog' emits cancelled event on Escape" := do
  let env ← SpiderEnv.new
  let (confirmResult, inputs, _visible, _setVisible) ← (do
    let (events, inputs) ← createInputs
    let (visEvent, setVis) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn true visEvent
    let (result, _render) ← (runWidget do
      confirmDialog' "Are you sure?" visible Theme.dark
    ).run events
    pure (result, inputs, visible, setVis)
  ).run env

  env.postBuildTrigger ()

  -- Track if cancelled was fired
  let cancelledRef ← IO.mkRef false
  let _unsub ← confirmResult.cancelled.subscribe fun () =>
    cancelledRef.set true

  -- Press Escape
  inputs.fireKey { event := KeyEvent.escape, focusedWidget := none }

  let wasCancelled ← cancelledRef.get
  wasCancelled ≡ true

  env.currentScope.dispose

test "messageDialog' emits dismiss on Enter" := do
  let env ← SpiderEnv.new
  let (dismissEvent, inputs, _visible) ← (do
    let (events, inputs) ← createInputs
    let (visEvent, _setVis) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn true visEvent
    let (result, _render) ← (runWidget do
      messageDialog' "Operation complete" visible Theme.dark
    ).run events
    pure (result, inputs, visible)
  ).run env

  env.postBuildTrigger ()

  let dismissedRef ← IO.mkRef false
  let _unsub ← dismissEvent.subscribe fun () =>
    dismissedRef.set true

  inputs.fireKey { event := KeyEvent.enter, focusedWidget := none }

  let wasDismissed ← dismissedRef.get
  wasDismissed ≡ true

  env.currentScope.dispose

#generate_tests

end TerminusTests.Reactive.OverlayTests
