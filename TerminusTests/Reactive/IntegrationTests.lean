-- TerminusTests.Reactive.IntegrationTests: App loop and terminal integration tests

import Crucible
import Terminus.Reactive
import Terminus.Reactive.Demos.ReactiveDemo
import Terminus.Reactive.Demos.ReactiveInput
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.IntegrationTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Integration Tests"

-- ============================================================================
-- Terminal flush regression tests
-- ============================================================================

test "Terminal.flush writes updates when buffer changes" := do
  let action : MockTerminal (String × String) := do
    let term ← Terminus.Terminal.new

    let buf1 := (Buffer.new 5 1).writeString 0 0 "A" {}
    let term := term.setBuffer buf1
    let term ← term.flush

    let output1 := (← get).outputBuffer

    let buf2 := (Buffer.new 5 1).writeString 0 0 "B" {}
    let term := term.setBuffer buf2
    let _term ← term.flush

    let output2 := (← get).outputBuffer

    pure (output1, output2)

  let ((output1, output2), _) := MockTerminal.run action
  -- Output should grow and include the updated character
  (decide (output2.length > output1.length)) ≡ true
  (output2.toList.any (· == 'B')) ≡ true

-- ============================================================================
-- Reactive app loop regression tests
-- ============================================================================

test "reactive app loop updates buffer on tick events" := do
  let env ← SpiderEnv.new
  let (render, inputs) ← (do
    let (events, inputs) ← createInputs
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 events.tickEvent
    let (_, render) ← (runWidget do
      emitDynamic do
        let n ← countDyn.sample
        pure (RNode.text s!"{n}" {})
    ).run events
    pure (render, inputs)
  ).run env

  env.postBuildTrigger ()

  let node1 ← render
  let buf1 := Terminus.Reactive.renderOnly node1 10 2

  inputs.fireTick { frame := 1, elapsedMs := 16 }

  let node2 ← render
  let buf2 := Terminus.Reactive.renderOnly node2 10 2

  (buf1.get 0 0).char ≡ '0'
  (buf2.get 0 0).char ≡ '1'

  env.currentScope.dispose

test "reactive app loop updates buffer on key events" := do
  let env ← SpiderEnv.new
  let (render, inputs) ← (do
    let (events, inputs) ← createInputs
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 events.keyEvent
    let (_, render) ← (runWidget do
      emitDynamic do
        let n ← countDyn.sample
        pure (RNode.text s!"{n}" {})
    ).run events
    pure (render, inputs)
  ).run env

  env.postBuildTrigger ()

  let node1 ← render
  let buf1 := Terminus.Reactive.renderOnly node1 10 2

  inputs.fireKey { event := KeyEvent.char 'a', focusedWidget := none }

  let node2 ← render
  let buf2 := Terminus.Reactive.renderOnly node2 10 2

  (buf1.get 0 0).char ≡ '0'
  (buf2.get 0 0).char ≡ '1'

  env.currentScope.dispose

-- ============================================================================
-- Extracted loop + mock terminal integration
-- ============================================================================

test "runReactiveLoop updates frames with TerminalMock" := do
  let env ← SpiderEnv.new
  let (appState, events, inputs) ← (do
    let (events, inputs) ← createInputs
    let setup : ReactiveTermM ReactiveAppState := do
      let tickEvents ← useTick
      let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 tickEvents
      let (_, render) ← runWidget do
        emitDynamic do
          let n ← countDyn.sample
          pure (RNode.text s!"{n}" {})
      pure { render }
    let appState ← setup.run events
    pure (appState, events, inputs)
  ).run env

  env.postBuildTrigger ()

  let framesRef ← IO.mkRef (#[] : Array Buffer)
  let clockRef ← IO.mkRef 0

  let deps : LoopDeps MockTerminalIO := {
    eventSource := Events.poll
    nowMs := do
      let n ← liftM (m := IO) clockRef.get
      liftM (m := IO) (clockRef.set (n + 16))
      pure n
    sleepMs := fun _ => pure ()
    log := fun _ => pure ()
    maxFrames := some 3
    onFrame := fun _ buf =>
      liftM (m := IO) <| framesRef.modify (fun frames => frames.push buf)
  }

  let action : MockTerminalIO Unit := do
    let term ← Terminal.new
    let termRef ← liftM (m := IO) (IO.mkRef term)
    runReactiveLoop { frameMs := 0 } events inputs appState.render termRef deps

  let _ ← action.run ({} : MockTerminalState)

  let frames ← framesRef.get
  match frames.toList with
  | buf1 :: buf2 :: _ =>
    ((Buffer.diff buf1 buf2).isEmpty) ≡ false
  | _ =>
    ensure false "expected at least 2 frames"

  env.currentScope.dispose

-- ============================================================================
-- Reactive demo widget tree regression
-- ============================================================================

test "ReactiveDemo widget updates on tick and key events" := do
  let env ← SpiderEnv.new
  let (appState, _events, inputs) ← (do
    let (events, inputs) ← createInputs
    let appState ← ReactiveTermM.run events reactiveDemoApp
    pure (appState, events, inputs)
  ).run env

  env.postBuildTrigger ()

  let node1 ← appState.render
  let buf1 := Terminus.Reactive.renderOnly node1 80 24

  (rnodeHasText node1 "0:00") ≡ true
  (rnodeHasText node1 "none") ≡ true

  inputs.fireTick { frame := 1, elapsedMs := 1500 }

  let node2 ← appState.render
  let buf2 := Terminus.Reactive.renderOnly node2 80 24

  (rnodeHasText node2 "0:01") ≡ true
  ensure (!(Buffer.diff buf1 buf2).isEmpty) "expected buffer to change after tick"

  inputs.fireKey { event := KeyEvent.char 'a', focusedWidget := none }

  let node3 ← appState.render
  let buf3 := Terminus.Reactive.renderOnly node3 80 24

  (rnodeHasText node3 "'a'") ≡ true
  ensure (!(Buffer.diff buf2 buf3).isEmpty) "expected buffer to change after key"

  env.currentScope.dispose

-- ============================================================================
-- Events.poll + tick integration
-- ============================================================================

test "Events.poll and tick drive frame-to-frame changes" := do
  let env ← SpiderEnv.new
  let (appState, events, inputs) ← (do
    let (events, inputs) ← createInputs
    let setup : ReactiveTermM ReactiveAppState := do
      let keyEvents ← useKeyEvent
      let tickEvents ← useTick
      let keyCount ← Reactive.foldDyn (fun _ n => n + 1) 0 keyEvents
      let tickCount ← Reactive.foldDyn (fun _ n => n + 1) 0 tickEvents
      let (_, render) ← runWidget do
        emitDynamic do
          let k ← keyCount.sample
          let t ← tickCount.sample
          pure (RNode.text s!"Key={k} Tick={t}" {})
      pure { render }
    let appState ← setup.run events
    pure (appState, events, inputs)
  ).run env

  env.postBuildTrigger ()

  let rowsRef ← IO.mkRef (#[] : Array String)
  let clockRef ← IO.mkRef 0

  let deps : LoopDeps MockTerminalIO := {
    eventSource := Events.poll
    nowMs := do
      let n ← liftM (m := IO) clockRef.get
      liftM (m := IO) (clockRef.set (n + 16))
      pure n
    sleepMs := fun _ => pure ()
    log := fun _ => pure ()
    maxFrames := some 2
    onFrame := fun _ buf =>
      let row := bufferRowPrefix buf 0 20
      liftM (m := IO) <| rowsRef.modify (fun rows => rows.push row)
  }

  let action : MockTerminalIO Unit := do
    let term ← Terminal.new
    let termRef ← liftM (m := IO) (IO.mkRef term)
    runReactiveLoop { frameMs := 0 } events inputs appState.render termRef deps

  let initialState : MockTerminalState := { inputQueue := [120] } -- 'x'
  let _ ← action.run initialState

  let rows ← rowsRef.get
  match rows.toList with
  | row1 :: row2 :: _ =>
    let expected1 := "Key=1 Tick=1"
    let expected2 := "Key=1 Tick=2"
    (row1.take expected1.length) ≡ expected1
    (row2.take expected2.length) ≡ expected2
  | _ =>
    ensure false "expected at least 2 frames"

  env.currentScope.dispose

-- ============================================================================
-- ReactiveInput app rendering test
-- ============================================================================

test "titledBlock with content renders correctly" := do
  runSpider do
    let (events, _) ← createInputs
    let theme := Theme.dark

    let (_, render) ← (runWidget do
      titledBlock' "Test Panel" .rounded theme do
        text' "Line 1" {}
        text' "Line 2" {}
    ).run events

    let node ← SpiderM.liftIO render
    let buf := Terminus.Reactive.renderOnly node 40 10

    -- Title should appear in top border
    (buf.get 2 0).char ≡ 'T'  -- " Test Panel" starts at col 2

    -- Content should be inside the block (row 1+)
    -- Line 1 should start at column 1 (inside border), row 1
    (buf.get 1 1).char ≡ 'L'
    (buf.get 2 1).char ≡ 'i'
    (buf.get 3 1).char ≡ 'n'
    (buf.get 4 1).char ≡ 'e'

test "reactiveInputApp renders correctly" := do
  let env ← SpiderEnv.new
  let (appState, _events, _inputs) ← (do
    let (events, inputs) ← createInputs
    let appState ← ReactiveTermM.run events reactiveInputApp
    pure (appState, events, inputs)
  ).run env

  env.postBuildTrigger ()

  let node ← appState.render
  let buf := Terminus.Reactive.renderOnly node 80 60

  -- Check header is at top
  (buf.get 0 0).char ≡ '='

  -- Check the buffer has some content rendered (not all empty)
  -- The app should render multiple panels with text
  let hasContent := (buf.get 0 6).char != ' ' -- Should have border char
  hasContent ≡ true

  env.currentScope.dispose

#generate_tests

end TerminusTests.Reactive.IntegrationTests
