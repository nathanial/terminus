-- TerminusTests.Reactive.TabsTests: Tabs widget tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.TabsTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Tabs Tests"

-- ============================================================================
-- Tabs Tests
-- ============================================================================

test "tabs' renders all labels" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _result ← tabs' #["Home", "Settings", "Help"] 0 {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeHasText node "Home") "expected Home label")
    SpiderM.liftIO (ensure (rnodeHasText node "Settings") "expected Settings label")
    SpiderM.liftIO (ensure (rnodeHasText node "Help") "expected Help label")

test "tabs' starts with initial tab selected" := do
  let env ← SpiderEnv.new
  let (tabsResult, _events, _) ← (do
    let (events, inputs) ← createInputs
    let (result, _render) ← (runWidget do
      tabs' #["One", "Two", "Three"] 1 {}  -- Start at index 1
    ).run events
    pure (result, events, inputs)
  ).run env

  env.postBuildTrigger ()

  let activeTab ← tabsResult.activeTab.sample
  activeTab ≡ 1

  env.currentScope.dispose

test "tabs' responds to right arrow key" := do
  let env ← SpiderEnv.new
  let (tabsResult, inputs) ← (do
    let (events, inputs) ← createInputs
    -- Set focus to the tabs
    SpiderM.liftIO <| events.registry.fireFocus (some "tabs_0")
    let (result, _render) ← (runWidget do
      tabs' #["A", "B", "C"] 0 { focusName := "tabs_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Initial tab should be 0
  let activeTab ← tabsResult.activeTab.sample
  activeTab ≡ 0

  -- Press right arrow
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "tabs_0" }

  -- Should now be 1
  let newTab ← tabsResult.activeTab.sample
  newTab ≡ 1

  env.currentScope.dispose

test "tabs' responds to left arrow key" := do
  let env ← SpiderEnv.new
  let (tabsResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tabs_0")
    let (result, _render) ← (runWidget do
      tabs' #["A", "B", "C"] 2 { focusName := "tabs_0" }  -- Start at last
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  let activeTab ← tabsResult.activeTab.sample
  activeTab ≡ 2

  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "tabs_0" }

  let newTab ← tabsResult.activeTab.sample
  newTab ≡ 1

  env.currentScope.dispose

test "tabs' wraps around at boundaries (wrapAround true)" := do
  let env ← SpiderEnv.new
  let (tabsResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tabs_0")
    let (result, _render) ← (runWidget do
      tabs' #["A", "B", "C"] 0 { focusName := "tabs_0", wrapAround := true }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Start at 0, press left should wrap to 2
  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "tabs_0" }
  let tab1 ← tabsResult.activeTab.sample
  tab1 ≡ 2

  -- At 2, press right should wrap to 0
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "tabs_0" }
  let tab2 ← tabsResult.activeTab.sample
  tab2 ≡ 0

  env.currentScope.dispose

test "tabs' does not wrap when wrapAround is false" := do
  let env ← SpiderEnv.new
  let (tabsResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tabs_0")
    let (result, _render) ← (runWidget do
      tabs' #["A", "B", "C"] 0 { focusName := "tabs_0", wrapAround := false }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Start at 0, press left should stay at 0
  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "tabs_0" }
  let tab1 ← tabsResult.activeTab.sample
  tab1 ≡ 0

  env.currentScope.dispose

test "tabs' home key jumps to first tab" := do
  let env ← SpiderEnv.new
  let (tabsResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tabs_0")
    let (result, _render) ← (runWidget do
      tabs' #["A", "B", "C", "D", "E"] 4 { focusName := "tabs_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  let activeTab ← tabsResult.activeTab.sample
  activeTab ≡ 4

  inputs.fireKey { event := { code := .home }, focusedWidget := some "tabs_0" }

  let newTab ← tabsResult.activeTab.sample
  newTab ≡ 0

  env.currentScope.dispose

test "tabs' end key jumps to last tab" := do
  let env ← SpiderEnv.new
  let (tabsResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tabs_0")
    let (result, _render) ← (runWidget do
      tabs' #["A", "B", "C", "D", "E"] 0 { focusName := "tabs_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  inputs.fireKey { event := { code := .«end» }, focusedWidget := some "tabs_0" }

  let newTab ← tabsResult.activeTab.sample
  newTab ≡ 4

  env.currentScope.dispose

test "tabs' fires onTabChange event" := do
  let env ← SpiderEnv.new
  let (lastChange, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tabs_0")
    let (result, _render) ← (runWidget do
      tabs' #["A", "B", "C"] 0 { focusName := "tabs_0" }
    ).run events
    let lastChange ← captureLatest result.onTabChange
    pure (lastChange, inputs)
  ).run env

  env.postBuildTrigger ()

  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "tabs_0" }

  let changed ← lastChange.sample
  changed ≡ some 1

  env.currentScope.dispose

test "numberedTabs' shows number prefixes" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _result ← numberedTabs' #["Alpha", "Beta", "Gamma"] 0 {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should have "1. Alpha", "2. Beta", "3. Gamma"
    SpiderM.liftIO (ensure (rnodeHasText node "1. Alpha") "expected numbered Alpha")
    SpiderM.liftIO (ensure (rnodeHasText node "2. Beta") "expected numbered Beta")
    SpiderM.liftIO (ensure (rnodeHasText node "3. Gamma") "expected numbered Gamma")

test "numberedTabs' responds to number keys" := do
  let env ← SpiderEnv.new
  let (tabsResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tabs_0")
    let (result, _render) ← (runWidget do
      numberedTabs' #["A", "B", "C", "D", "E"] 0 { focusName := "tabs_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Press '3' to jump to third tab (index 2)
  inputs.fireKey { event := KeyEvent.char '3', focusedWidget := some "tabs_0" }

  let tab1 ← tabsResult.activeTab.sample
  tab1 ≡ 2

  -- Press '5' to jump to fifth tab (index 4)
  inputs.fireKey { event := KeyEvent.char '5', focusedWidget := some "tabs_0" }

  let tab2 ← tabsResult.activeTab.sample
  tab2 ≡ 4

  env.currentScope.dispose

test "tabs' renders empty state gracefully" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _result ← tabs' #[] 0 {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeHasText node "(no tabs)") "expected empty tabs message")

test "tabs' ignores keys when not focused" := do
  let env ← SpiderEnv.new
  let (tabsResult, inputs) ← (do
    let (events, inputs) ← createInputs
    -- Don't set focus
    let (result, _render) ← (runWidget do
      tabs' #["A", "B", "C"] 0 { focusName := "tabs_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  let activeTab ← tabsResult.activeTab.sample
  activeTab ≡ 0

  -- Press right without focus should do nothing
  inputs.fireKey { event := KeyEvent.right, focusedWidget := none }

  let newTab ← tabsResult.activeTab.sample
  newTab ≡ 0

  env.currentScope.dispose



end TerminusTests.Reactive.TabsTests
