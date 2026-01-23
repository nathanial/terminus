-- TerminusTests.Reactive.FormTests: Form widget tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.FormTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Form Tests"

-- ============================================================================
-- OptionSelector Tests
-- ============================================================================

test "optionSelector' starts with initial selection" := do
  let env ← SpiderEnv.new
  let (result, _) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "priority")
    let (res, _render) ← (runWidget do
      optionSelector' "priority" #["Low", "Medium", "High"] 1 {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  let idx ← result.selectedIndex.sample
  let val ← result.selectedValue.sample
  ensure (idx == 1) "should start at index 1"
  ensure (val == "Medium") "should have value Medium"

  env.currentScope.dispose

test "optionSelector' responds to arrow keys" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "opt")
    let (res, _render) ← (runWidget do
      optionSelector' "opt" #["A", "B", "C"] 0 {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move right
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "opt" }
  let idx1 ← result.selectedIndex.sample
  ensure (idx1 == 1) "should move to index 1"

  -- Move right again
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "opt" }
  let idx2 ← result.selectedIndex.sample
  ensure (idx2 == 2) "should move to index 2"

  -- Move left
  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "opt" }
  let idx3 ← result.selectedIndex.sample
  ensure (idx3 == 1) "should move back to index 1"

  env.currentScope.dispose

test "optionSelector' respects boundaries" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "opt")
    let (res, _render) ← (runWidget do
      optionSelector' "opt" #["A", "B"] 0 {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Try to move left at start
  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "opt" }
  let idx1 ← result.selectedIndex.sample
  ensure (idx1 == 0) "should stay at 0"

  -- Move to end
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "opt" }
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "opt" }
  let idx2 ← result.selectedIndex.sample
  ensure (idx2 == 1) "should stop at 1"

  env.currentScope.dispose

test "optionSelector' home jumps to first" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "opt")
    let (res, _render) ← (runWidget do
      optionSelector' "opt" #["A", "B", "C", "D"] 2 {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  inputs.fireKey { event := { code := .home }, focusedWidget := some "opt" }
  let idx ← result.selectedIndex.sample
  ensure (idx == 0) "home should jump to first"

  env.currentScope.dispose

test "optionSelector' end jumps to last" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "opt")
    let (res, _render) ← (runWidget do
      optionSelector' "opt" #["A", "B", "C", "D"] 0 {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  inputs.fireKey { event := { code := .«end» }, focusedWidget := some "opt" }
  let idx ← result.selectedIndex.sample
  ensure (idx == 3) "end should jump to last"

  env.currentScope.dispose

test "optionSelector' fires onChange" := do
  let env ← SpiderEnv.new
  let (lastChange, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "opt")
    let (res, _render) ← (runWidget do
      optionSelector' "opt" #["A", "B", "C"] 0 {}
    ).run events
    let lastChange ← captureLatest res.onChange
    pure (lastChange, inputs)
  ).run env

  env.postBuildTrigger ()

  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "opt" }

  let changed ← lastChange.sample
  ensure (changed == some 1) "onChange should fire with new index"

  env.currentScope.dispose

-- ============================================================================
-- Checkbox Tests
-- ============================================================================

test "checkbox' starts with initial value" := do
  let env ← SpiderEnv.new
  let (result, _) ← (do
    let (events, inputs) ← createInputs
    let (res, _render) ← (runWidget do
      checkbox' "agree" "I agree" true {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  let checked ← result.checked.sample
  ensure checked "should start checked"

  env.currentScope.dispose

test "checkbox' toggles on space" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "cb")
    let (res, _render) ← (runWidget do
      checkbox' "cb" "Toggle me" false {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Initially unchecked
  let checked1 ← result.checked.sample
  ensure (!checked1) "should start unchecked"

  -- Toggle with space
  inputs.fireKey { event := { code := .space }, focusedWidget := some "cb" }
  let checked2 ← result.checked.sample
  ensure checked2 "should be checked after space"

  -- Toggle again
  inputs.fireKey { event := { code := .space }, focusedWidget := some "cb" }
  let checked3 ← result.checked.sample
  ensure (!checked3) "should be unchecked after second space"

  env.currentScope.dispose

test "checkbox' toggles on enter" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "cb")
    let (res, _render) ← (runWidget do
      checkbox' "cb" "Toggle me" false {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  inputs.fireKey { event := KeyEvent.enter, focusedWidget := some "cb" }
  let checked ← result.checked.sample
  ensure checked "enter should toggle checkbox"

  env.currentScope.dispose

test "checkbox' fires onChange" := do
  let env ← SpiderEnv.new
  let (lastChange, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "cb")
    let (res, _render) ← (runWidget do
      checkbox' "cb" "Notify" false {}
    ).run events
    let lastChange ← captureLatest res.onChange
    pure (lastChange, inputs)
  ).run env

  env.postBuildTrigger ()

  inputs.fireKey { event := { code := .space }, focusedWidget := some "cb" }

  let changed ← lastChange.sample
  ensure (changed == some true) "onChange should fire with new value"

  env.currentScope.dispose

test "checkbox' renders label" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← checkbox' "cb" "Accept terms" false {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Accept terms") "should show label")

-- ============================================================================
-- LabeledOptionSelector Tests
-- ============================================================================

test "labeledOptionSelector' renders label" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← labeledOptionSelector' "Priority:" "priority" #["Low", "High"] 0 {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Priority:") "should contain label")

-- ============================================================================
-- InlineLabeledInput Tests
-- ============================================================================

test "inlineLabeledInput' renders label and input" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← inlineLabeledInput' "Name" "name" "" {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Name:") "should contain label")

-- ============================================================================
-- FieldGroup Tests
-- ============================================================================

test "fieldGroup' renders with title" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      fieldGroup' "Personal Info" Theme.dark do
        text' "Content" {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Personal Info") "should contain title")
    SpiderM.liftIO (ensure (rnodeContainsText node "Content") "should contain content")



end TerminusTests.Reactive.FormTests
