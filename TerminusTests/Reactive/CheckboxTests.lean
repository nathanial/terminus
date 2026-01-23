-- TerminusTests.Reactive.CheckboxTests: Tests for reactive Checkbox and RadioGroup widgets

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.CheckboxTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Checkbox and RadioGroup Tests"

-- ============================================================================
-- Checkbox Tests
-- ============================================================================

test "checkbox'' renders correct symbol when unchecked" := do
  runSpider do
    let (events, _) ← createInputs
    let (_result, render) ← (runWidget do
      checkbox'' "test" "Accept terms" false {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "[ ]") "expected unchecked symbol")
    SpiderM.liftIO (ensure (rnodeContainsText node "Accept terms") "expected label")

test "checkbox'' renders correct symbol when checked" := do
  runSpider do
    let (events, _) ← createInputs
    let (_result, render) ← (runWidget do
      checkbox'' "test" "Accept terms" true {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "[x]") "expected checked symbol")

test "checkbox'' respects custom symbols" := do
  runSpider do
    let (events, _) ← createInputs
    let config : CheckboxConfig' := {
      checkedSymbol := "[*]"
      uncheckedSymbol := "[-]"
    }
    let (_result, render) ← (runWidget do
      checkbox'' "test" "Custom" true config
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "[*]") "expected custom checked symbol")

test "checkbox'' initial state is reflected in dynamic" := do
  runSpider do
    let (events, _) ← createInputs
    let (result, _render) ← (runWidget do
      checkbox'' "test" "Label" true {}
    ).run events

    let checked ← SpiderM.liftIO result.checked.sample
    SpiderM.liftIO (checked ≡ true)

test "checkbox'' toggles on space key when focused" := do
  let env ← SpiderEnv.new
  let (cbResult, inputs, _events) ← (do
    let (events, inputs) ← createInputs
    -- Focus the checkbox
    SpiderM.liftIO <| events.registry.fireFocus (some "test-cb")
    let (result, _render) ← (runWidget do
      checkbox'' "test-cb" "Toggle me" false {}
    ).run events
    pure (result, inputs, events)
  ).run env

  env.postBuildTrigger ()

  -- Initial state
  let v0 ← cbResult.checked.sample
  v0 ≡ false

  -- Fire space key event
  inputs.fireKey { event := { code := .space, modifiers := {} }, focusedWidget := some "test-cb" }

  -- Should now be toggled
  let v1 ← cbResult.checked.sample
  v1 ≡ true

  -- Toggle again
  inputs.fireKey { event := { code := .space, modifiers := {} }, focusedWidget := some "test-cb" }

  let v2 ← cbResult.checked.sample
  v2 ≡ false

  env.currentScope.dispose

test "checkbox'' toggles on enter key when focused" := do
  let env ← SpiderEnv.new
  let (cbResult, inputs, _events) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "enter-cb")
    let (result, _render) ← (runWidget do
      checkbox'' "enter-cb" "Enter test" false {}
    ).run events
    pure (result, inputs, events)
  ).run env

  env.postBuildTrigger ()

  let v0 ← cbResult.checked.sample
  v0 ≡ false

  inputs.fireKey { event := { code := .enter, modifiers := {} }, focusedWidget := some "enter-cb" }

  let v1 ← cbResult.checked.sample
  v1 ≡ true

  env.currentScope.dispose

test "checkbox'' does not toggle when not focused" := do
  let env ← SpiderEnv.new
  let (cbResult, inputs, _events) ← (do
    let (events, inputs) ← createInputs
    -- Don't focus the checkbox
    let (result, _render) ← (runWidget do
      checkbox'' "unfocused" "Label" false {}
    ).run events
    pure (result, inputs, events)
  ).run env

  env.postBuildTrigger ()

  let v0 ← cbResult.checked.sample
  v0 ≡ false

  -- Fire space key (but checkbox is not focused)
  inputs.fireKey { event := { code := .space, modifiers := {} }, focusedWidget := none }

  -- Should still be unchecked
  let v1 ← cbResult.checked.sample
  v1 ≡ false

  env.currentScope.dispose

-- ============================================================================
-- RadioGroup Tests
-- ============================================================================

test "radioGroup' renders all options" := do
  runSpider do
    let (events, _) ← createInputs
    let options := #["Apple", "Banana", "Cherry"]
    let (_result, render) ← (runWidget do
      radioGroup' "fruit" options (some 0) {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Apple") "expected Apple")
    SpiderM.liftIO (ensure (rnodeContainsText node "Banana") "expected Banana")
    SpiderM.liftIO (ensure (rnodeContainsText node "Cherry") "expected Cherry")

test "radioGroup' renders correct symbols for selected/unselected" := do
  runSpider do
    let (events, _) ← createInputs
    let options := #["A", "B", "C"]
    let (_result, render) ← (runWidget do
      radioGroup' "test" options (some 1) {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- B should have selected symbol
    SpiderM.liftIO (ensure (rnodeContainsText node "(o) B") "expected selected B")
    -- A and C should have unselected symbol
    SpiderM.liftIO (ensure (rnodeContainsText node "( ) A") "expected unselected A")
    SpiderM.liftIO (ensure (rnodeContainsText node "( ) C") "expected unselected C")

test "radioGroup' initial selection is reflected in dynamics" := do
  runSpider do
    let (events, _) ← createInputs
    let options := #["Red", "Green", "Blue"]
    let (result, _render) ← (runWidget do
      radioGroup' "colors" options (some 2) {}
    ).run events

    let idx ← SpiderM.liftIO result.selectedIndex.sample
    SpiderM.liftIO (idx ≡ some 2)

    let label ← SpiderM.liftIO result.selectedLabel.sample
    SpiderM.liftIO (label ≡ some "Blue")

test "radioGroup' clamps initial selection to valid range" := do
  runSpider do
    let (events, _) ← createInputs
    let options := #["X", "Y"]
    let (result, _render) ← (runWidget do
      radioGroup' "test" options (some 10) {}  -- Invalid index
    ).run events

    -- Should clamp to none since index is out of range
    let idx ← SpiderM.liftIO result.selectedIndex.sample
    SpiderM.liftIO (idx ≡ none)

test "radioGroup' handles empty options" := do
  runSpider do
    let (events, _) ← createInputs
    let options : Array String := #[]
    let (result, render) ← (runWidget do
      radioGroup' "empty" options (some 0) {}
    ).run events

    let idx ← SpiderM.liftIO result.selectedIndex.sample
    SpiderM.liftIO (idx ≡ none)

    let node ← SpiderM.liftIO render.sample
    -- Should render empty or not have any radio symbols
    SpiderM.liftIO (ensure (!rnodeContainsText node "(o)" && !rnodeContainsText node "( )") "expected empty render")

test "radioGroup' RadioState navigation works correctly" := do
  -- Test RadioState operations
  let s0 : RadioState := { selected := some 1, scrollOffset := 0 }

  -- Move up
  let s1 := s0.moveUp 5 true
  s1.selected ≡ some 0

  -- Move up with wrap
  let s2 : RadioState := { selected := some 0, scrollOffset := 0 }
  let s3 := s2.moveUp 5 true
  s3.selected ≡ some 4

  -- Move up without wrap
  let s4 := s2.moveUp 5 false
  s4.selected ≡ some 0

  -- Move down
  let s5 := s0.moveDown 5 true
  s5.selected ≡ some 2

  -- Move down with wrap
  let s6 : RadioState := { selected := some 4, scrollOffset := 0 }
  let s7 := s6.moveDown 5 true
  s7.selected ≡ some 0

  -- Move to first/last
  let s8 := s0.moveToFirst
  s8.selected ≡ some 0

  let s9 := s0.moveToLast 5
  s9.selected ≡ some 4

test "radioGroup' RadioState scroll adjustment" := do
  let s0 : RadioState := { selected := some 5, scrollOffset := 0 }

  -- Adjust scroll to keep selection visible (maxVisible = 3)
  let s1 := s0.adjustScroll 3
  s1.scrollOffset ≡ 3  -- selected(5) - maxVisible(3) + 1 = 3

  -- Selection within visible range should not change scroll
  let s2 : RadioState := { selected := some 1, scrollOffset := 0 }
  let s3 := s2.adjustScroll 3
  s3.scrollOffset ≡ 0

test "radioGroup' RadioState clamp selection" := do
  let s0 : RadioState := { selected := some 5, scrollOffset := 2 }

  -- Clamp when count shrinks
  let s1 := s0.clampSelection 3  -- 5 >= 3, should clamp to 2
  s1.selected ≡ some 2

  -- Clamp to none when empty
  let s2 := s0.clampSelection 0
  s2.selected ≡ none

  -- No change when selection is valid
  let s3 := s0.clampSelection 10
  s3.selected ≡ some 5

test "radioGroup' navigates with arrow keys when focused" := do
  let env ← SpiderEnv.new
  let (rgResult, inputs, _events) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "nav-radio")
    let (result, _render) ← (runWidget do
      radioGroup' "nav-radio" #["A", "B", "C"] (some 0) {}
    ).run events
    pure (result, inputs, events)
  ).run env

  env.postBuildTrigger ()

  -- Initial state
  let idx0 ← rgResult.selectedIndex.sample
  idx0 ≡ some 0

  -- Press down
  inputs.fireKey { event := { code := .down, modifiers := {} }, focusedWidget := some "nav-radio" }

  let idx1 ← rgResult.selectedIndex.sample
  idx1 ≡ some 1

  -- Press down again
  inputs.fireKey { event := { code := .down, modifiers := {} }, focusedWidget := some "nav-radio" }

  let idx2 ← rgResult.selectedIndex.sample
  idx2 ≡ some 2

  -- Press up
  inputs.fireKey { event := { code := .up, modifiers := {} }, focusedWidget := some "nav-radio" }

  let idx3 ← rgResult.selectedIndex.sample
  idx3 ≡ some 1

  env.currentScope.dispose

test "radioGroup' wraps around when configured" := do
  let env ← SpiderEnv.new
  let (rgResult, inputs, _events) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "wrap-radio")
    let (result, _render) ← (runWidget do
      radioGroup' "wrap-radio" #["A", "B", "C"] (some 2) { wrapAround := true }
    ).run events
    pure (result, inputs, events)
  ).run env

  env.postBuildTrigger ()

  -- Start at last item
  let idx0 ← rgResult.selectedIndex.sample
  idx0 ≡ some 2

  -- Press down should wrap to first
  inputs.fireKey { event := { code := .down, modifiers := {} }, focusedWidget := some "wrap-radio" }

  let idx1 ← rgResult.selectedIndex.sample
  idx1 ≡ some 0

  -- Press up should wrap to last
  inputs.fireKey { event := { code := .up, modifiers := {} }, focusedWidget := some "wrap-radio" }

  let idx2 ← rgResult.selectedIndex.sample
  idx2 ≡ some 2

  env.currentScope.dispose

test "radioGroup' does not wrap when wrapAround is false" := do
  let env ← SpiderEnv.new
  let (rgResult, inputs, _events) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "nowrap-radio")
    let (result, _render) ← (runWidget do
      radioGroup' "nowrap-radio" #["A", "B", "C"] (some 2) { wrapAround := false }
    ).run events
    pure (result, inputs, events)
  ).run env

  env.postBuildTrigger ()

  -- Start at last item
  let idx0 ← rgResult.selectedIndex.sample
  idx0 ≡ some 2

  -- Press down should not change (at end, no wrap)
  inputs.fireKey { event := { code := .down, modifiers := {} }, focusedWidget := some "nowrap-radio" }

  let idx1 ← rgResult.selectedIndex.sample
  idx1 ≡ some 2

  env.currentScope.dispose

-- ============================================================================
-- Dynamic RadioGroup Tests
-- ============================================================================

test "dynRadioGroup' clamps selection when options shrink" := do
  let env ← SpiderEnv.new
  let (rgResult, optsFire) ← (do
    let (events, _inputs) ← createInputs
    let (optsEvent, fireOpts) ← Reactive.newTriggerEvent (t := Spider) (a := Array String)
    let optsDyn ← Reactive.holdDyn #["A", "B", "C", "D"] optsEvent

    let (result, _render) ← (runWidget do
      dynRadioGroup' "dyn-radio" optsDyn (some 3) {}  -- Select D
    ).run events
    pure (result, fireOpts)
  ).run env

  env.postBuildTrigger ()

  -- Initial state
  let idx0 ← rgResult.selectedIndex.sample
  idx0 ≡ some 3

  let label0 ← rgResult.selectedLabel.sample
  label0 ≡ some "D"

  -- Shrink options (remove D)
  optsFire #["A", "B", "C"]

  -- Selection should clamp to new max (2)
  let idx1 ← rgResult.selectedIndex.sample
  idx1 ≡ some 2

  let label1 ← rgResult.selectedLabel.sample
  label1 ≡ some "C"

  env.currentScope.dispose

test "dynRadioGroup' preserves selection by label when options change" := do
  let env ← SpiderEnv.new
  let (rgResult, optsFire) ← (do
    let (events, _inputs) ← createInputs
    let (optsEvent, fireOpts) ← Reactive.newTriggerEvent (t := Spider) (a := Array String)
    let optsDyn ← Reactive.holdDyn #["Alpha", "Beta", "Gamma"] optsEvent

    let (result, _render) ← (runWidget do
      dynRadioGroup' "preserve-radio" optsDyn (some 1) {}  -- Select Beta
    ).run events
    pure (result, fireOpts)
  ).run env

  env.postBuildTrigger ()

  let label0 ← rgResult.selectedLabel.sample
  label0 ≡ some "Beta"

  -- Change options but keep Beta (at different position)
  optsFire #["Delta", "Beta", "Epsilon", "Gamma"]

  -- Should still be Beta but at new index
  let label1 ← rgResult.selectedLabel.sample
  label1 ≡ some "Beta"

  let idx1 ← rgResult.selectedIndex.sample
  idx1 ≡ some 1  -- Beta is still at index 1 in new list

  env.currentScope.dispose

test "dynRadioGroup' handles options becoming empty" := do
  let env ← SpiderEnv.new
  let (rgResult, optsFire) ← (do
    let (events, _inputs) ← createInputs
    let (optsEvent, fireOpts) ← Reactive.newTriggerEvent (t := Spider) (a := Array String)
    let optsDyn ← Reactive.holdDyn #["X", "Y", "Z"] optsEvent

    let (result, _render) ← (runWidget do
      dynRadioGroup' "empty-radio" optsDyn (some 1) {}
    ).run events
    pure (result, fireOpts)
  ).run env

  env.postBuildTrigger ()

  let idx0 ← rgResult.selectedIndex.sample
  idx0 ≡ some 1

  -- Make options empty
  optsFire #[]

  -- Selection should become none
  let idx1 ← rgResult.selectedIndex.sample
  idx1 ≡ none

  let label1 ← rgResult.selectedLabel.sample
  label1 ≡ none

  env.currentScope.dispose



end TerminusTests.Reactive.CheckboxTests
