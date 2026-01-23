-- TerminusTests.Reactive.ComponentTests: TextInput and SelectableList tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.ComponentTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Component Tests"

-- ============================================================================
-- TextInput Tests
-- ============================================================================

test "textInput' renders with placeholder when empty" := do
  runSpider do
    let (events, _) ← createInputs
    let (_result, render) ← (runWidget do
      textInput' "test" "" { placeholder := "Enter text...", width := 15 }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain placeholder text when empty and not focused
    SpiderM.liftIO (ensure (rnodeHasText node "Enter text...") "expected placeholder")

test "textInput' value dynamic updates on input" := do
  let env ← SpiderEnv.new
  let (inputResult, _inputs) ← (do
    let (events, inputs) ← createInputs
    let (result, _render) ← (runWidget do
      textInput' "test-input" "" {}
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Initial value should be empty
  let v0 ← inputResult.value.sample
  v0 ≡ ""

  env.currentScope.dispose

test "textInput' state operations work correctly" := do
  -- Test TextInputState operations
  let s0 : TextInputState := { text := "hello", cursor := 3 }

  -- Insert character
  let s1 := s0.insertChar 'X' none
  s1.text ≡ "helXlo"
  s1.cursor ≡ 4

  -- Backspace
  let s2 := s1.backspace
  s2.text ≡ "hello"
  s2.cursor ≡ 3

  -- Move left
  let s3 := s0.moveLeft
  s3.cursor ≡ 2

  -- Move right
  let s4 := s0.moveRight
  s4.cursor ≡ 4

  -- Home
  let s5 := s0.moveHome
  s5.cursor ≡ 0

  -- End
  let s6 := s0.moveEnd
  s6.cursor ≡ 5

  -- Delete
  let s7 : TextInputState := { text := "hello", cursor := 2 }
  let s8 := s7.delete
  s8.text ≡ "helo"
  s8.cursor ≡ 2

test "textInput' respects maxLength" := do
  let s0 : TextInputState := { text := "abc", cursor := 3 }

  -- Should allow insert when under limit
  let s1 := s0.insertChar 'd' (some 5)
  s1.text ≡ "abcd"

  -- Should block insert when at limit
  let s2 : TextInputState := { text := "abcde", cursor := 5 }
  let s3 := s2.insertChar 'f' (some 5)
  s3.text ≡ "abcde"

-- ============================================================================
-- SelectableList Tests
-- ============================================================================

test "selectableList' renders items with selection" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #["Apple", "Banana", "Cherry"]
    let (_result, render) ← (runWidget do
      selectableList' items 0 {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should have all items
    SpiderM.liftIO (ensure (rnodeHasText node "> Apple") "expected selected Apple")
    SpiderM.liftIO (ensure (rnodeHasText node "  Banana") "expected Banana")
    SpiderM.liftIO (ensure (rnodeHasText node "  Cherry") "expected Cherry")

test "selectableList' initial selection is correct" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #["A", "B", "C"]
    let (result, _render) ← (runWidget do
      selectableList' items 1 {}  -- Start at index 1
    ).run events

    let idx ← SpiderM.liftIO result.selectedIndex.sample
    SpiderM.liftIO (idx ≡ 1)

    let item ← SpiderM.liftIO result.selectedItem.sample
    match item with
    | some s => SpiderM.liftIO (s ≡ "B")
    | none => SpiderM.liftIO (ensure false "expected selected item")

test "selectableList' ListState navigation" := do
  -- Test ListState operations
  let s0 : ListState := { selected := 1, scrollOffset := 0 }

  -- Move up
  let s1 := s0.moveUp 5 true
  s1.selected ≡ 0

  -- Move up with wrap
  let s2 : ListState := { selected := 0, scrollOffset := 0 }
  let s3 := s2.moveUp 5 true
  s3.selected ≡ 4

  -- Move up without wrap
  let s4 := s2.moveUp 5 false
  s4.selected ≡ 0

  -- Move down
  let s5 := s0.moveDown 5 true
  s5.selected ≡ 2

  -- Move down with wrap
  let s6 : ListState := { selected := 4, scrollOffset := 0 }
  let s7 := s6.moveDown 5 true
  s7.selected ≡ 0

  -- Move to first/last
  let s8 := s0.moveToFirst
  s8.selected ≡ 0

  let s9 := s0.moveToLast 5
  s9.selected ≡ 4

test "selectableList' scroll adjustment" := do
  let s0 : ListState := { selected := 5, scrollOffset := 0 }

  -- Adjust scroll to keep selection visible (maxVisible = 3)
  let s1 := s0.adjustScroll 3
  s1.scrollOffset ≡ 3  -- selected(5) - maxVisible(3) + 1 = 3

  -- Selection within visible range should not change scroll
  let s2 : ListState := { selected := 1, scrollOffset := 0 }
  let s3 := s2.adjustScroll 3
  s3.scrollOffset ≡ 0

test "selectableList' renders scroll indicators" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #["A", "B", "C", "D", "E"]
    let (_result, render) ← (runWidget do
      selectableList' items 3 { maxVisible := some 2, showScrollIndicators := true }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should have scroll indicators since list is truncated
    SpiderM.liftIO (ensure (rnodeHasText node "  ...") "expected scroll indicator")

test "numberedList' renders with numbers" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #["Red", "Green", "Blue"]
    let (_result, render) ← (runWidget do
      numberedList' items 0 {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeHasText node "> 1. Red") "expected numbered item 1")
    SpiderM.liftIO (ensure (rnodeHasText node "  2. Green") "expected numbered item 2")
    SpiderM.liftIO (ensure (rnodeHasText node "  3. Blue") "expected numbered item 3")



end TerminusTests.Reactive.ComponentTests
