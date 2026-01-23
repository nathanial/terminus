-- TerminusTests.Reactive.TextAreaTests: TextArea widget tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.TextAreaTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive TextArea Tests"

-- ============================================================================
-- TextAreaState Tests
-- ============================================================================

test "TextAreaState.fromString creates correct state" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  ensure (state.lines.size == 2) "should have 2 lines"
  ensure (state.lines[0]! == "Hello") "first line should be Hello"
  ensure (state.lines[1]! == "World") "second line should be World"
  ensure (state.line == 0) "should start at line 0"
  ensure (state.column == 0) "should start at column 0"

test "TextAreaState.fromString handles empty string" := do
  let state := TextAreaState.fromString ""
  ensure (state.lines.size == 1) "should have 1 empty line"
  ensure (state.lines[0]! == "") "line should be empty"

test "TextAreaState.getText reconstructs text" := do
  let state := TextAreaState.fromString "Line 1\nLine 2\nLine 3"
  let text := state.getText
  ensure (text == "Line 1\nLine 2\nLine 3") "should reconstruct original text"

test "TextAreaState.insertChar inserts at cursor" := do
  let state := TextAreaState.fromString "Hello"
  let state := { state with column := 5 }  -- at end
  let state := state.insertChar '!'
  ensure (state.lines[0]! == "Hello!") "should append char"
  ensure (state.column == 6) "cursor should advance"

test "TextAreaState.insertChar inserts in middle" := do
  let state := TextAreaState.fromString "Hllo"
  let state := { state with column := 1 }
  let state := state.insertChar 'e'
  ensure (state.lines[0]! == "Hello") "should insert char at position"
  ensure (state.column == 2) "cursor should advance"

test "TextAreaState.insertNewline splits line" := do
  let state := TextAreaState.fromString "HelloWorld"
  let state := { state with column := 5 }
  let state := state.insertNewline
  ensure (state.lines.size == 2) "should have 2 lines"
  ensure (state.lines[0]! == "Hello") "first line should be Hello"
  ensure (state.lines[1]! == "World") "second line should be World"
  ensure (state.line == 1) "cursor should be on line 1"
  ensure (state.column == 0) "cursor should be at column 0"

test "TextAreaState.backspace deletes before cursor" := do
  let state := TextAreaState.fromString "Hello"
  let state := { state with column := 5 }
  let state := state.backspace
  ensure (state.lines[0]! == "Hell") "should delete last char"
  ensure (state.column == 4) "cursor should move back"

test "TextAreaState.backspace at line start merges lines" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  let state := { state with line := 1, column := 0 }
  let state := state.backspace
  ensure (state.lines.size == 1) "should have 1 line"
  ensure (state.lines[0]! == "HelloWorld") "lines should be merged"
  ensure (state.line == 0) "cursor should be on line 0"
  ensure (state.column == 5) "cursor should be at join point"

test "TextAreaState.backspace at start does nothing" := do
  let state := TextAreaState.fromString "Hello"
  let state := state.backspace
  ensure (state.lines[0]! == "Hello") "should not change"
  ensure (state.column == 0) "cursor should stay at 0"

test "TextAreaState.delete removes at cursor" := do
  let state := TextAreaState.fromString "Hello"
  let state := { state with column := 0 }
  let state := state.delete
  ensure (state.lines[0]! == "ello") "should delete first char"
  ensure (state.column == 0) "cursor should stay"

test "TextAreaState.delete at line end merges with next" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  let state := { state with column := 5 }
  let state := state.delete
  ensure (state.lines.size == 1) "should have 1 line"
  ensure (state.lines[0]! == "HelloWorld") "lines should be merged"

test "TextAreaState.moveLeft moves cursor" := do
  let state := TextAreaState.fromString "Hello"
  let state := { state with column := 3 }
  let state := state.moveLeft
  ensure (state.column == 2) "cursor should move left"

test "TextAreaState.moveLeft at line start goes to prev line" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  let state := { state with line := 1, column := 0 }
  let state := state.moveLeft
  ensure (state.line == 0) "should be on previous line"
  ensure (state.column == 5) "should be at end of previous line"

test "TextAreaState.moveRight moves cursor" := do
  let state := TextAreaState.fromString "Hello"
  let state := { state with column := 2 }
  let state := state.moveRight
  ensure (state.column == 3) "cursor should move right"

test "TextAreaState.moveRight at line end goes to next line" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  let state := { state with column := 5 }
  let state := state.moveRight
  ensure (state.line == 1) "should be on next line"
  ensure (state.column == 0) "should be at start of next line"

test "TextAreaState.moveUp moves to previous line" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  let state := { state with line := 1, column := 3 }
  let state := state.moveUp
  ensure (state.line == 0) "should be on line 0"
  ensure (state.column == 3) "column should be preserved"

test "TextAreaState.moveUp clamps column to line length" := do
  let state := TextAreaState.fromString "Hi\nLonger"
  let state := { state with line := 1, column := 6 }
  let state := state.moveUp
  ensure (state.line == 0) "should be on line 0"
  ensure (state.column == 2) "column should be clamped to line length"

test "TextAreaState.moveDown moves to next line" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  let state := { state with line := 0, column := 3 }
  let state := state.moveDown
  ensure (state.line == 1) "should be on line 1"
  ensure (state.column == 3) "column should be preserved"

test "TextAreaState.moveHome moves to line start" := do
  let state := TextAreaState.fromString "Hello"
  let state := { state with column := 3 }
  let state := state.moveHome
  ensure (state.column == 0) "cursor should be at start"

test "TextAreaState.moveEnd moves to line end" := do
  let state := TextAreaState.fromString "Hello"
  let state := { state with column := 0 }
  let state := state.moveEnd
  ensure (state.column == 5) "cursor should be at end"

test "TextAreaState.pageUp moves up by page" := do
  let state := TextAreaState.fromString "1\n2\n3\n4\n5\n6\n7\n8\n9\n10"
  let state := { state with line := 8 }
  let state := state.pageUp 5
  ensure (state.line == 3) "should move up by 5"

test "TextAreaState.pageDown moves down by page" := do
  let state := TextAreaState.fromString "1\n2\n3\n4\n5\n6\n7\n8\n9\n10"
  let state := { state with line := 2 }
  let state := state.pageDown 5
  ensure (state.line == 7) "should move down by 5"

test "TextAreaState.moveToStart goes to beginning" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  let state := { state with line := 1, column := 3 }
  let state := state.moveToStart
  ensure (state.line == 0) "should be on line 0"
  ensure (state.column == 0) "should be at column 0"

test "TextAreaState.moveToEnd goes to end" := do
  let state := TextAreaState.fromString "Hello\nWorld"
  let state := state.moveToEnd
  ensure (state.line == 1) "should be on last line"
  ensure (state.column == 5) "should be at end of last line"

test "TextAreaState.ensureCursorVisible adjusts scroll" := do
  let state := TextAreaState.fromString "1\n2\n3\n4\n5\n6\n7\n8\n9\n10"
  let state := { state with line := 8, scrollOffset := 0 }
  let state := state.ensureCursorVisible 5
  ensure (state.scrollOffset == 4) "scroll should adjust to show cursor"

test "TextAreaState.insertTab inserts spaces" := do
  let state := TextAreaState.fromString "Hello"
  let state := { state with column := 0 }
  let state := state.insertTab 2
  ensure (state.lines[0]! == "  Hello") "should insert 2 spaces"
  ensure (state.column == 2) "cursor should advance by tab width"

-- ============================================================================
-- TextArea Widget Tests
-- ============================================================================

test "textArea' works in WidgetM" := do
  runSpider do
    let (events, _) ← createInputs
    let (result, _render) ← (runWidget do
      let area ← textArea' "test" "Hello\nWorld" {}
      emitStatic RNode.empty
      pure area
    ).run events

    let content ← SpiderM.liftIO result.content.sample
    SpiderM.liftIO (ensure (content == "Hello\nWorld") "content should match initial")

test "textArea' cursor starts at 0,0" := do
  runSpider do
    let (events, _) ← createInputs
    let (result, _render) ← (runWidget do
      let area ← textArea' "test" "Hello" {}
      emitStatic RNode.empty
      pure area
    ).run events

    let (line, col) ← SpiderM.liftIO result.cursorPos.sample
    SpiderM.liftIO (ensure (line == 0 && col == 0) "cursor should be at 0,0")

test "labeledTextArea' renders label" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _ ← labeledTextArea' "Notes" "notes" "" {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Notes") "should contain label")

test "textDisplay' shows content" := do
  runSpider do
    let (events, _) ← createInputs
    let (contentEvent, _) ← Reactive.newTriggerEvent (t := Spider) (a := String)
    let contentDyn ← Reactive.holdDyn "Line 1\nLine 2" contentEvent
    let (_, render) ← (runWidget do
      textDisplay' contentDyn { showLineNumbers := true }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Line 1") "should contain Line 1")
    SpiderM.liftIO (ensure (rnodeContainsText node "Line 2") "should contain Line 2")

-- ============================================================================
-- TextArea Keyboard Input Tests
-- ============================================================================

test "textArea' accepts character input when focused" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "editor")
    let (res, _render) ← (runWidget do
      textArea' "editor" "" {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Type 'H'
  inputs.fireKey { event := KeyEvent.char 'H', focusedWidget := some "editor" }
  let content1 ← result.content.sample
  ensure (content1 == "H") "should have 'H'"

  -- Type 'i'
  inputs.fireKey { event := KeyEvent.char 'i', focusedWidget := some "editor" }
  let content2 ← result.content.sample
  ensure (content2 == "Hi") "should have 'Hi'"

  env.currentScope.dispose

test "textArea' backspace removes character" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "editor")
    let (res, _render) ← (runWidget do
      textArea' "editor" "Hello" {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move cursor to end first (5 right moves)
  for _ in [:5] do
    inputs.fireKey { event := KeyEvent.right, focusedWidget := some "editor" }

  -- Backspace
  inputs.fireKey { event := { code := .backspace }, focusedWidget := some "editor" }
  let content ← result.content.sample
  ensure (content == "Hell") "should have 'Hell' after backspace"

  env.currentScope.dispose

test "textArea' enter creates newline" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "editor")
    let (res, _render) ← (runWidget do
      textArea' "editor" "Hello" {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move to end
  for _ in [:5] do
    inputs.fireKey { event := KeyEvent.right, focusedWidget := some "editor" }

  -- Press enter
  inputs.fireKey { event := KeyEvent.enter, focusedWidget := some "editor" }
  let content ← result.content.sample
  ensure (content == "Hello\n") "should have newline"

  env.currentScope.dispose

test "textArea' ignores input when not focused" := do
  let env ← SpiderEnv.new
  let (result, inputs) ← (do
    let (events, inputs) ← createInputs
    -- Do NOT focus the editor
    let (res, _render) ← (runWidget do
      textArea' "editor" "" {}
    ).run events
    pure (res, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Try to type (should be ignored because not focused)
  inputs.fireKey { event := KeyEvent.char 'H', focusedWidget := none }
  let content ← result.content.sample
  ensure (content == "") "should still be empty when not focused"

  env.currentScope.dispose



end TerminusTests.Reactive.TextAreaTests
