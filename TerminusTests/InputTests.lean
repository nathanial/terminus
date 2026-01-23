-- TerminusTests.InputTests: Tests for input handling, key parsing, escape sequences, mouse events

import Crucible
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalMock
import Terminus.Input.Events
import Terminus.Input.Key

namespace TerminusTests.InputTests

open Terminus
open Crucible

testSuite "Input Tests"

-- ============================================================================
-- Input Reading Tests
-- ============================================================================

test "readByte returns none when queue empty" := do
  let (result, _) := MockTerminal.run TerminalEffect.readByte
  result ≡ none

test "readByte returns byte from queue" := do
  let initial := MockTerminal.withInput [65, 66, 67] -- 'A', 'B', 'C'
  let (result, _) := MockTerminal.run TerminalEffect.readByte initial
  result ≡ some 65

test "readByte consumes bytes in order" := do
  let initial := MockTerminal.withInput [65, 66]
  let action := do
    let _ ← TerminalEffect.readByte
    TerminalEffect.readByte
  let (result, _) := MockTerminal.run action initial
  result ≡ some 66

test "readByte drains queue" := do
  let initial := MockTerminal.withInput [65]
  let action := do
    let _ ← TerminalEffect.readByte
    TerminalEffect.readByte
  let (result, _) := MockTerminal.run action initial
  result ≡ none

test "readByteBlocking returns none when queue empty" := do
  let (result, _) := MockTerminal.run TerminalEffect.readByteBlocking
  result ≡ none

test "readByteBlocking returns byte from queue" := do
  let initial := MockTerminal.withInput [65, 66, 67] -- 'A', 'B', 'C'
  let (result, _) := MockTerminal.run TerminalEffect.readByteBlocking initial
  result ≡ some 65

-- ============================================================================
-- Key Parsing Tests
-- ============================================================================

test "parses regular character a" := do
  let initial := MockTerminal.withInput []
  let (event, _) := MockTerminal.run (Events.parseInput 97) initial -- 'a'
  match event with
  | .key key => key.code ≡ KeyCode.char 'a'
  | _ => ensure false "expected key event"

test "parses Enter key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 13)
  match event with
  | .key key => key.code ≡ KeyCode.enter
  | _ => ensure false "expected key event"

test "parses Tab key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 9)
  match event with
  | .key key => key.code ≡ KeyCode.tab
  | _ => ensure false "expected key event"

test "parses Backspace key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 127)
  match event with
  | .key key => key.code ≡ KeyCode.backspace
  | _ => ensure false "expected key event"

test "parses Space key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 32)
  match event with
  | .key key => key.code ≡ KeyCode.space
  | _ => ensure false "expected key event"

test "parses Ctrl C" := do
  let (event, _) := MockTerminal.run (Events.parseInput 3)
  match event with
  | .key key => do
    key.code ≡ KeyCode.char 'c'
    key.modifiers.ctrl ≡ true
  | _ => ensure false "expected key event"

test "parses Ctrl D" := do
  let (event, _) := MockTerminal.run (Events.parseInput 4)
  match event with
  | .key key => do
    key.code ≡ KeyCode.char 'd'
    key.modifiers.ctrl ≡ true
  | _ => ensure false "expected key event"

-- ============================================================================
-- Escape Sequence Tests
-- ============================================================================

test "parses Up arrow" := do
  let initial := MockTerminal.withInput [91, 65] -- '[', 'A'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.up
  | _ => ensure false "expected key event"

test "parses Down arrow" := do
  let initial := MockTerminal.withInput [91, 66] -- '[', 'B'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.down
  | _ => ensure false "expected key event"

test "parses Left arrow" := do
  let initial := MockTerminal.withInput [91, 68] -- '[', 'D'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.left
  | _ => ensure false "expected key event"

test "parses Right arrow" := do
  let initial := MockTerminal.withInput [91, 67] -- '[', 'C'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.right
  | _ => ensure false "expected key event"

test "parses Home key" := do
  let initial := MockTerminal.withInput [91, 72] -- '[', 'H'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.home
  | _ => ensure false "expected key event"

test "parses End key" := do
  let initial := MockTerminal.withInput [91, 70] -- '[', 'F'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.«end»
  | _ => ensure false "expected key event"

test "parses Delete key" := do
  let initial := MockTerminal.withInput [91, 51, 126] -- '[', '3', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.delete
  | _ => ensure false "expected key event"

test "parses Page Up" := do
  let initial := MockTerminal.withInput [91, 53, 126] -- '[', '5', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.pageUp
  | _ => ensure false "expected key event"

test "parses Page Down" := do
  let initial := MockTerminal.withInput [91, 54, 126] -- '[', '6', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.pageDown
  | _ => ensure false "expected key event"

test "parses F1" := do
  let initial := MockTerminal.withInput [79, 80] -- 'O', 'P'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.f 1
  | _ => ensure false "expected key event"

test "parses F2" := do
  let initial := MockTerminal.withInput [79, 81] -- 'O', 'Q'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.f 2
  | _ => ensure false "expected key event"

test "parses Alt a" := do
  let initial := MockTerminal.withInput [97] -- 'a'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => do
    key.code ≡ KeyCode.char 'a'
    key.modifiers.alt ≡ true
  | _ => ensure false "expected key event"

test "parses bare escape when no following bytes" := do
  let initial := MockTerminal.withInput []
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => key.code ≡ KeyCode.escape
  | _ => ensure false "expected key event"

-- ============================================================================
-- Event Polling Tests
-- ============================================================================

private def pollReturnsKeyCode (input : List UInt8) (expected : KeyCode) : Bool :=
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.key k => k.code == expected
  | _ => false

test "poll returns none when no input" := do
  let (event, _) := MockTerminal.run Events.poll
  event ≡ Event.none

test "poll returns key event for character" := do
  (pollReturnsKeyCode [97] (KeyCode.char 'a')) ≡ true

test "poll handles escape sequences" := do
  (pollReturnsKeyCode [27, 91, 65] KeyCode.up) ≡ true

-- ============================================================================
-- Mouse Event Parsing Tests
-- ============================================================================

private def pollReturnsMouse (input : List UInt8) (btn : MouseButton) (act : MouseAction) (x y : Nat) : Bool :=
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.mouse me => me.button == btn && me.action == act && me.x == x && me.y == y
  | _ => false

test "parses left click press" := do
  -- ESC [ < 0 ; 10 ; 20 M
  let input : List UInt8 := [27, 91, 60, 48, 59, 49, 48, 59, 50, 48, 77]
  (pollReturnsMouse input .left .press 10 20) ≡ true

test "parses right click release" := do
  -- ESC [ < 2 ; 5 ; 15 m
  let input : List UInt8 := [27, 91, 60, 50, 59, 53, 59, 49, 53, 109]
  (pollReturnsMouse input .right .release 5 15) ≡ true

test "parses middle click" := do
  -- ESC [ < 1 ; 8 ; 12 M
  let input : List UInt8 := [27, 91, 60, 49, 59, 56, 59, 49, 50, 77]
  (pollReturnsMouse input .middle .press 8 12) ≡ true

test "parses scroll up" := do
  -- ESC [ < 64 ; 1 ; 1 M
  let input : List UInt8 := [27, 91, 60, 54, 52, 59, 49, 59, 49, 77]
  (pollReturnsMouse input .scrollUp .press 1 1) ≡ true

test "parses scroll down" := do
  -- ESC [ < 65 ; 25 ; 10 M
  let input : List UInt8 := [27, 91, 60, 54, 53, 59, 50, 53, 59, 49, 48, 77]
  (pollReturnsMouse input .scrollDown .press 25 10) ≡ true

test "parses mouse motion" := do
  -- ESC [ < 35 ; 50 ; 30 M (35 = button none + motion flag)
  let input : List UInt8 := [27, 91, 60, 51, 53, 59, 53, 48, 59, 51, 48, 77]
  (pollReturnsMouse input .none .motion 50 30) ≡ true

test "parses mouse with ctrl modifier" := do
  -- ESC [ < 16 ; 3 ; 7 M (16 = 0 + ctrl modifier)
  let input : List UInt8 := [27, 91, 60, 49, 54, 59, 51, 59, 55, 77]
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.mouse me => do
    me.button ≡ .left
    me.modifiers.ctrl ≡ true
  | _ => ensure false "expected mouse event"



end TerminusTests.InputTests
