-- Tests.Main: Crucible test suite for Terminus terminal library

import Crucible
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalMock
import Terminus.Input.Events
import Terminus.Input.Key

open Terminus
open Crucible

namespace Tests.Terminus

testSuite "Terminus Tests"

-- ============================================================================
-- Raw Mode Tests
-- ============================================================================

test "enableRawMode sets rawModeEnabled to true" := do
  let (_, state) := MockTerminal.run TerminalEffect.enableRawMode
  ensure (state.rawModeEnabled == true) "rawModeEnabled should be true"

test "disableRawMode sets rawModeEnabled to false" := do
  let initial : MockTerminalState := { rawModeEnabled := true }
  let (_, state) := MockTerminal.run TerminalEffect.disableRawMode initial
  ensure (state.rawModeEnabled == false) "rawModeEnabled should be false"

test "withRawMode restores state after action" := do
  let action := TerminalEffect.withRawMode (pure ())
  let (_, state) := MockTerminal.run action
  ensure (state.rawModeEnabled == false) "rawModeEnabled should be restored to false"

test "withRawMode enables raw mode during action" := do
  let action := TerminalEffect.withRawMode do
    let s ← get
    pure s.rawModeEnabled
  let (wasEnabled, _) := MockTerminal.run action
  ensure (wasEnabled == true) "rawModeEnabled should be true during action"

-- ============================================================================
-- Terminal Size Tests
-- ============================================================================

test "getTerminalSize returns default 80x24" := do
  let (size, _) := MockTerminal.run TerminalEffect.getTerminalSize
  ensure (size == (80, 24)) "default size should be 80x24"

test "getTerminalSize returns configured size" := do
  let initial := MockTerminal.withSize 120 40
  let (size, _) := MockTerminal.run TerminalEffect.getTerminalSize initial
  ensure (size == (120, 40)) "size should be 120x40"

-- ============================================================================
-- Input Reading Tests
-- ============================================================================

test "readByte returns none when queue empty" := do
  let (result, _) := MockTerminal.run TerminalEffect.readByte
  ensure (result == none) "should return none when queue empty"

test "readByte returns byte from queue" := do
  let initial := MockTerminal.withInput [65, 66, 67] -- 'A', 'B', 'C'
  let (result, _) := MockTerminal.run TerminalEffect.readByte initial
  ensure (result == some 65) "should return first byte"

test "readByte consumes bytes in order" := do
  let initial := MockTerminal.withInput [65, 66]
  let action := do
    let _ ← TerminalEffect.readByte
    TerminalEffect.readByte
  let (result, _) := MockTerminal.run action initial
  ensure (result == some 66) "should return second byte"

test "readByte drains queue" := do
  let initial := MockTerminal.withInput [65]
  let action := do
    let _ ← TerminalEffect.readByte
    TerminalEffect.readByte
  let (result, _) := MockTerminal.run action initial
  ensure (result == none) "should return none after draining"

-- ============================================================================
-- Output Writing Tests
-- ============================================================================

test "writeStdout accumulates output" := do
  let action := do
    TerminalEffect.writeStdout "Hello, "
    TerminalEffect.writeStdout "World!"
  let (_, state) := MockTerminal.run action
  ensure (state.outputBuffer == "Hello, World!") "output should accumulate"

test "writeStdout clears flushed flag" := do
  let action := TerminalEffect.writeStdout "test"
  let (_, state) := MockTerminal.run action
  ensure (state.flushed == false) "flushed should be false"

test "flushStdout sets flushed flag" := do
  let action := do
    TerminalEffect.writeStdout "test"
    TerminalEffect.flushStdout
  let (_, state) := MockTerminal.run action
  ensure (state.flushed == true) "flushed should be true"

-- ============================================================================
-- Key Parsing Tests
-- ============================================================================

test "parses regular character a" := do
  let initial := MockTerminal.withInput []
  let (event, _) := MockTerminal.run (Events.parseInput 97) initial -- 'a'
  match event with
  | .key key => ensure (key.code == KeyCode.char 'a') "should parse 'a'"
  | _ => ensure false "expected key event"

test "parses Enter key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 13)
  match event with
  | .key key => ensure (key.code == KeyCode.enter) "should parse Enter"
  | _ => ensure false "expected key event"

test "parses Tab key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 9)
  match event with
  | .key key => ensure (key.code == KeyCode.tab) "should parse Tab"
  | _ => ensure false "expected key event"

test "parses Backspace key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 127)
  match event with
  | .key key => ensure (key.code == KeyCode.backspace) "should parse Backspace"
  | _ => ensure false "expected key event"

test "parses Space key" := do
  let (event, _) := MockTerminal.run (Events.parseInput 32)
  match event with
  | .key key => ensure (key.code == KeyCode.space) "should parse Space"
  | _ => ensure false "expected key event"

test "parses Ctrl C" := do
  let (event, _) := MockTerminal.run (Events.parseInput 3)
  match event with
  | .key key => ensure (key.code == KeyCode.char 'c' && key.modifiers.ctrl == true) "should parse Ctrl+C"
  | _ => ensure false "expected key event"

test "parses Ctrl D" := do
  let (event, _) := MockTerminal.run (Events.parseInput 4)
  match event with
  | .key key => ensure (key.code == KeyCode.char 'd' && key.modifiers.ctrl == true) "should parse Ctrl+D"
  | _ => ensure false "expected key event"

-- ============================================================================
-- Escape Sequence Tests
-- ============================================================================

test "parses Up arrow" := do
  let initial := MockTerminal.withInput [91, 65] -- '[', 'A'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.up) "should parse Up arrow"
  | _ => ensure false "expected key event"

test "parses Down arrow" := do
  let initial := MockTerminal.withInput [91, 66] -- '[', 'B'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.down) "should parse Down arrow"
  | _ => ensure false "expected key event"

test "parses Left arrow" := do
  let initial := MockTerminal.withInput [91, 68] -- '[', 'D'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.left) "should parse Left arrow"
  | _ => ensure false "expected key event"

test "parses Right arrow" := do
  let initial := MockTerminal.withInput [91, 67] -- '[', 'C'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.right) "should parse Right arrow"
  | _ => ensure false "expected key event"

test "parses Home key" := do
  let initial := MockTerminal.withInput [91, 72] -- '[', 'H'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.home) "should parse Home"
  | _ => ensure false "expected key event"

test "parses End key" := do
  let initial := MockTerminal.withInput [91, 70] -- '[', 'F'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.«end») "should parse End"
  | _ => ensure false "expected key event"

test "parses Delete key" := do
  let initial := MockTerminal.withInput [91, 51, 126] -- '[', '3', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.delete) "should parse Delete"
  | _ => ensure false "expected key event"

test "parses Page Up" := do
  let initial := MockTerminal.withInput [91, 53, 126] -- '[', '5', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.pageUp) "should parse Page Up"
  | _ => ensure false "expected key event"

test "parses Page Down" := do
  let initial := MockTerminal.withInput [91, 54, 126] -- '[', '6', '~'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.pageDown) "should parse Page Down"
  | _ => ensure false "expected key event"

test "parses F1" := do
  let initial := MockTerminal.withInput [79, 80] -- 'O', 'P'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.f 1) "should parse F1"
  | _ => ensure false "expected key event"

test "parses F2" := do
  let initial := MockTerminal.withInput [79, 81] -- 'O', 'Q'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.f 2) "should parse F2"
  | _ => ensure false "expected key event"

test "parses Alt a" := do
  let initial := MockTerminal.withInput [97] -- 'a'
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.char 'a' && key.modifiers.alt == true) "should parse Alt+a"
  | _ => ensure false "expected key event"

test "parses bare escape when no following bytes" := do
  let initial := MockTerminal.withInput []
  let (event, _) := MockTerminal.run Events.parseEscapeSequence initial
  match event with
  | .key key => ensure (key.code == KeyCode.escape) "should parse bare Escape"
  | _ => ensure false "expected key event"

-- ============================================================================
-- Event Polling Tests
-- ============================================================================

/-- Helper to check if poll returned a key event with expected code -/
def pollReturnsKeyCode (input : List UInt8) (expected : KeyCode) : Bool :=
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.key k => k.code == expected
  | _ => false

test "poll returns none when no input" := do
  let (event, _) := MockTerminal.run Events.poll
  ensure (event == Event.none) "should return none when no input"

test "poll returns key event for character" := do
  ensure (pollReturnsKeyCode [97] (KeyCode.char 'a')) "should return key event for 'a'"

test "poll handles escape sequences" := do
  ensure (pollReturnsKeyCode [27, 91, 65] KeyCode.up) "should parse ESC [ A as Up"

-- ============================================================================
-- Mouse Event Parsing Tests
-- ============================================================================

/-- Helper to check if poll returned a mouse event with expected properties -/
def pollReturnsMouse (input : List UInt8) (btn : MouseButton) (act : MouseAction) (x y : Nat) : Bool :=
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.mouse me => me.button == btn && me.action == act && me.x == x && me.y == y
  | _ => false

-- SGR mouse format: ESC [ < Cb ; Cx ; Cy M|m
-- M = press, m = release
-- Cb: 0=left, 1=middle, 2=right, 64=scrollUp, 65=scrollDown, +32=motion

test "parses left click press" := do
  -- ESC [ < 0 ; 10 ; 20 M
  let input : List UInt8 := [27, 91, 60, 48, 59, 49, 48, 59, 50, 48, 77]
  ensure (pollReturnsMouse input .left .press 10 20) "should parse left click at (10, 20)"

test "parses right click release" := do
  -- ESC [ < 2 ; 5 ; 15 m
  let input : List UInt8 := [27, 91, 60, 50, 59, 53, 59, 49, 53, 109]
  ensure (pollReturnsMouse input .right .release 5 15) "should parse right release at (5, 15)"

test "parses middle click" := do
  -- ESC [ < 1 ; 8 ; 12 M
  let input : List UInt8 := [27, 91, 60, 49, 59, 56, 59, 49, 50, 77]
  ensure (pollReturnsMouse input .middle .press 8 12) "should parse middle click at (8, 12)"

test "parses scroll up" := do
  -- ESC [ < 64 ; 1 ; 1 M
  let input : List UInt8 := [27, 91, 60, 54, 52, 59, 49, 59, 49, 77]
  ensure (pollReturnsMouse input .scrollUp .press 1 1) "should parse scroll up"

test "parses scroll down" := do
  -- ESC [ < 65 ; 25 ; 10 M
  let input : List UInt8 := [27, 91, 60, 54, 53, 59, 50, 53, 59, 49, 48, 77]
  ensure (pollReturnsMouse input .scrollDown .press 25 10) "should parse scroll down at (25, 10)"

test "parses mouse motion" := do
  -- ESC [ < 35 ; 50 ; 30 M (35 = button none + motion flag)
  let input : List UInt8 := [27, 91, 60, 51, 53, 59, 53, 48, 59, 51, 48, 77]
  ensure (pollReturnsMouse input .none .motion 50 30) "should parse motion at (50, 30)"

test "parses mouse with ctrl modifier" := do
  -- ESC [ < 16 ; 3 ; 7 M (16 = 0 + ctrl modifier)
  let input : List UInt8 := [27, 91, 60, 49, 54, 59, 51, 59, 55, 77]
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.mouse me =>
    ensure (me.button == .left && me.modifiers.ctrl) "should parse left click with ctrl"
  | _ => ensure false "expected mouse event"

#generate_tests

end Tests.Terminus

def main : IO UInt32 := do
  IO.println "╔══════════════════════════════════════════════════════════════╗"
  IO.println "║                    Terminus Test Suite                       ║"
  IO.println "╚══════════════════════════════════════════════════════════════╝"
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result == 0 then
    IO.println "All tests passed!"
  else
    IO.println "Some tests failed"

  return result
