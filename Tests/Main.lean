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
  let (key, _) := MockTerminal.run (Events.parseKey 97) initial -- 'a'
  ensure (key.code == KeyCode.char 'a') "should parse 'a'"

test "parses Enter key" := do
  let (key, _) := MockTerminal.run (Events.parseKey 13)
  ensure (key.code == KeyCode.enter) "should parse Enter"

test "parses Tab key" := do
  let (key, _) := MockTerminal.run (Events.parseKey 9)
  ensure (key.code == KeyCode.tab) "should parse Tab"

test "parses Backspace key" := do
  let (key, _) := MockTerminal.run (Events.parseKey 127)
  ensure (key.code == KeyCode.backspace) "should parse Backspace"

test "parses Space key" := do
  let (key, _) := MockTerminal.run (Events.parseKey 32)
  ensure (key.code == KeyCode.space) "should parse Space"

test "parses Ctrl C" := do
  let (key, _) := MockTerminal.run (Events.parseKey 3)
  ensure (key.code == KeyCode.char 'c' && key.modifiers.ctrl == true) "should parse Ctrl+C"

test "parses Ctrl D" := do
  let (key, _) := MockTerminal.run (Events.parseKey 4)
  ensure (key.code == KeyCode.char 'd' && key.modifiers.ctrl == true) "should parse Ctrl+D"

-- ============================================================================
-- Escape Sequence Tests
-- ============================================================================

test "parses Up arrow" := do
  let initial := MockTerminal.withInput [91, 65] -- '[', 'A'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.up) "should parse Up arrow"

test "parses Down arrow" := do
  let initial := MockTerminal.withInput [91, 66] -- '[', 'B'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.down) "should parse Down arrow"

test "parses Left arrow" := do
  let initial := MockTerminal.withInput [91, 68] -- '[', 'D'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.left) "should parse Left arrow"

test "parses Right arrow" := do
  let initial := MockTerminal.withInput [91, 67] -- '[', 'C'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.right) "should parse Right arrow"

test "parses Home key" := do
  let initial := MockTerminal.withInput [91, 72] -- '[', 'H'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.home) "should parse Home"

test "parses End key" := do
  let initial := MockTerminal.withInput [91, 70] -- '[', 'F'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.«end») "should parse End"

test "parses Delete key" := do
  let initial := MockTerminal.withInput [91, 51, 126] -- '[', '3', '~'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.delete) "should parse Delete"

test "parses Page Up" := do
  let initial := MockTerminal.withInput [91, 53, 126] -- '[', '5', '~'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.pageUp) "should parse Page Up"

test "parses Page Down" := do
  let initial := MockTerminal.withInput [91, 54, 126] -- '[', '6', '~'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.pageDown) "should parse Page Down"

test "parses F1" := do
  let initial := MockTerminal.withInput [79, 80] -- 'O', 'P'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.f 1) "should parse F1"

test "parses F2" := do
  let initial := MockTerminal.withInput [79, 81] -- 'O', 'Q'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.f 2) "should parse F2"

test "parses Alt a" := do
  let initial := MockTerminal.withInput [97] -- 'a'
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.char 'a' && key.modifiers.alt == true) "should parse Alt+a"

test "parses bare escape when no following bytes" := do
  let initial := MockTerminal.withInput []
  let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
  ensure (key.code == KeyCode.escape) "should parse bare Escape"

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
