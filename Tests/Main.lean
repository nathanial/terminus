-- Tests.Main: LSpec test suite for Terminus terminal library

import LSpec
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalMock
import Terminus.Input.Events
import Terminus.Input.Key

open Terminus
open LSpec
open Std (HashMap)

-- ============================================================================
-- Raw Mode Tests
-- ============================================================================

def rawModeTests : TestSeq :=
  group "Raw Mode" $
    test "enableRawMode sets rawModeEnabled to true" (
      let (_, state) := MockTerminal.run TerminalEffect.enableRawMode
      state.rawModeEnabled == true
    ) ++
    test "disableRawMode sets rawModeEnabled to false" (
      let initial : MockTerminalState := { rawModeEnabled := true }
      let (_, state) := MockTerminal.run TerminalEffect.disableRawMode initial
      state.rawModeEnabled == false
    ) ++
    test "withRawMode restores state after action" (
      let action := TerminalEffect.withRawMode (pure ())
      let (_, state) := MockTerminal.run action
      state.rawModeEnabled == false
    ) ++
    test "withRawMode enables raw mode during action" (
      let action := TerminalEffect.withRawMode do
        let s ← get
        pure s.rawModeEnabled
      let (wasEnabled, _) := MockTerminal.run action
      wasEnabled == true
    )

-- ============================================================================
-- Terminal Size Tests
-- ============================================================================

def sizeTests : TestSeq :=
  group "Terminal Size" $
    test "getTerminalSize returns default 80x24" (
      let (size, _) := MockTerminal.run TerminalEffect.getTerminalSize
      size == (80, 24)
    ) ++
    test "getTerminalSize returns configured size" (
      let initial := MockTerminal.withSize 120 40
      let (size, _) := MockTerminal.run TerminalEffect.getTerminalSize initial
      size == (120, 40)
    )

-- ============================================================================
-- Input Reading Tests
-- ============================================================================

def inputTests : TestSeq :=
  group "Input Reading" $
    test "readByte returns none when queue empty" (
      let (result, _) := MockTerminal.run TerminalEffect.readByte
      result == none
    ) ++
    test "readByte returns byte from queue" (
      let initial := MockTerminal.withInput [65, 66, 67] -- 'A', 'B', 'C'
      let (result, _) := MockTerminal.run TerminalEffect.readByte initial
      result == some 65
    ) ++
    test "readByte consumes bytes in order" (
      let initial := MockTerminal.withInput [65, 66]
      let action := do
        let _ ← TerminalEffect.readByte
        TerminalEffect.readByte
      let (result, _) := MockTerminal.run action initial
      result == some 66
    ) ++
    test "readByte drains queue" (
      let initial := MockTerminal.withInput [65]
      let action := do
        let _ ← TerminalEffect.readByte
        TerminalEffect.readByte
      let (result, _) := MockTerminal.run action initial
      result == none
    )

-- ============================================================================
-- Output Writing Tests
-- ============================================================================

def outputTests : TestSeq :=
  group "Output Writing" $
    test "writeStdout accumulates output" (
      let action := do
        TerminalEffect.writeStdout "Hello, "
        TerminalEffect.writeStdout "World!"
      let (_, state) := MockTerminal.run action
      state.outputBuffer == "Hello, World!"
    ) ++
    test "writeStdout clears flushed flag" (
      let action := TerminalEffect.writeStdout "test"
      let (_, state) := MockTerminal.run action
      state.flushed == false
    ) ++
    test "flushStdout sets flushed flag" (
      let action := do
        TerminalEffect.writeStdout "test"
        TerminalEffect.flushStdout
      let (_, state) := MockTerminal.run action
      state.flushed == true
    )

-- ============================================================================
-- Key Parsing Tests
-- ============================================================================

def keyParsingTests : TestSeq :=
  group "Key Parsing" $
    test "parses regular character 'a'" (
      let initial := MockTerminal.withInput []
      let (key, _) := MockTerminal.run (Events.parseKey 97) initial -- 'a'
      key.code == KeyCode.char 'a'
    ) ++
    test "parses Enter key (13)" (
      let (key, _) := MockTerminal.run (Events.parseKey 13)
      key.code == KeyCode.enter
    ) ++
    test "parses Tab key (9)" (
      let (key, _) := MockTerminal.run (Events.parseKey 9)
      key.code == KeyCode.tab
    ) ++
    test "parses Backspace key (127)" (
      let (key, _) := MockTerminal.run (Events.parseKey 127)
      key.code == KeyCode.backspace
    ) ++
    test "parses Space key (32)" (
      let (key, _) := MockTerminal.run (Events.parseKey 32)
      key.code == KeyCode.space
    ) ++
    test "parses Ctrl+C (3)" (
      let (key, _) := MockTerminal.run (Events.parseKey 3)
      key.code == KeyCode.char 'c' && key.modifiers.ctrl == true
    ) ++
    test "parses Ctrl+D (4)" (
      let (key, _) := MockTerminal.run (Events.parseKey 4)
      key.code == KeyCode.char 'd' && key.modifiers.ctrl == true
    )

-- ============================================================================
-- Escape Sequence Tests
-- ============================================================================

def escapeSequenceTests : TestSeq :=
  group "Escape Sequences" $
    test "parses Up arrow (ESC [ A)" (
      let initial := MockTerminal.withInput [91, 65] -- '[', 'A'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.up
    ) ++
    test "parses Down arrow (ESC [ B)" (
      let initial := MockTerminal.withInput [91, 66] -- '[', 'B'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.down
    ) ++
    test "parses Left arrow (ESC [ D)" (
      let initial := MockTerminal.withInput [91, 68] -- '[', 'D'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.left
    ) ++
    test "parses Right arrow (ESC [ C)" (
      let initial := MockTerminal.withInput [91, 67] -- '[', 'C'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.right
    ) ++
    test "parses Home key (ESC [ H)" (
      let initial := MockTerminal.withInput [91, 72] -- '[', 'H'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.home
    ) ++
    test "parses End key (ESC [ F)" (
      let initial := MockTerminal.withInput [91, 70] -- '[', 'F'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.«end»
    ) ++
    test "parses Delete key (ESC [ 3 ~)" (
      let initial := MockTerminal.withInput [91, 51, 126] -- '[', '3', '~'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.delete
    ) ++
    test "parses Page Up (ESC [ 5 ~)" (
      let initial := MockTerminal.withInput [91, 53, 126] -- '[', '5', '~'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.pageUp
    ) ++
    test "parses Page Down (ESC [ 6 ~)" (
      let initial := MockTerminal.withInput [91, 54, 126] -- '[', '6', '~'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.pageDown
    ) ++
    test "parses F1 (ESC O P)" (
      let initial := MockTerminal.withInput [79, 80] -- 'O', 'P'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.f 1
    ) ++
    test "parses F2 (ESC O Q)" (
      let initial := MockTerminal.withInput [79, 81] -- 'O', 'Q'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.f 2
    ) ++
    test "parses Alt+a" (
      let initial := MockTerminal.withInput [97] -- 'a'
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.char 'a' && key.modifiers.alt == true
    ) ++
    test "parses bare escape when no following bytes" (
      let initial := MockTerminal.withInput []
      let (key, _) := MockTerminal.run Events.parseEscapeSequence initial
      key.code == KeyCode.escape
    )

-- ============================================================================
-- Event Polling Tests
-- ============================================================================

-- Helper to check if poll returned a key event with expected code
def pollReturnsKeyCode (input : List UInt8) (expected : KeyCode) : Bool :=
  let (event, _) := MockTerminal.run Events.poll (MockTerminal.withInput input)
  match event with
  | Event.key k => k.code == expected
  | _ => false

def pollTests : TestSeq :=
  group "Event Polling" $
    test "poll returns none when no input" (
      let (event, _) := MockTerminal.run Events.poll
      event == Event.none
    ) ++
    test "poll returns key event for character" (
      pollReturnsKeyCode [97] (KeyCode.char 'a')  -- 'a'
    ) ++
    test "poll handles escape sequences" (
      pollReturnsKeyCode [27, 91, 65] KeyCode.up  -- ESC [ A
    )

-- ============================================================================
-- Main Test Runner
-- ============================================================================

def allTests : HashMap String (List TestSeq) :=
  let m : HashMap String (List TestSeq) := {}
  m.insert "backend" [rawModeTests, sizeTests, inputTests, outputTests]
    |>.insert "input" [keyParsingTests, escapeSequenceTests, pollTests]

def main (args : List String) : IO UInt32 :=
  lspecIO allTests args
