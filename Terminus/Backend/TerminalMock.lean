-- Terminus.Backend.TerminalMock: Mock implementation of TerminalEffect for testing

import Terminus.Backend.TerminalEffect
import Std.Data.HashMap

namespace Terminus

/-- Mock terminal state for pure testing -/
structure MockTerminalState where
  /-- Simulated terminal dimensions -/
  terminalSize : Nat × Nat := (80, 24)
  /-- Raw mode enabled status -/
  rawModeEnabled : Bool := false
  /-- Queue of bytes to be "read" -/
  inputQueue : List UInt8 := []
  /-- Accumulated output -/
  outputBuffer : String := ""
  /-- Whether output has been flushed -/
  flushed : Bool := true
  /-- Virtual file system for `readFileBytes`. Keys are string paths. -/
  files : Std.HashMap String ByteArray := {}
  deriving Inhabited

/-- State monad for mock terminal operations -/
abbrev MockTerminal := StateM MockTerminalState

instance : TerminalEffect MockTerminal where
  enableRawMode := modify fun s => { s with rawModeEnabled := true }

  disableRawMode := modify fun s => { s with rawModeEnabled := false }

  getTerminalSize := do
    let s ← get
    pure s.terminalSize

  readByte := do
    let s ← get
    match s.inputQueue with
    | [] => pure none
    | b :: rest =>
      set { s with inputQueue := rest }
      pure (some b)

  readByteBlocking := do
    let s ← get
    match s.inputQueue with
    | [] => pure none
    | b :: rest =>
      set { s with inputQueue := rest }
      pure (some b)

  unreadByte b := modify fun s =>
    { s with inputQueue := b :: s.inputQueue }

  writeStdout str := modify fun s =>
    { s with outputBuffer := s.outputBuffer ++ str, flushed := false }

  flushStdout := modify fun s => { s with flushed := true }

  readFileBytes path := do
    let s ← get
    pure (s.files.getD path.toString ByteArray.empty)

  decodeImageBytes _ := pure none

namespace MockTerminal

/-- Run a mock terminal computation with initial state -/
def run (action : MockTerminal α) (initial : MockTerminalState := ({} : MockTerminalState)) : α × MockTerminalState :=
  StateT.run action initial

/-- Run and return only the result -/
def eval (action : MockTerminal α) (initial : MockTerminalState := ({} : MockTerminalState)) : α :=
  (run action initial).1

/-- Run and return only the final state -/
def exec (action : MockTerminal α) (initial : MockTerminalState := ({} : MockTerminalState)) : MockTerminalState :=
  (run action initial).2

/-- Create initial state with preset input bytes -/
def withInput (bytes : List UInt8) : MockTerminalState :=
  { inputQueue := bytes }

/-- Create initial state with preset terminal size -/
def withSize (width height : Nat) : MockTerminalState :=
  { terminalSize := (width, height) }

/-- Create initial state with both input and size -/
def withInputAndSize (bytes : List UInt8) (width height : Nat) : MockTerminalState :=
  { inputQueue := bytes, terminalSize := (width, height) }

/-- Add a file to the virtual filesystem. -/
def withFile (path : String) (bytes : ByteArray) (s : MockTerminalState := ({} : MockTerminalState)) : MockTerminalState :=
  { s with files := s.files.insert path bytes }

/-- Get the captured output from a mock terminal run -/
def getOutput (state : MockTerminalState) : String :=
  state.outputBuffer

/-- Check if raw mode is enabled in the state -/
def isRawMode (state : MockTerminalState) : Bool :=
  state.rawModeEnabled

end MockTerminal

end Terminus
