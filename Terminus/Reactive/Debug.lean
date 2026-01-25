/-
  Terminus Debug Mode
  File-based rendering for reproducible testing and verification.
-/

import Terminus.Core.Buffer
import Terminus.Input.Key
import Terminus.Backend.TerminalMock
import Terminus.Reactive.App
import Terminus.Reactive.Monad
import Terminus.Reactive.Render
import Reactive

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

namespace Terminus.Reactive.Debug

/-! ## Scripted Input -/

/-- A scripted input event with optional delay in milliseconds. -/
structure ScriptedEvent where
  event : Terminus.Event
  delayMs : Nat := 0
  deriving Repr

/-- A sequence of scripted input events for reproducible testing. -/
abbrev InputScript := List ScriptedEvent

namespace InputScript

/-- Create a script from a list of key events. -/
def fromKeyEvents (keys : List KeyEvent) : InputScript :=
  keys.map fun ke => { event := .key ke }

/-- Create a script from a list of key codes (no modifiers). -/
def fromKeyCodes (codes : List KeyCode) : InputScript :=
  codes.map fun code => { event := .key { code } }

/-- Create a script from a string (each character becomes a key event). -/
def fromString (s : String) : InputScript :=
  s.toList.map fun c => { event := .key (KeyEvent.char c) }

/-- Add a scripted event to the end of the script. -/
def push (script : InputScript) (event : Terminus.Event) (delayMs : Nat := 0) : InputScript :=
  script ++ [{ event := event, delayMs := delayMs }]

/-- Add a key event to the script. -/
def pushKey (script : InputScript) (ke : KeyEvent) (delayMs : Nat := 0) : InputScript :=
  script.push (.key ke) delayMs

/-- Add a resize event to the script. -/
def pushResize (script : InputScript) (width height : Nat) (delayMs : Nat := 0) : InputScript :=
  script.push (.resize width height) delayMs

end InputScript

/-! ## Debug Configuration -/

/-- Configuration for debug mode. -/
structure DebugConfig where
  /-- Directory to write frame files. -/
  outputDir : System.FilePath := ".debug"
  /-- Terminal width. -/
  width : Nat := 80
  /-- Terminal height. -/
  height : Nat := 24
  /-- Maximum frames to capture (none = unlimited until script ends). -/
  maxFrames : Option Nat := none
  /-- Whether to write files or just capture in memory. -/
  writeFiles : Bool := true
  deriving Repr, Inhabited

/-! ## Debug State -/

/-- Captured debug state after running. -/
structure DebugState where
  /-- Previous buffer for change detection. -/
  previousBuffer : Option Buffer := none
  /-- Number of frames written. -/
  framesWritten : Nat := 0
  /-- Captured frames: (frame number, plain text content). -/
  capturedFrames : Array (Nat × String) := #[]
  deriving Repr, Inhabited

/-! ## Change Detection -/

/-- Check if buffer has changed from previous state. -/
def hasChanged (prev : Option Buffer) (current : Buffer) : Bool :=
  match prev with
  | none => true  -- First frame always counts as changed
  | some p => !(Buffer.diff p current).isEmpty

/-! ## Mock Terminal IO for Debug Mode -/

/-- MockTerminalIO for hybrid mock/IO operations (like in integration tests). -/
abbrev DebugTerminalIO := StateT MockTerminalState IO

instance : TerminalEffect DebugTerminalIO where
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

/-! ## Debug Runner -/

/-- Format frame number as zero-padded string. -/
private def formatFrameNumber (n : Nat) : String :=
  let s := toString n
  let padLen := if s.length >= 3 then 0 else 3 - s.length
  let padding := "".pushn '0' padLen
  padding ++ s

/-- Run a reactive app with scripted input, capturing changed frames.
    Returns the debug state with captured frames. -/
def runDebugCapture
    (setup : ReactiveTermM ReactiveAppState)
    (config : DebugConfig)
    (script : InputScript) : IO DebugState := do
  let spiderEnv ← SpiderEnv.new defaultErrorHandler

  let (appState, events, inputs) ← (do
    let (events, inputs) ← createInputs
    let appState ← setup.run events
    pure (appState, events, inputs)
  ).run spiderEnv

  spiderEnv.postBuildTrigger ()

  let stateRef ← IO.mkRef ({ capturedFrames := #[] } : DebugState)
  let clockRef ← IO.mkRef 0
  let scriptRef ← IO.mkRef script
  let frameCountRef ← IO.mkRef 0
  -- Track if we need to send a render signal after an input
  let pendingRenderRef ← IO.mkRef false

  let deps : LoopDeps DebugTerminalIO := {
    nextSignal := do
      -- Check if we need to send a render signal
      let pendingRender ← liftM (m := IO) pendingRenderRef.get
      if pendingRender then
        liftM (m := IO) (pendingRenderRef.set false)
        pure .render
      else
        let script ← liftM (m := IO) scriptRef.get
        match script with
        | [] => pure .shutdown
        | ev :: rest =>
          liftM (m := IO) (scriptRef.set rest)
          if ev.delayMs > 0 then
            liftM (m := IO) (IO.sleep ev.delayMs.toUInt32)
          match ev.event with
          | .none => pure .tick  -- Treat none as tick
          | other =>
            -- After input, schedule a render
            liftM (m := IO) (pendingRenderRef.set true)
            pure (.input other)
    nowMs := do
      let n ← liftM (m := IO) clockRef.get
      liftM (m := IO) (clockRef.set (n + 16))
      pure n
    log := fun _ => pure ()
    maxFrames := config.maxFrames
    onFrame := fun frameNum buf => do
      let state ← liftM (m := IO) stateRef.get
      let changed := hasChanged state.previousBuffer buf
      if changed then
        let plainText := buf.toPlainText
        let newState := {
          previousBuffer := some buf
          framesWritten := state.framesWritten + 1
          capturedFrames := state.capturedFrames.push (frameNum, plainText)
        }
        liftM (m := IO) (stateRef.set newState)
      else
        liftM (m := IO) (stateRef.set { state with previousBuffer := some buf })
      liftM (m := IO) (frameCountRef.set (frameNum + 1))
  }

  let action : DebugTerminalIO Unit := do
    let term ← Terminal.new
    let termRef ← liftM (m := IO) (IO.mkRef term)
    runReactiveLoop { frameMs := 0 } events inputs appState.render termRef deps

  let initialState : MockTerminalState := { terminalSize := (config.width, config.height) }
  let _ ← action.run initialState

  spiderEnv.currentScope.dispose
  stateRef.get

/-- Run a reactive app with scripted input, writing changed frames to files.
    Returns the debug state with captured frames. -/
def runDebug
    (setup : ReactiveTermM ReactiveAppState)
    (config : DebugConfig)
    (script : InputScript) : IO DebugState := do
  -- First capture all frames
  let state ← runDebugCapture setup config script

  -- Write files if enabled
  if config.writeFiles && state.capturedFrames.size > 0 then
    IO.FS.createDirAll config.outputDir
    for (frameNum, content) in state.capturedFrames do
      let filename := s!"frame-{formatFrameNumber frameNum}.txt"
      let path := config.outputDir / filename
      IO.FS.writeFile path content

  pure state

end Terminus.Reactive.Debug
