-- Terminus.Frame: Rendering frame abstraction for TUI applications

import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Backend.Commands
import Terminus.Backend.Terminal
import Terminus.Input.Key
import Terminus.Input.Events

namespace Terminus

/-- Frame provides a high-level API for rendering widgets to the terminal -/
structure Frame where
  buffer : Buffer
  area : Rect
  commands : List TerminalCommand := []
  deriving Inhabited

namespace Frame

/-- Create a new frame for the given area -/
def new (area : Rect) : Frame := {
  buffer := Buffer.new area.width area.height
  area
  commands := []
}

/-- Create a frame from terminal dimensions -/
def fromTerminal (term : Terminal) : Frame := {
  buffer := term.currentBuffer
  area := term.area
  commands := []
}

/-- Get the drawable area -/
def size (f : Frame) : Rect := f.area

/-- Widget-like rendering that can also emit terminal commands. -/
class FrameWidget (α : Type) where
  render : α → Rect → Frame → Frame

/-- Render a widget into the frame at the given area -/
def render [FrameWidget α] (f : Frame) (widget : α) (area : Rect) : Frame :=
  FrameWidget.render widget area f

/-- Render a widget into the full frame area -/
def renderFull [FrameWidget α] (f : Frame) (widget : α) : Frame :=
  f.render widget f.area

/-- Render a widget with an offset from the frame origin -/
def renderAt [FrameWidget α] (f : Frame) (widget : α) (x y width height : Nat) : Frame :=
  f.render widget { x, y, width, height }

/-- Set a single cell -/
def setCell (f : Frame) (x y : Nat) (cell : Cell) : Frame := {
  f with buffer := f.buffer.set x y cell
}

/-- Write a string at the given position -/
def writeString (f : Frame) (x y : Nat) (s : String) (style : Style := {}) : Frame := {
  f with buffer := f.buffer.writeString x y s style
}

/-- Write a hyperlinked string at the given position -/
def writeLink (f : Frame) (x y : Nat) (s : String) (url : String) (style : Style := {}) : Frame := {
  f with buffer := f.buffer.writeLink x y s url style
}

/-- Clear the frame buffer -/
def clear (f : Frame) : Frame := { f with buffer := f.buffer.clear, commands := [] }

/-- Add a terminal command to the frame. -/
def addCommand (f : Frame) (cmd : TerminalCommand) : Frame :=
  { f with commands := f.commands ++ [cmd] }

end Frame

/-- Application state wrapper for TUI applications -/
structure App (State : Type) where
  state : State
  terminal : Terminal
  shouldQuit : Bool := false
  deriving Inhabited

namespace App

/-- Create a new app with initial state -/
def new (state : State) : IO (App State) := do
  let term ← Terminal.new
  pure { state, terminal := term }

/-- Update the app state -/
def withState (app : App State) (f : State → State) : App State := {
  app with state := f app.state
}

/-- Set the state directly -/
def setState (app : App State) (s : State) : App State := { app with state := s }

/-- Mark the app for exit -/
def quit (app : App State) : App State := { app with shouldQuit := true }

/-- Get the terminal's drawable area -/
def area (app : App State) : Rect := app.terminal.area

/-- Draw function type -/
abbrev DrawFn (State : Type) := Frame → State → Frame

/-- Update function type - called every frame with optional event (keyboard or mouse) -/
abbrev UpdateFn (State : Type) := State → Option Event → State × Bool  -- Bool = shouldQuit

/-- Run a single frame: poll input, update state, render -/
def tick (app : App State) (draw : DrawFn State) (update : UpdateFn State) : IO (App State) := do
  -- Poll for input
  let event ← Events.poll

  -- Convert Event.none to Option none, otherwise wrap in some
  let optEvent := match event with
    | .none => none
    | e => some e

  -- Always call update (every frame) with optional event
  let (newState, shouldQuit) := update app.state optEvent

  let app := { app with state := newState, shouldQuit := app.shouldQuit || shouldQuit }

  if app.shouldQuit then
    return app

  -- Create frame and render
  let frame := Frame.new app.terminal.area
  let frame := draw frame app.state

  -- Update terminal buffer and flush
  let term := app.terminal.setBuffer frame.buffer
  let term ← term.flush frame.commands

  pure { app with terminal := term }

/-- Main run loop -/
partial def run (app : App State) (draw : DrawFn State) (update : UpdateFn State) : IO Unit := do
  if app.shouldQuit then return

  let app ← app.tick draw update
  IO.sleep 16  -- ~60 FPS
  run app draw update

/-- Run a TUI application with setup and teardown -/
def runApp (initialState : State) (draw : DrawFn State) (update : UpdateFn State) : IO Unit := do
  try
    Terminal.setup
    let app ← App.new initialState
    -- Initial draw
    let term ← app.terminal.draw
    let app := { app with terminal := term }
    run app draw update
  finally
    Terminal.teardown

end App

end Terminus
