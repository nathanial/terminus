/-
  Terminus Reactive - Application Framework
  Entry point for running reactive terminal applications.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Render
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host
open Terminus

namespace Terminus.Reactive

/-! ## Application State

The result of setting up a reactive application.
-/

/-- Application state returned from app setup. -/
structure ReactiveAppState where
  /-- Render function that samples all component state and returns the complete UI. -/
  render : ComponentRender

/-! ## Application Runner -/

/-- Configuration for the reactive app runner. -/
structure AppConfig where
  /-- Frame rate in milliseconds between updates. -/
  frameMs : Nat := 16
  deriving Repr, Inhabited

/-- Run a reactive terminal application.

    The `setup` function receives the TerminusEvents context and should:
    1. Set up reactive state using foldDyn, holdDyn, etc.
    2. Build the widget tree using WidgetM
    3. Return a ReactiveAppState with the render function

    Example:
    ```
    def main : IO Unit := runReactiveApp do
      let keyEvents ← useKeyEvent
      let count ← Reactive.foldDyn (fun _ n => n + 1) 0 keyEvents

      let (_, render) ← runWidget do
        column' (gap := 1) (style := {}) do
          heading1' "Counter" Theme.dark
          dynText' (count.current.map (s!"Count: {·}")) (Theme.dark.bodyStyle)

      pure { render }
    ```
-/
def runReactiveApp (setup : ReactiveTermM ReactiveAppState) (config : AppConfig := {}) : IO Unit := do
  Terminal.setup
  try
    -- Create terminal
    let termRef ← IO.mkRef (← Terminal.new)

    -- Run the reactive network
    Reactive.Host.runSpider do
      -- Create event infrastructure
      let (events, inputs) ← createInputs

      -- Run user setup
      let appState ← setup.run events

      -- Track quit signal
      let quitRef ← SpiderM.liftIO (IO.mkRef false)

      -- Subscribe to Ctrl+C/Ctrl+Q to quit
      let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
        if kd.event.isCtrlC || kd.event.isCtrlQ then
          quitRef.set true

      -- Main loop
      SpiderM.liftIO do
        while !(← quitRef.get) do
          -- Poll for terminal events (non-blocking)
          let ev ← Events.poll

          -- Fire events into reactive network
          match ev with
          | .key ke =>
            let focusedName ← events.registry.focusedInput.sample
            inputs.fireKey { event := ke, focusedWidget := focusedName }
          | .mouse me =>
            inputs.fireMouse { event := me }
          | .resize w h =>
            inputs.fireResize { width := w, height := h }
            -- Update terminal size
            let newTerm ← Terminal.new
            termRef.set newTerm
          | .none => pure ()

          -- Sample and render
          let rootNode ← appState.render
          let (width, height) ← getTerminalSize
          let buffer := Terminus.Reactive.render rootNode width height

          -- Render to terminal
          let term ← termRef.get
          let term := term.setBuffer buffer
          let term ← term.draw
          termRef.set term

          -- Frame delay
          IO.sleep config.frameMs.toUInt32

  finally
    Terminal.teardown

/-! ## Simplified Runner

For simple apps that don't need the full reactive setup.
-/

/-- Run a simple reactive app with just a widget tree.
    Good for static displays or simple demos. -/
def runSimpleApp (widget : WidgetM Unit) : IO Unit := do
  runReactiveApp do
    let (_, render) ← runWidget widget
    pure { render }

/-! ## Testing Helpers -/

/-- Render an RNode tree once and return the buffer (for testing). -/
def renderOnce (widget : WidgetM Unit) (width height : Nat) : IO Buffer := do
  Reactive.Host.runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget widget).run events
    let node ← SpiderM.liftIO render
    pure (Terminus.Reactive.render node width height)

end Terminus.Reactive
