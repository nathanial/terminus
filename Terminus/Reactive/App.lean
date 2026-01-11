/-
  Terminus Reactive - Application Framework
  Entry point for running reactive terminal applications.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Render
import Terminus.Reactive.Hooks
import Reactive
import Chronicle

open Reactive Reactive.Host
open Terminus

namespace Terminus.Reactive

instance : MonadLift IO IO where
  monadLift x := x

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
  /-- Enable debug logging to file. -/
  debug : Bool := false
  /-- Log file path for debug output. -/
  logPath : System.FilePath := "reactive_debug.log"
  deriving Repr, Inhabited

/-! ## App Loop Dependencies -/

/-- Dependencies for running the reactive app loop.
    These are parameterized to enable deterministic testing. -/
structure LoopDeps (m : Type → Type) where
  /-- Source of external terminal events (defaults to Events.poll). -/
  eventSource : m Terminus.Event
  /-- Current monotonic time in milliseconds. -/
  nowMs : m Nat
  /-- Sleep for the given milliseconds. -/
  sleepMs : Nat → m Unit
  /-- Optional debug logger. -/
  log : String → m Unit
  /-- Optional frame limit for tests. -/
  maxFrames : Option Nat := none
  /-- Optional per-frame callback (e.g., capture buffers in tests). -/
  onFrame : Nat → Buffer → m Unit

namespace LoopDeps

/-- Default IO dependencies using real terminal polling and sleep. -/
def io (log : String → IO Unit := fun _ => pure ()) : LoopDeps IO := {
  eventSource := Events.poll
  nowMs := IO.monoMsNow
  sleepMs := fun ms => IO.sleep ms.toUInt32
  log := log
  onFrame := fun _ _ => pure ()
}

end LoopDeps

/-- Run the reactive app main loop.
    This is extracted to enable deterministic tests with mock terminal effects. -/
partial def runReactiveLoop [Monad m] [TerminalEffect m] [MonadLift IO m]
    (config : AppConfig)
    (events : TerminusEvents)
    (inputs : TerminusInputs)
    (render : ComponentRender)
    (termRef : IO.Ref Terminal)
    (deps : LoopDeps m) : m Unit := do
  -- Track quit signal
  let quitRef ← liftM (m := IO) (IO.mkRef false)

  -- Track frame counter and start time
  let frameRef ← liftM (m := IO) (IO.mkRef 0)
  let startTime ← deps.nowMs

  -- Subscribe to Ctrl+C/Ctrl+Q to quit
  let _unsub ← liftM (m := IO) <| events.keyEvent.subscribe fun kd => do
    if kd.event.isCtrlC || kd.event.isCtrlQ then
      quitRef.set true

  -- Main loop (runs in m, fires events into the live reactive network)
  let rec loop : m Unit := do
    let quit ← liftM (m := IO) quitRef.get
    let frame ← liftM (m := IO) frameRef.get
    let reachedMax := match deps.maxFrames with
      | some max => frame >= max
      | none => false
    if quit || reachedMax then
      pure ()
    else
      -- Poll for terminal events (non-blocking)
      let ev ← deps.eventSource

      -- Fire events into reactive network
      match ev with
      | .key ke =>
        if ke.isCtrlC || ke.isCtrlQ then
          deps.log s!"Quit signal received: {repr ke.code}"
        deps.log s!"Key event: {repr ke.code}"
        let focusedName ← liftM (m := IO) events.registry.focusedInput.sample
        liftM (m := IO) <| inputs.fireKey { event := ke, focusedWidget := focusedName }
        deps.log "Key event fired"
      | .mouse me =>
        deps.log s!"Mouse event: button={repr me.button} x={me.x} y={me.y}"
        liftM (m := IO) <| inputs.fireMouse { event := me }
      | .resize w h =>
        deps.log s!"Resize event: {w}x{h}"
        liftM (m := IO) <| inputs.fireResize { width := w, height := h }
        -- Update terminal size
        let newTerm ← Terminal.new
        liftM (m := IO) <| termRef.set newTerm
      | .none => pure ()

      -- Fire tick event
      let now ← deps.nowMs
      let elapsed := now - startTime
      liftM (m := IO) <| inputs.fireTick { frame := frame, elapsedMs := elapsed }
      liftM (m := IO) <| frameRef.set (frame + 1)

      -- Log every 60 frames (roughly once per second)
      if frame % 60 == 0 then
        deps.log s!"Frame {frame}, elapsed {elapsed}ms"

      -- Sample and render
      let rootNode ← liftM (m := IO) render
      let (width, height) ← getTerminalSize
      let renderResult := Terminus.Reactive.render rootNode width height
      let buffer := renderResult.buffer
      -- TODO: Process renderResult.commands for images, clipboard, etc.

      -- Log first frame's render output
      if frame == 0 then
        deps.log s!"First render: size={width}x{height}, node={repr rootNode}"

      -- Render to terminal (use flush for differential updates like traditional Terminus)
      let term ← liftM (m := IO) termRef.get
      let term := term.setBuffer buffer

      -- Debug: check how many cells changed and compare specific cells
      let changes := Buffer.diff term.previousBuffer term.currentBuffer
      if frame % 60 == 0 then
        deps.log s!"Buffer diff found {changes.length} changes"
        -- Log first 20 chars of row 0 (title row)
        let mut row0 := ""
        for x in [0:20] do
          row0 := row0 ++ (term.currentBuffer.get x 0).char.toString
        deps.log s!"Row 0: '{row0}'"
        -- Log first 20 chars of row 5 (should be progress bar area)
        let mut row5 := ""
        for x in [0:20] do
          row5 := row5 ++ (term.currentBuffer.get x 5).char.toString
        deps.log s!"Row 5: '{row5}'"

      let term ← term.flush
      liftM (m := IO) <| termRef.set term

      deps.onFrame frame buffer

      -- Frame delay
      deps.sleepMs config.frameMs
      loop

  loop

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
  -- Set up optional debug logger
  let logConfig := Chronicle.Config.default config.logPath
    |>.withLevel .debug
  let loggerOpt ← if config.debug then
    some <$> Chronicle.Logger.create logConfig
  else
    pure none

  let log (msg : String) : IO Unit := match loggerOpt with
    | some logger => logger.debug msg
    | none => pure ()

  log "=== Reactive App Starting ==="

  Terminal.setup
  try
    -- Create SpiderEnv manually (don't use runFresh which fires postBuild after loop)
    -- This follows the pattern from Afferent demos
    let spiderEnv ← Reactive.Host.SpiderEnv.new Reactive.Host.defaultErrorHandler
    log "SpiderEnv created"

    -- Run the reactive network setup within the env
    let (appState, events, inputs) ← (do
      -- Create event infrastructure (pass log function for debug access in setup)
      let (events, inputs) ← createInputs log
      SpiderM.liftIO <| log "Event infrastructure created"

      -- Run user setup
      let appState ← setup.run events
      SpiderM.liftIO <| log "User setup complete"

      pure (appState, events, inputs)
    ).run spiderEnv

    -- Fire post-build event BEFORE the main loop (keep env alive!)
    spiderEnv.postBuildTrigger ()
    log "Post-build event fired"

    -- Track quit signal
    let termRef ← IO.mkRef (← Terminal.new)
    log "Terminal created"

    log "Entering main loop"

    let deps := LoopDeps.io log
    runReactiveLoop config events inputs appState.render termRef deps

    log "Main loop exited"

    -- Dispose scope to clean up subscriptions
    spiderEnv.currentScope.dispose

  finally
    log "Tearing down terminal"
    Terminal.teardown
    -- Close logger if open
    match loggerOpt with
    | some logger => logger.close
    | none => pure ()

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
    pure (Terminus.Reactive.render node width height).buffer

end Terminus.Reactive
