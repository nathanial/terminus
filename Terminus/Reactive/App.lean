/-
  Terminus Reactive - Application Framework
  Entry point for running reactive terminal applications.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Render
import Terminus.Reactive.Hooks
import Std.Sync.Channel
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
  /-- Root retained UI tree as a Dynamic. -/
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
inductive LoopSignal where
  | input (ev : Terminus.Event)
  | render
  | tick
  | shutdown
  deriving Repr, BEq, Inhabited

structure LoopDeps (m : Type → Type) where
  /-- Wait for the next loop signal (blocking). -/
  nextSignal : m LoopSignal
  /-- Current monotonic time in milliseconds. -/
  nowMs : m Nat
  /-- Optional debug logger. -/
  log : String → m Unit
  /-- Optional frame limit for tests. -/
  maxFrames : Option Nat := none
  /-- Optional per-frame callback (e.g., capture buffers in tests). -/
  onFrame : Nat → Buffer → m Unit

namespace LoopDeps

/-- Default IO dependencies using real terminal polling and sleep. -/
def io (nextSignal : IO LoopSignal) (log : String → IO Unit := fun _ => pure ()) : LoopDeps IO := {
  nextSignal := nextSignal
  nowMs := IO.monoMsNow
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

  let renderFrame : m Unit := do
    let rootNode ← liftM (m := IO) render.sample
    let (width, height) ← getTerminalSize
    let renderResult := Terminus.Reactive.render rootNode width height
    let buffer := renderResult.buffer
    let commands := renderResult.commands.toList

    let frame ← liftM (m := IO) frameRef.get
    if frame == 0 then
      deps.log s!"First render: size={width}x{height}, node={repr rootNode}"

    let term ← liftM (m := IO) termRef.get
    let term := term.setBuffer buffer

    let changes := Buffer.diff term.previousBuffer term.currentBuffer
    if frame % 60 == 0 then
      deps.log s!"Buffer diff found {changes.length} changes"
      let mut row0 := ""
      for x in [0:20] do
        row0 := row0 ++ (term.currentBuffer.get x 0).char.toString
      deps.log s!"Row 0: '{row0}'"
      let mut row5 := ""
      for x in [0:20] do
        row5 := row5 ++ (term.currentBuffer.get x 5).char.toString
      deps.log s!"Row 5: '{row5}'"

    let term ← term.flush commands
    liftM (m := IO) <| termRef.set term
    deps.onFrame frame buffer

  -- Initial render
  renderFrame

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
      let signal ← deps.nextSignal
      match signal with
      | .shutdown =>
        pure ()
      | .input ev =>
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
          let newTerm ← Terminal.new
          liftM (m := IO) <| termRef.set newTerm
          renderFrame
        | .none => pure ()
        loop
      | .tick =>
        let now ← deps.nowMs
        let elapsed := now - startTime
        liftM (m := IO) <| inputs.fireTick { frame := frame, elapsedMs := elapsed }
        liftM (m := IO) <| frameRef.set (frame + 1)
        if frame % 60 == 0 then
          deps.log s!"Frame {frame}, elapsed {elapsed}ms"
        loop
      | .render =>
        renderFrame
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

    let signalChan ← Std.Channel.new
    let signalSync := Std.Channel.sync signalChan

    -- Render trigger: wake loop when the root dynamic updates
    let renderUnsub ← appState.render.updated.subscribe fun _ =>
      Std.Channel.Sync.send signalSync .render
    spiderEnv.currentScope.register renderUnsub

    -- Input worker (blocking)
    let _inputTask ← IO.asTask (prio := .dedicated) do
      while true do
        let ev ← Events.read
        Std.Channel.Sync.send signalSync (.input ev)

    -- Tick worker (only if requested by hooks)
    let tickEnabled ← events.tickRequested.get
    if tickEnabled then
      let _tickTask ← IO.asTask (prio := .default) do
        while true do
          IO.sleep config.frameMs.toUInt32
          Std.Channel.Sync.send signalSync .tick
      pure ()

    log "Entering main loop"

    let nextSignal : IO LoopSignal :=
      Std.Channel.Sync.recv signalSync

    let deps := LoopDeps.io nextSignal log
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
    let node ← SpiderM.liftIO render.sample
    pure (Terminus.Reactive.render node width height).buffer

end Terminus.Reactive
