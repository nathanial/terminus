/-
  Terminus Reactive - Animation Utilities
  Helpers for frame-based animations, interpolation, and phase state machines.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive.Host

namespace Terminus.Reactive

/-! ## Animation Phase State Machine -/

/-- Animation phase state. -/
inductive AnimPhase where
  /-- No animation running. -/
  | idle
  /-- Animation in progress with name and progress (0.0 to 1.0). -/
  | phase (name : String) (progress : Float)
  deriving Repr, BEq

namespace AnimPhase

/-- Check if animation is idle. -/
def isIdle : AnimPhase → Bool
  | idle => true
  | _ => false

/-- Check if animation is in a specific phase. -/
def isPhase (p : AnimPhase) (name : String) : Bool :=
  match p with
  | phase n _ => n == name
  | idle => false

/-- Get the progress (0.0 if idle). -/
def getProgress : AnimPhase → Float
  | idle => 0.0
  | phase _ p => p

/-- Get the phase name (empty string if idle). -/
def getName : AnimPhase → String
  | idle => ""
  | phase n _ => n

end AnimPhase

/-! ## Animation Structures -/

/-- Configuration for a timed animation. -/
structure AnimationConfig where
  /-- Duration in milliseconds. -/
  durationMs : Nat
  /-- Whether to loop the animation. -/
  loop : Bool := false
  /-- Easing function (default: linear). -/
  easing : Float → Float := id
  deriving Inhabited

/-- Result returned by animation hooks. -/
structure AnimationResult where
  /-- Current progress (0.0 to 1.0). -/
  progress : Reactive.Dynamic Spider Float
  /-- Whether animation is currently running. -/
  isRunning : Reactive.Dynamic Spider Bool
  /-- Start the animation. -/
  start : IO Unit
  /-- Stop the animation. -/
  stop : IO Unit
  /-- Reset to beginning. -/
  reset : IO Unit

/-- Result for interpolated value animations. -/
structure InterpolatedResult where
  /-- Current interpolated value. -/
  value : Reactive.Dynamic Spider Float
  /-- Whether animation is running. -/
  isRunning : Reactive.Dynamic Spider Bool
  /-- Start the animation. -/
  start : IO Unit

/-! ## Easing Functions -/

namespace Easing

/-- Linear easing (no acceleration). -/
def linear (t : Float) : Float := t

/-- Ease in quadratic (accelerating). -/
def easeInQuad (t : Float) : Float := t * t

/-- Ease out quadratic (decelerating). -/
def easeOutQuad (t : Float) : Float := t * (2 - t)

/-- Ease in-out quadratic. -/
def easeInOutQuad (t : Float) : Float :=
  if t < 0.5 then 2 * t * t else -1 + (4 - 2 * t) * t

/-- Ease in cubic. -/
def easeInCubic (t : Float) : Float := t * t * t

/-- Ease out cubic. -/
def easeOutCubic (t : Float) : Float :=
  let t' := t - 1
  t' * t' * t' + 1

/-- Ease in-out cubic. -/
def easeInOutCubic (t : Float) : Float :=
  if t < 0.5 then 4 * t * t * t else (t - 1) * (2 * t - 2) * (2 * t - 2) + 1

/-- Bounce easing. -/
def bounce (t : Float) : Float :=
  if t < 1/2.75 then
    7.5625 * t * t
  else if t < 2/2.75 then
    let t' := t - 1.5/2.75
    7.5625 * t' * t' + 0.75
  else if t < 2.5/2.75 then
    let t' := t - 2.25/2.75
    7.5625 * t' * t' + 0.9375
  else
    let t' := t - 2.625/2.75
    7.5625 * t' * t' + 0.984375

end Easing

/-! ## Animation Hooks -/

/-- Minimum of two floats. -/
private def floatMin (a b : Float) : Float := if a <= b then a else b

/-- Create a basic timed animation.
    Returns progress from 0.0 to 1.0 over the duration.
    Starts when trigger event fires. -/
def useAnimation (config : AnimationConfig)
    (trigger : Reactive.Event Spider Unit) : ReactiveTermM AnimationResult := do
  let tickEvents ← useTick
  let elapsedDyn ← useElapsedMs

  -- State tracking
  let startTimeRef ← SpiderM.liftIO (IO.mkRef (none : Option Nat))
  let runningRef ← SpiderM.liftIO (IO.mkRef false)

  -- Create trigger events
  let (progressEvent, fireProgress) ← Reactive.newTriggerEvent (t := Spider) (a := Float)
  let (runningEvent, fireRunning) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)

  -- Create dynamics
  let progressDyn ← Reactive.holdDyn 0.0 progressEvent
  let runningDyn ← Reactive.holdDyn false runningEvent

  -- Start function
  let startFn : IO Unit := do
    let currentTime ← elapsedDyn.sample
    startTimeRef.set (some currentTime)
    runningRef.set true
    fireRunning true
    fireProgress 0.0

  -- Stop function
  let stopFn : IO Unit := do
    runningRef.set false
    fireRunning false

  -- Reset function
  let resetFn : IO Unit := do
    startTimeRef.set none
    runningRef.set false
    fireRunning false
    fireProgress 0.0

  -- Subscribe to trigger
  let _unsub ← SpiderM.liftIO <| trigger.subscribe fun () => startFn

  -- Subscribe to tick events to update progress
  let _unsub2 ← SpiderM.liftIO <| tickEvents.subscribe fun td => do
    let running ← runningRef.get
    if !running then pure ()
    else
      let startOpt ← startTimeRef.get
      match startOpt with
      | none => pure ()
      | some startTime =>
        let elapsed := td.elapsedMs - startTime
        let rawProgress := Float.ofNat elapsed / Float.ofNat config.durationMs
        let progress := config.easing (floatMin 1.0 rawProgress)

        fireProgress progress

        -- Check if animation is complete
        if rawProgress >= 1.0 then
          if config.loop then
            -- Restart
            startTimeRef.set (some td.elapsedMs)
            fireProgress 0.0
          else
            runningRef.set false
            fireRunning false
            fireProgress 1.0

  pure {
    progress := progressDyn
    isRunning := runningDyn
    start := startFn
    stop := stopFn
    reset := resetFn
  }

/-- Interpolate between two values over time.
    Starts when trigger fires, animates from `fromVal` to `toVal`. -/
def useInterpolation (durationMs : Nat) (fromVal toVal : Float)
    (trigger : Reactive.Event Spider Unit)
    (easing : Float → Float := Easing.linear) : ReactiveTermM InterpolatedResult := do
  let config : AnimationConfig := { durationMs, easing }
  let anim ← useAnimation config trigger

  -- Map progress to interpolated value
  let progressEvent := anim.progress.updated
  let interpFn : Float → Float := fun prog => fromVal + (toVal - fromVal) * prog
  let interpolatedEvent ← Event.mapM interpFn progressEvent
  let valueDyn ← Reactive.holdDyn fromVal interpolatedEvent

  pure {
    value := valueDyn
    isRunning := anim.isRunning
    start := anim.start
  }

/-- Create a multi-phase animation state machine.
    Each phase is (name, durationMs).
    Trigger event starts a phase by name. -/
def useAnimationPhases (phases : Array (String × Nat))
    (trigger : Reactive.Event Spider String) : ReactiveTermM (Reactive.Dynamic Spider AnimPhase) := do
  let tickEvents ← useTick
  let elapsedDyn ← useElapsedMs

  -- State tracking
  let currentPhaseRef ← SpiderM.liftIO (IO.mkRef AnimPhase.idle)
  let startTimeRef ← SpiderM.liftIO (IO.mkRef (0 : Nat))
  let durationRef ← SpiderM.liftIO (IO.mkRef (0 : Nat))

  -- Create phase event and dynamic
  let (phaseEvent, firePhase) ← Reactive.newTriggerEvent (t := Spider) (a := AnimPhase)
  let phaseDyn ← Reactive.holdDyn AnimPhase.idle phaseEvent

  -- Subscribe to trigger to start phases
  let _unsub ← SpiderM.liftIO <| trigger.subscribe fun phaseName => do
    -- Find the phase duration
    match phases.find? (fun (n, _) => n == phaseName) with
    | some (_, duration) =>
      let currentTime ← elapsedDyn.sample
      startTimeRef.set currentTime
      durationRef.set duration
      let newPhase := AnimPhase.phase phaseName 0.0
      currentPhaseRef.set newPhase
      firePhase newPhase
    | none => pure ()

  -- Subscribe to tick events to update phase progress
  let _unsub2 ← SpiderM.liftIO <| tickEvents.subscribe fun td => do
    let current ← currentPhaseRef.get
    match current with
    | .idle => pure ()
    | .phase name _ =>
      let startTime ← startTimeRef.get
      let duration ← durationRef.get
      if duration == 0 then pure ()
      else
        let elapsed := td.elapsedMs - startTime
        let progress := floatMin 1.0 (Float.ofNat elapsed / Float.ofNat duration)

        if progress >= 1.0 then
          -- Phase complete, go idle
          currentPhaseRef.set AnimPhase.idle
          firePhase AnimPhase.idle
        else
          let newPhase := AnimPhase.phase name progress
          currentPhaseRef.set newPhase
          firePhase newPhase

  pure phaseDyn

/-! ## Simple Animation Patterns -/

/-- Create a pulse that toggles on/off at an interval. -/
def usePulse (intervalMs : Nat) : ReactiveTermM (Reactive.Dynamic Spider Bool) := do
  let tickEvents ← useTick

  let stateRef ← SpiderM.liftIO (IO.mkRef false)
  let lastToggleRef ← SpiderM.liftIO (IO.mkRef (0 : Nat))

  let (pulseEvent, firePulse) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
  let pulseDyn ← Reactive.holdDyn false pulseEvent

  let _unsub ← SpiderM.liftIO <| tickEvents.subscribe fun td => do
    let lastToggle ← lastToggleRef.get
    if td.elapsedMs - lastToggle >= intervalMs then
      lastToggleRef.set td.elapsedMs
      let current ← stateRef.get
      let newState := !current
      stateRef.set newState
      firePulse newState

  pure pulseDyn

/-- Create a cycling value from 0.0 to 1.0 over the period. -/
def useCycle (periodMs : Nat) : ReactiveTermM (Reactive.Dynamic Spider Float) := do
  let tickEvents ← useTick

  let cycleEvent ← Event.mapM (fun td =>
    if periodMs == 0 then 0.0
    else Float.ofNat (td.elapsedMs % periodMs) / Float.ofNat periodMs) tickEvents
  let cycleDyn ← Reactive.holdDyn 0.0 cycleEvent

  pure cycleDyn

/-- Create a timer that counts down from a duration.
    Returns remaining time in milliseconds. -/
def useCountdown (durationMs : Nat) (trigger : Reactive.Event Spider Unit)
    : ReactiveTermM (Reactive.Dynamic Spider Nat × Reactive.Dynamic Spider Bool) := do
  let tickEvents ← useTick
  let elapsedDyn ← useElapsedMs

  let startTimeRef ← SpiderM.liftIO (IO.mkRef (none : Option Nat))
  let runningRef ← SpiderM.liftIO (IO.mkRef false)

  let (remainingEvent, fireRemaining) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)
  let (runningEvent, fireRunning) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)

  let remainingDyn ← Reactive.holdDyn durationMs remainingEvent
  let runningDyn ← Reactive.holdDyn false runningEvent

  -- Subscribe to trigger
  let _unsub ← SpiderM.liftIO <| trigger.subscribe fun () => do
    let currentTime ← elapsedDyn.sample
    startTimeRef.set (some currentTime)
    runningRef.set true
    fireRunning true
    fireRemaining durationMs

  -- Subscribe to ticks
  let _unsub2 ← SpiderM.liftIO <| tickEvents.subscribe fun td => do
    let running ← runningRef.get
    if !running then pure ()
    else
      let startOpt ← startTimeRef.get
      match startOpt with
      | none => pure ()
      | some startTime =>
        let elapsed := td.elapsedMs - startTime
        if elapsed >= durationMs then
          runningRef.set false
          fireRunning false
          fireRemaining 0
        else
          fireRemaining (durationMs - elapsed)

  pure (remainingDyn, runningDyn)

/-- Create a blink cursor effect.
    Returns whether cursor should be visible. -/
def useBlinkingCursor (blinkMs : Nat := 530) : ReactiveTermM (Reactive.Dynamic Spider Bool) := do
  usePulse blinkMs

/-! ## WidgetM Versions -/

/-- Create a timed animation in WidgetM. -/
def useAnimationW (config : AnimationConfig)
    (trigger : Reactive.Event Spider Unit) : WidgetM AnimationResult :=
  StateT.lift (useAnimation config trigger)

/-- Create an interpolated animation in WidgetM. -/
def useInterpolationW (durationMs : Nat) (fromVal toVal : Float)
    (trigger : Reactive.Event Spider Unit)
    (easing : Float → Float := Easing.linear) : WidgetM InterpolatedResult :=
  StateT.lift (useInterpolation durationMs fromVal toVal trigger easing)

/-- Create animation phases in WidgetM. -/
def useAnimationPhasesW (phases : Array (String × Nat))
    (trigger : Reactive.Event Spider String) : WidgetM (Reactive.Dynamic Spider AnimPhase) :=
  StateT.lift (useAnimationPhases phases trigger)

/-- Create a pulse in WidgetM. -/
def usePulseW (intervalMs : Nat) : WidgetM (Reactive.Dynamic Spider Bool) :=
  StateT.lift (usePulse intervalMs)

/-- Create a cycle in WidgetM. -/
def useCycleW (periodMs : Nat) : WidgetM (Reactive.Dynamic Spider Float) :=
  StateT.lift (useCycle periodMs)

/-- Create a blinking cursor in WidgetM. -/
def useBlinkingCursorW (blinkMs : Nat := 530) : WidgetM (Reactive.Dynamic Spider Bool) :=
  StateT.lift (useBlinkingCursor blinkMs)

/-- Create a countdown timer in WidgetM. -/
def useCountdownW (durationMs : Nat) (trigger : Reactive.Event Spider Unit)
    : WidgetM (Reactive.Dynamic Spider Nat × Reactive.Dynamic Spider Bool) :=
  StateT.lift (useCountdown durationMs trigger)

end Terminus.Reactive
