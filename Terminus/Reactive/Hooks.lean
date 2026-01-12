/-
  Terminus Reactive - Event Hooks
  React-like hooks for subscribing to terminal events.
-/
import Terminus.Reactive.Monad
import Reactive

-- Don't open Reactive to avoid Event name conflict with Terminus.Event
open Reactive.Host

namespace Terminus.Reactive

/-! ## Event Hooks

These are like React hooks - they access the event context implicitly
and set up subscriptions automatically.
-/

/-- Subscribe to all keyboard events. -/
def useKeyEvent : ReactiveTermM (Reactive.Event Spider KeyData) := do
  let events ← getEvents
  pure events.keyEvent

/-- Subscribe to all mouse events. -/
def useMouseEvent : ReactiveTermM (Reactive.Event Spider MouseData) := do
  let events ← getEvents
  pure events.mouseEvent

/-- Subscribe to terminal resize events. -/
def useResize : ReactiveTermM (Reactive.Event Spider ResizeData) := do
  let events ← getEvents
  pure events.resizeEvent

/-- Subscribe to tick events (fired each frame). -/
def useTick : ReactiveTermM (Reactive.Event Spider TickData) := do
  let events ← getEvents
  SpiderM.liftIO <| events.tickRequested.set true
  pure events.tickEvent

/-- Get the current frame number as a dynamic. -/
def useFrame : ReactiveTermM (Reactive.Dynamic Spider Nat) := do
  let tickEvents ← useTick
  let frames ← Event.mapM (fun td => td.frame) tickEvents
  Reactive.holdDyn 0 frames

/-- Get the elapsed time in milliseconds as a dynamic. -/
def useElapsedMs : ReactiveTermM (Reactive.Dynamic Spider Nat) := do
  let tickEvents ← useTick
  let times ← Event.mapM (fun td => td.elapsedMs) tickEvents
  Reactive.holdDyn 0 times

/-! ## Filtered Event Hooks -/

/-- Filter key events by key code. -/
def useKey (code : Terminus.KeyCode) : ReactiveTermM (Reactive.Event Spider KeyData) := do
  let events ← getEvents
  Event.filterM (fun kd => kd.event.code == code) events.keyEvent

/-- Filter key events by character. -/
def useChar (c : Char) : ReactiveTermM (Reactive.Event Spider KeyData) := do
  let events ← getEvents
  Event.filterM (fun kd => kd.event.isChar c) events.keyEvent

/-- Get event when Enter is pressed. -/
def useEnter : ReactiveTermM (Reactive.Event Spider Unit) := do
  let events ← getEvents
  let enterEvents ← Event.filterM (fun kd => kd.event.code == .enter) events.keyEvent
  Event.voidM enterEvents

/-- Get event when Escape is pressed. -/
def useEscape : ReactiveTermM (Reactive.Event Spider Unit) := do
  let events ← getEvents
  let escEvents ← Event.filterM (fun kd => kd.event.code == .escape) events.keyEvent
  Event.voidM escEvents

/-- Get event when a character key is pressed (any printable character). -/
def useAnyChar : ReactiveTermM (Reactive.Event Spider Char) := do
  let events ← getEvents
  Event.mapMaybeM (fun kd =>
    match kd.event.code with
    | .char c => some c
    | _ => none
  ) events.keyEvent

/-- Get event when arrow keys are pressed. -/
def useArrowKeys : ReactiveTermM (Reactive.Event Spider Terminus.KeyCode) := do
  let events ← getEvents
  let isArrow := fun (code : Terminus.KeyCode) => match code with
    | .up | .down | .left | .right => true
    | _ => false
  Event.mapMaybeM (fun kd =>
    if isArrow kd.event.code then some kd.event.code else none
  ) events.keyEvent

/-! ## Mouse Event Hooks -/

/-- Get left click events. -/
def useLeftClick : ReactiveTermM (Reactive.Event Spider MouseData) := do
  let events ← getEvents
  Event.filterM (fun md => md.event.isLeftClick) events.mouseEvent

/-- Get right click events. -/
def useRightClick : ReactiveTermM (Reactive.Event Spider MouseData) := do
  let events ← getEvents
  Event.filterM (fun md => md.event.isRightClick) events.mouseEvent

/-- Get scroll events. -/
def useScroll : ReactiveTermM (Reactive.Event Spider MouseData) := do
  let events ← getEvents
  Event.filterM (fun md => md.event.isScroll) events.mouseEvent

/-- Get mouse position updates (motion events). -/
def useMousePosition : ReactiveTermM (Reactive.Dynamic Spider (Nat × Nat)) := do
  let events ← getEvents
  let positions ← Event.mapM (fun md => (md.event.x, md.event.y)) events.mouseEvent
  Reactive.holdDyn (0, 0) positions

/-! ## Focus Management -/

/-- Get the currently focused input widget name. -/
def useFocusedInput : ReactiveTermM (Reactive.Dynamic Spider (Option String)) := do
  let events ← getEvents
  pure events.registry.focusedInput

/-- Set the focused input widget. -/
def setFocus (name : Option String) : ReactiveTermM Unit := do
  let events ← getEvents
  SpiderM.liftIO (events.registry.fireFocus name)

/-- Check if a specific widget is focused. -/
def useIsFocused (name : String) : ReactiveTermM (Reactive.Dynamic Spider Bool) := do
  let focusedInput ← useFocusedInput
  Dynamic.mapM (· == some name) focusedInput

/-! ## State Management Helpers -/

/-- Create a counter that increments on each event occurrence. -/
def useCounter (event : Reactive.Event Spider α) (initial : Int := 0)
    : ReactiveTermM (Reactive.Dynamic Spider Int) := do
  Reactive.foldDyn (fun _ n => n + 1) initial event

/-- Create a toggle that flips on each event occurrence. -/
def useToggle (event : Reactive.Event Spider α) (initial : Bool := false)
    : ReactiveTermM (Reactive.Dynamic Spider Bool) := do
  Reactive.foldDyn (fun _ b => !b) initial event

/-- Hold the latest value from an event stream. -/
def useLatest [Inhabited α] (event : Reactive.Event Spider α)
    : ReactiveTermM (Reactive.Dynamic Spider α) := do
  Reactive.holdDyn default event

/-! ## WidgetM Versions

Convenience wrappers for using hooks in WidgetM context.
-/

/-- Subscribe to keyboard events in WidgetM. -/
def useKeyEventW : WidgetM (Reactive.Event Spider KeyData) :=
  StateT.lift useKeyEvent

/-- Subscribe to mouse events in WidgetM. -/
def useMouseEventW : WidgetM (Reactive.Event Spider MouseData) :=
  StateT.lift useMouseEvent

/-- Subscribe to resize events in WidgetM. -/
def useResizeW : WidgetM (Reactive.Event Spider ResizeData) :=
  StateT.lift useResize

/-- Get focused input in WidgetM. -/
def useFocusedInputW : WidgetM (Reactive.Dynamic Spider (Option String)) :=
  StateT.lift useFocusedInput

/-- Subscribe to tick events in WidgetM. -/
def useTickW : WidgetM (Reactive.Event Spider TickData) :=
  StateT.lift useTick

/-- Get current frame number in WidgetM. -/
def useFrameW : WidgetM (Reactive.Dynamic Spider Nat) :=
  StateT.lift useFrame

/-- Get elapsed time in WidgetM. -/
def useElapsedMsW : WidgetM (Reactive.Dynamic Spider Nat) :=
  StateT.lift useElapsedMs

/-- Get key events filtered to only fire when this widget is focused.
    If globalKeys is true, returns all key events regardless of focus. -/
def useFocusedKeyEventsW (widgetName : String) (globalKeys : Bool := false)
    : WidgetM (Reactive.Event Spider KeyData) := do
  let events ← getEventsW
  if globalKeys then
    pure events.keyEvent
  else
    let focusedInput ← useFocusedInputW
    let isFocusedDyn ← Dynamic.map' focusedInput (· == some widgetName)
    Event.gateM isFocusedDyn.current events.keyEvent

/-- Get key events filtered to only fire when a visibility dynamic is true.
    Useful for dialogs and overlays that should only respond when visible. -/
def useVisibilityGatedKeyEventsW (visible : Reactive.Dynamic Spider Bool)
    : WidgetM (Reactive.Event Spider KeyData) := do
  let events ← getEventsW
  Event.gateM visible.current events.keyEvent

end Terminus.Reactive
