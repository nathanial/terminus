/-
  Terminus Reactive - Monad Stack
  The ReactiveTermM and WidgetM monads for building reactive terminal widgets.
-/
import Terminus.Reactive.Types
import Terminus.Reactive.Dynamic

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## The ReactiveTermM Monad

Like React's context, ReactiveTermM carries the event streams implicitly.
Components use hooks that access this context without explicit parameters.
-/

/-- ReactiveTermM is SpiderM with implicit access to TerminusEvents.
    This is analogous to how React components access context through hooks. -/
abbrev ReactiveTermM := ReaderT TerminusEvents SpiderM

/-- Explicit ForIn instance for ReactiveTermM to avoid issues with derived instances.
    This properly threads through both the TerminusEvents context and SpiderEnv. -/
instance [ForIn SpiderM ρ α] : ForIn ReactiveTermM ρ α where
  forIn x init f := fun ctx => ForIn.forIn x init fun a b => f a b ctx

/-- Explicit MonadLift instance to allow SpiderM operations in ReactiveTermM. -/
instance : MonadLift SpiderM ReactiveTermM where
  monadLift m := fun _ => m

/-- MonadSample instance for ReactiveTermM - delegates to SpiderM. -/
instance : MonadSample Spider ReactiveTermM where
  sample b := fun _ => b.sample

/-- MonadHold instance for ReactiveTermM - delegates to SpiderM.
    This allows holdDyn, foldDyn, etc. to work directly in ReactiveTermM. -/
instance : MonadHold Spider ReactiveTermM where
  hold initial event := fun _ => MonadHold.hold initial event
  holdDyn initial event := fun _ => MonadHold.holdDyn initial event
  foldDyn f init event := fun _ => MonadHold.foldDyn f init event
  foldDynM f init event := fun ctx =>
    MonadHold.foldDynM (m := SpiderM) (fun a b => f a b ctx) init event

/-- TriggerEvent instance for ReactiveTermM - delegates to SpiderM.
    This allows newTriggerEvent to work directly in ReactiveTermM. -/
instance : TriggerEvent Spider ReactiveTermM where
  newTriggerEvent := fun _ => TriggerEvent.newTriggerEvent
  newEventWithTrigger callback := fun _ => TriggerEvent.newEventWithTrigger callback

/-- Run a ReactiveTermM computation with the given events context. -/
def ReactiveTermM.run (events : TerminusEvents) (m : ReactiveTermM α) : SpiderM α :=
  ReaderT.run m events

/-- Get the events from the implicit context. -/
def getEvents : ReactiveTermM TerminusEvents := read

/-- Register a component and get a name (auto-generated unless `nameOverride` is provided).
    This is the preferred way to register components in ReactiveTermM context. -/
def registerComponent (namePrefix : String) (isInput : Bool := false)
    (isInteractive : Bool := true) (nameOverride : String := "") : ReactiveTermM String := do
  let events ← getEvents
  let name ← SpiderM.liftIO <| events.registry.register namePrefix isInput isInteractive nameOverride
  SpiderM.liftIO <| events.registry.registerGroups name events.focusGroups
  let scope ← SpiderM.getScope
  SpiderM.liftIO <| scope.register (events.registry.unregister name isInput isInteractive)
  pure name

/-- Run a ReactiveTermM computation with a focus group added to the context. -/
def withFocusGroup (group : String) (m : ReactiveTermM α) : ReactiveTermM α :=
  fun events => m { events with focusGroups := group :: events.focusGroups }

/-! ## Type Aliases -/

/-- A component's retained render tree as a Dynamic. -/
abbrev ComponentRender := Dynamic Spider RNode

/-! ## The WidgetM Monad

WidgetM combines FRP network construction (ReactiveTermM) with widget tree accumulation.
This enables Reflex-DOM style monadic widget building where components emit their
renders into the parent container automatically.
-/

/-- State for accumulating child render functions during widget building. -/
structure WidgetMState where
  /-- Array of child component dynamics, accumulated in order. -/
  children : Array ComponentRender := #[]
deriving Inhabited

/-- WidgetM combines FRP setup (ReactiveTermM) with widget accumulation.
    Components use `emit` to add their render functions to the current container. -/
abbrev WidgetM := StateT WidgetMState ReactiveTermM

/-- ForIn instance for WidgetM - threads state through each iteration properly.
    This ensures that emit calls inside for loops accumulate correctly. -/
instance [ForIn ReactiveTermM ρ α] : ForIn WidgetM ρ α where
  forIn x init f := fun s => do
    -- Thread state through by including it in the accumulator
    let (result, finalState) ← ForIn.forIn x (init, s) fun a (b, currentState) => do
      let (step, newState) ← f a b currentState
      match step with
      | .done b' => pure (ForInStep.done (b', newState))
      | .yield b' => pure (ForInStep.yield (b', newState))
    pure (result, finalState)

/-- MonadLift from SpiderM to WidgetM. -/
instance : MonadLift SpiderM WidgetM where
  monadLift m := StateT.lift (fun _ => m)

/-- MonadSample instance for WidgetM - delegates to ReactiveTermM. -/
instance : MonadSample Spider WidgetM where
  sample b := StateT.lift (sample b)

/-- MonadHold instance for WidgetM - delegates to ReactiveTermM. -/
instance : MonadHold Spider WidgetM where
  hold initial event := StateT.lift (MonadHold.hold initial event)
  holdDyn initial event := StateT.lift (MonadHold.holdDyn initial event)
  foldDyn f init event := StateT.lift (MonadHold.foldDyn f init event)
  foldDynM f init event := fun s => do
    let result ← MonadHold.foldDynM (m := ReactiveTermM) (fun a b => (f a b).run' s) init event
    pure (result, s)

/-- TriggerEvent instance for WidgetM - delegates to ReactiveTermM. -/
instance : TriggerEvent Spider WidgetM where
  newTriggerEvent := StateT.lift TriggerEvent.newTriggerEvent
  newEventWithTrigger callback := StateT.lift (TriggerEvent.newEventWithTrigger callback)

/-! ## WidgetM Core Helpers -/

/-- Emit a widget render function into the current container's children.
    This is the primary way components contribute their visual representation. -/
def emit (render : ComponentRender) : WidgetM Unit := do
  modify fun s => { s with children := s.children.push render }

/-- Emit a static node as a constant Dynamic. -/
def emitStatic (node : RNode) : WidgetM Unit := do
  let dyn ← Dynamic.pureM node
  emit dyn

/-- Run a WidgetM computation and extract both the result and collected child renders.
    Used by container combinators to gather children's render functions. -/
def runWidgetChildren (m : WidgetM α) : WidgetM (α × Array ComponentRender) := do
  let parentState ← get
  set (WidgetMState.mk #[])
  let result ← m
  let childState ← get
  set parentState
  pure (result, childState.children)

/-- Run a WidgetM computation in ReactiveTermM context and extract the final render.
    This is used at the top level to get a single ComponentRender from WidgetM. -/
def runWidget (m : WidgetM α) : ReactiveTermM (α × ComponentRender) := do
  let (result, state) ← m.run { children := #[] }
  let childrenList ← liftM (m := SpiderM) <| Reactive.Dynamic.sequence state.children.toList
  let render ← liftM (m := SpiderM) <| childrenList.map' fun kids =>
    if kids.isEmpty then
      RNode.empty
    else if _ : kids.length = 1 then
      kids.head!
    else
      RNode.column 0 {} kids.toArray
  pure (result, render)

/-- Get the events from WidgetM context. -/
def getEventsW : WidgetM TerminusEvents := StateT.lift getEvents

/-- Register a component in WidgetM context. -/
def registerComponentW (namePrefix : String) (isInput : Bool := false)
    (isInteractive : Bool := true) (nameOverride : String := "") : WidgetM String :=
  StateT.lift (registerComponent namePrefix isInput isInteractive nameOverride)

/-- Run a WidgetM computation with a focus group added to the context. -/
def withFocusGroupW (group : String) (m : WidgetM α) : WidgetM α := do
  let s ← get
  let events ← getEventsW
  let newEvents := { events with focusGroups := group :: events.focusGroups }
  let action : ReactiveTermM (α × WidgetMState) := fun _ => (m.run s) newEvents
  let (result, newState) ← StateT.lift action
  set newState
  pure result

end Terminus.Reactive
