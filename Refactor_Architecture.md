# Terminus Architecture Refactor: From Immediate to Retained FRP

## Objective
Transition Terminus from an **Immediate Mode** architecture (polling `IO RNode` every frame) to a **Push-Based Retained** architecture (`Dynamic RNode`).

**Goals:**
1.  **Zero Polling**: Only render when data actually changes.
2.  **Pure FRP**: Eliminate imperative `.sample` checks in render loops.
3.  **Efficient Updates**: Idle applications should consume 0% CPU.

## Core Concept
Currently, `WidgetM` accumulates a list of `IO RNode` actions (renderers) that are executed every frame.
**New Architecture**: `WidgetM` will build a single `Dynamic RNode` (a reactive value that holds the current View Tree).

## Detailed Implementation Plan

### 1. New Reactive Combinators (`Reactive/Host/Spider.lean`)
We need combinators to aggregate and switch dynamics.

#### `Dynamic.sequence`
Combines a list of Dynamics into a Dynamic List.
```lean
def sequence [BEq a] (dynamics : List (Dynamic Spider a)) : SpiderM (Dynamic Spider (List a)) :=
  -- Fold using zipWithM to build the list
  dynamics.foldrM (fun d acc => Dynamic.zipWithM (· :: ·) d acc) (Dynamic.pureM [])
```

#### `Dynamic.switch`
Unwraps a nested Dynamic, switching the inner subscription when the outer one changes.
```lean
def switch [BEq a] (dd : Dynamic Spider (Dynamic Spider a)) : SpiderM (Dynamic Spider a)
```
*   **Logic**:
    *   Create a generic `Dynamic` (via `createDynamic`).
    *   Subscribe to `dd.updated`. On fire (new inner dyn):
        *   Dispose previous inner subscription.
        *   Sample new inner dyn -> Push to result.
        *   Subscribe to new inner dyn -> Push to result.
    *   Needs careful `IO.Ref` management for the current subscription.

### 2. Redefine `WidgetM` (`Terminus/Reactive/Monad.lean`)
The core monad state changes from imperative actions to reactive values.

#### `WidgetMState`
```lean
structure WidgetMState where
  -- Changed: IO RNode -> Dynamic Spider RNode
  children : Array (Dynamic Spider RNode) := #[]
```

#### `emit`
```lean
-- Changed: Takes a Dynamic RNode instead of ComponentRender
def emit (nodeDyn : Dynamic Spider RNode) : WidgetM Unit := do
  modify fun s => { s with children := s.children.push nodeDyn }
```

#### `runWidget`
Combines all accumulated child dynamics into a single `Dynamic RNode`.
```lean
def runWidget (m : WidgetM α) : ReactiveTermM (α × Dynamic Spider RNode) := do
  let (result, state) ← m.run { children := #[] }
  -- Use sequence to combine array of dynamics
  let childrenList ← SpiderM.liftIO <| Dynamic.sequence state.children.toList
  let rootDyn ← childrenList.map' fun kids =>
     if kids.isEmpty then RNode.empty
     else if kids.length == 1 then kids.head!
     else RNode.column 0 {} (kids.toArray) -- Default to column layout
  pure (result, rootDyn)
```

### 3. Update Components (`Terminus/Reactive/Components.lean`)
Widgets now transform input Dynamics to RNode Dynamics purely.

#### Pure Components
```lean
def text' (s : String) : WidgetM Unit := do
  let node ← Dynamic.pureM (RNode.text s ...)
  emit node
```

#### Dynamic Text
Crucially, `dynText'` no longer samples. It maps.
```lean
def dynText' (d : Dynamic Spider String) : WidgetM Unit := do
  -- Pure transformation in the graph
  let node ← d.map' fun s => RNode.text s ...
  emit node
```

#### Structural Dynamics (`dynWidget`)
Replaces the complex `dynWidget` implementation with `Dyn.switch`.
```lean
def dynWidget (d : Dynamic Spider a) (f : a → WidgetM Unit) : WidgetM Unit := do
   -- Map usage to a Dynamic of a Widget (which returns Dynamic RNode)
   -- This requires lifting WidgetM construction into the graph, which is complex.
   -- Simpler approach:
   let dynDynRNode : Dynamic Spider (Dynamic Spider RNode) ← d.map' fun val => do
      -- We need to RUN the widget builder here to get the inner dynamic
      -- This implies `Dynamic.mapM` needs to support Side Effects (SpiderM)?
      -- SOLUTION: We likely need `dynIf` and `dynList` primitives that use `switch` internally.
      pass
```
*Refinement*: For the initial refactor, we can implement `ifThenElse'` using `Dynamic.zipWith3` (condition, trueBranch, falseBranch) if the branches are static. For true dynamic structure, we need `Dynamic.switch`.

### 4. Rewriting the App Loop (`App.lean`)
The core loop moves from Polling to Event-Driven.

#### `RenderTrigger`
We need a way to wake up the loop when the View changes.
```lean
-- In setup
let (rootDyn, _) ← runWidget ...
let (renderEvent, fireRender) ← newTriggerEvent
-- Subscribe to rootDyn updates
let _ ← rootDyn.updated.subscribe fun _ => fireRender ()
```

#### `runReactiveLoop`
```lean
partial def runLoop (events, renderTrigger, ...) := do
  -- Wait for ANY event (Input OR Render)
  let wakeup ← waitFor [events.key, events.mouse, renderTrigger]

  if wakeup == Render then
     -- Sample ONCE
     let tree ← rootDyn.sample
     let buffer = render tree
     terminal.draw buffer
  else
     -- Propagate input event
     -- (Spider automatically propagates, triggering rootDyn.updated if something changed)
     -- The loop will wake up again via renderTrigger if needed.
```

## Migration Order
1.  **Safe Primitives**: Add `sequence` and `switch` to a new `SpiderExtensions.lean` file.
2.  **Parallel Monad**: Create `WidgetM2` in `MonadNew.lean` to verify types without breaking existing code.
3.  **Component Port**: Port `text'`, `row'`, `dynText'` to `WidgetM2`.
4.  **Loop Switch**: Create `runApp2` that uses `WidgetM2` and the new loop.
5.  **Verify**: Run `UnifiedDemo` with `runApp2`.
6.  **Replace**: Rename `WidgetM2` -> `WidgetM`.
