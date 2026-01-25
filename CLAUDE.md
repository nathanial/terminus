# CLAUDE.md

Terminus is a reactive TUI (Terminal User Interface) framework for Lean 4.

## Build Commands

```bash
lake build        # Build the library
lake test         # Run tests
```

## Architecture

```
Terminus/
├── Reactive/           # FRP-powered widget system
│   ├── Monad.lean      # WidgetM monad, ReactiveM
│   ├── Hooks.lean      # useFocusedKeyEventsW, useHover, etc.
│   ├── Components.lean # Base component utilities
│   ├── Controls.lean   # Button, Slider, Stepper, etc.
│   ├── List.lean       # SelectableList, NumberedList
│   └── ...             # Other widgets
├── Core/               # Core types (Style, RNode, etc.)
└── Terminal/           # Terminal I/O, ANSI codes
```

## Idiomatic FRP Pattern (IMPORTANT)

**DO NOT use `IO.mkRef + subscribe + ref.get/set` for widget state.**

Instead, use the declarative `foldDyn` pattern:

```lean
-- 1. Map key events to state transformation functions
let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
  match kd.event.code with
  | .up => some fun (state : MyState) => state.moveUp
  | .down => some fun (state : MyState) => state.moveDown
  | .enter => some fun (state : MyState) => state.confirm
  | _ => none) keyEvents

-- 2. Fold state operations - NO subscribe needed!
let stateDyn ← foldDyn (fun op state => op state) initialState stateOps

-- 3. Derive output dynamics from state
let selectedDyn ← stateDyn.map' (·.selected)
let valueDyn ← stateDyn.map' (·.value)

-- 4. Derive output events from state updates
let changeEvent := stateDyn.updated

-- 5. For events that need current state at moment of trigger:
let enterEvents ← Event.filterM (fun kd => kd.event.code == .enter) keyEvents
let selectEvent ← Event.attachWithM
  (fun (state : MyState) _ => state.selectedItem)
  stateDyn.current enterEvents
```

### Why This Pattern?

- **Declarative**: State transitions are pure functions, easy to reason about
- **No side effects**: No `IO.mkRef`, no `subscribe`, no manual `fire` calls
- **Composable**: Events and dynamics compose naturally
- **Testable**: Pure state transition functions can be unit tested

### Anti-Patterns to Avoid

```lean
-- BAD: Imperative state management
let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
let (stateEvent, fireState) ← newTriggerEvent
let stateDyn ← holdDyn initialState stateEvent
let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
  let state ← stateRef.get
  let newState := computeNewState state kd
  stateRef.set newState
  fireState newState

-- GOOD: Declarative FRP (see pattern above)
```

### When Subscribe IS Appropriate

Only use `subscribe` for external side effects that cannot be expressed in FRP:
- Calling external APIs (e.g., `scrollToY` callback)
- Focus management (`registry.fireFocus`)
- Logging/debugging

## Key FRP Combinators

```lean
-- Create dynamic from initial value + update event
holdDyn : α → Event Spider α → m (Dynamic Spider α)

-- Fold events into state (preferred for widget state)
foldDyn : (α → β → β) → β → Event Spider α → m (Dynamic Spider β)

-- Transform events
Event.mapMaybeM : (α → Option β) → Event Spider α → m (Event Spider β)
Event.filterM : (α → Bool) → Event Spider α → m (Event Spider α)
Event.mapM : (α → β) → Event Spider α → m (Event Spider β)

-- Attach behavior value to events
Event.attachWithM : (β → α → γ) → Behavior Spider β → Event Spider α → m (Event Spider γ)

-- Merge events (first one wins on simultaneous)
Event.leftmostM : List (Event Spider α) → m (Event Spider α)
Event.mergeM : Event Spider α → Event Spider α → m (Event Spider α)

-- Transform dynamics (requires BEq for change detection)
Dynamic.map' : [BEq β] → Dynamic Spider α → (α → β) → m (Dynamic Spider β)
Dynamic.zipWith' : Dynamic Spider α → (α → β → γ) → Dynamic Spider β → m (Dynamic Spider γ)

-- Get update event from dynamic
Dynamic.updated : Dynamic Spider α → Event Spider α
```

## Widget Hooks

```lean
-- Get key events when widget is focused
useFocusedKeyEventsW : String → Bool → WidgetM (Event Spider KeyData)

-- Check if widget is focused
useIsFocused : String → WidgetM (Dynamic Spider Bool)

-- Get hover state
useHover : String → ReactiveM (Dynamic Spider Bool)

-- Get click events
useClick : String → ReactiveM (Event Spider Unit)
```

## Debug Mode

Terminus supports frame capture for debugging and testing TUI applications.

### Live Frame Capture

Enable frame capture during normal operation via `AppConfig.debugDir`:

```lean
def main : IO Unit := do
  let config : AppConfig := { debugDir := some ".debug" }
  runReactiveApp (config := config) do
    -- ... your app setup
```

This writes changed frames to `.debug/frame-NNN.txt` as plain text (no ANSI codes).

### Scripted Debug Capture

For reproducible testing, use `Debug.runDebugCapture` with scripted input:

```lean
import Terminus.Reactive.Debug

def testMyApp : IO Unit := do
  let script : InputScript := InputScript.fromKeyCodes [.down, .down, .enter]
  let config : DebugConfig := { width := 80, height := 24, writeFiles := true }
  let state ← Debug.runDebugCapture myAppSetup config script
  -- state.capturedFrames contains (frameNum, plainText) pairs
```

This runs the app with mock terminal input, capturing frames for verification.
