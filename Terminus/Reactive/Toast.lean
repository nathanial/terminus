/-
  Terminus Reactive - Toast Notification Widget
  Auto-dismissing toast notification system with stacking support.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Toast Types -/

/-- Toast notification type determining appearance and styling. -/
inductive ToastType where
  | info
  | success
  | warning
  | error
  deriving Repr, BEq, Inhabited

namespace ToastType

/-- Get the default text style for a toast type. -/
def toStyle : ToastType → Style
  | .info    => { fg := .ansi .blue }
  | .success => { fg := .ansi .green }
  | .warning => { fg := .ansi .yellow }
  | .error   => { fg := .ansi .red }

/-- Get the default border style for a toast type. -/
def toBorderStyle : ToastType → Style
  | .info    => { fg := .ansi .blue }
  | .success => { fg := .ansi .green }
  | .warning => { fg := .ansi .yellow }
  | .error   => { fg := .ansi .red }

/-- Get a label for the toast type. -/
def label : ToastType → String
  | .info    => "Info"
  | .success => "Success"
  | .warning => "Warning"
  | .error   => "Error"

/-- Get a symbol prefix for the toast type. -/
def symbol : ToastType → String
  | .info    => "i"
  | .success => "+"
  | .warning => "!"
  | .error   => "x"

end ToastType

/-! ## Toast Position -/

/-- Position for toast display on screen. -/
inductive ToastPosition where
  | topLeft
  | topCenter
  | topRight
  | bottomLeft
  | bottomCenter
  | bottomRight
  deriving Repr, BEq, Inhabited

/-! ## Toast Configuration -/

/-- Configuration for toast appearance and behavior. -/
structure ToastConfig where
  /-- Duration in milliseconds before auto-dismiss (0 = no auto-dismiss). -/
  duration : Nat := 3000
  /-- Maximum number of visible toasts at once. -/
  maxToasts : Nat := 5
  /-- Position for toast display. -/
  position : ToastPosition := .topRight
  /-- Base style for toast container. -/
  style : Style := {}
  /-- Style for info toasts. -/
  infoStyle : Style := { fg := .ansi .blue }
  /-- Style for success toasts. -/
  successStyle : Style := { fg := .ansi .green }
  /-- Style for warning toasts. -/
  warningStyle : Style := { fg := .ansi .yellow }
  /-- Style for error toasts. -/
  errorStyle : Style := { fg := .ansi .red }
  /-- Border style for toast boxes. -/
  borderStyle : Style := { fg := .ansi .brightBlack }
  /-- Whether to show borders around toasts. -/
  showBorder : Bool := true
  /-- Whether newest toasts appear at the top (true) or bottom (false). -/
  newestFirst : Bool := true
  deriving Repr, Inhabited

namespace ToastConfig

/-- Get the style for a toast type. -/
def styleFor (config : ToastConfig) : ToastType → Style
  | .info    => config.infoStyle
  | .success => config.successStyle
  | .warning => config.warningStyle
  | .error   => config.errorStyle

end ToastConfig

/-! ## Toast Data -/

/-- Individual toast notification with timing info. -/
structure Toast where
  /-- Unique identifier for this toast. -/
  id : Nat
  /-- Display message. -/
  message : String
  /-- Toast type (info, success, warning, error). -/
  toastType : ToastType
  /-- Timestamp when created (elapsed ms from app start). -/
  createdAt : Nat
  deriving Repr, Inhabited, BEq

/-! ## Toast Manager -/

/-- Result returned by toast container widget.
    Provides methods to show/dismiss toasts and access current state. -/
structure ToastManager where
  /-- Show a new toast notification. Safe to call from background tasks.
      Takes message and toast type. -/
  «show» : String → ToastType → IO Unit
  /-- Dismiss a specific toast by ID. -/
  dismiss : Nat → IO Unit
  /-- Dismiss all toasts. -/
  dismissAll : IO Unit
  /-- Current visible toasts as a Dynamic. -/
  toasts : Dynamic Spider (Array Toast)

/-! ## Toast Actions -/

/-- Internal actions for toast state machine. -/
private inductive ToastAction where
  | add (toast : Toast)
  | dismissId (id : Nat)
  | dismissOldest
  | dismissAll
  | tick (currentTime : Nat)

/-! ## Toast State -/

/-- Internal state for the toast container. -/
private structure ToastState where
  /-- Current visible toasts. -/
  toasts : Array Toast
  /-- Next ID for new toasts. -/
  nextId : Nat
  /-- Current elapsed time in milliseconds. -/
  currentTime : Nat
  deriving Inhabited

namespace ToastState

/-- Add a new toast to the state. -/
def addToast (state : ToastState) (message : String) (toastType : ToastType)
    (config : ToastConfig) : ToastState :=
  let toast : Toast := {
    id := state.nextId
    message
    toastType
    createdAt := state.currentTime
  }
  let newToasts := if state.toasts.size >= config.maxToasts then
    if config.newestFirst then
      #[toast] ++ state.toasts.extract 0 (state.toasts.size - 1)
    else
      state.toasts.extract 1 state.toasts.size |>.push toast
  else
    if config.newestFirst then
      #[toast] ++ state.toasts
    else
      state.toasts.push toast
  { state with toasts := newToasts, nextId := state.nextId + 1 }

/-- Dismiss a toast by ID. -/
def dismissById (state : ToastState) (toastId : Nat) : ToastState :=
  { state with toasts := state.toasts.filter fun t => t.id != toastId }

/-- Dismiss all toasts. -/
def dismissAll (state : ToastState) : ToastState :=
  { state with toasts := #[] }

/-- Update time and remove expired toasts. -/
def tick (state : ToastState) (elapsedMs : Nat) (config : ToastConfig) : ToastState :=
  let newState := { state with currentTime := elapsedMs }
  if config.duration > 0 then
    let remaining := newState.toasts.filter fun toast =>
      elapsedMs - toast.createdAt < config.duration
    { newState with toasts := remaining }
  else
    newState

end ToastState

/-! ## Toast Container Widget -/

/-- Internal actions for imperative toast API. -/
private inductive ToastOp where
  | add (message : String) (toastType : ToastType)
  | dismissId (toastId : Nat)
  | dismissAll
  | tick (elapsedMs : Nat)

/-- Create a toast notification container widget.

    The widget provides:
    - Multiple toast types with distinct styling
    - Auto-dismiss after configurable duration
    - Stacking of multiple toasts (newest at top or bottom)
    - Manual dismiss by ID or all at once
    - Thread-safe show/dismiss methods for background tasks

    Visual format:
    ```
    +-- Info ------------------+
    | File saved               |
    +--------------------------+

    +-- Error -----------------+
    | Connection failed        |
    +--------------------------+
    ```

    Example:
    ```
    let toastMgr ← toastContainer' { duration := 3000, maxToasts := 5 }

    -- Show toasts from events
    performEvent_ (← Event.mapM (fun _ => do
      toastMgr.show "Operation completed!" .success
    ) submitEvent)

    -- Access current toasts
    let _ ← dynWidget toastMgr.toasts fun ts =>
      text' s!"Active toasts: {ts.size}" theme.captionStyle
    ```
-/
def toastContainer' (config : ToastConfig := {}) : WidgetM ToastManager := do
  -- Get tick events for auto-dismiss timing
  let tickEvent ← useTickW
  let env ← SpiderM.getEnv

  -- Create trigger event for imperative API calls
  let (opEvent, fireOp) ← newTriggerEvent (t := Spider) (a := ToastOp)

  -- Map tick events to tick operations
  let tickOps ← Event.mapM (fun (td : TickData) => ToastOp.tick td.elapsedMs) tickEvent

  -- Merge all operation events
  let allOps ← Event.leftmostM [opEvent, tickOps]

  -- Initial state
  let initialState : ToastState := { toasts := #[], nextId := 0, currentTime := 0 }

  -- Fold all operations into state - NO subscribe needed!
  let stateDyn : Dynamic Spider ToastState ← foldDyn
    (fun (op : ToastOp) (state : ToastState) =>
      match op with
      | .add message toastType => state.addToast message toastType config
      | .dismissId toastId => state.dismissById toastId
      | .dismissAll => state.dismissAll
      | .tick elapsedMs => state.tick elapsedMs config)
    initialState allOps

  -- Derive toasts dynamic from state
  let toastsDyn : Dynamic Spider (Array Toast) ← stateDyn.map' (·.toasts)

  -- Show function: fires add operation
  let showFn : String → ToastType → IO Unit := fun message toastType => do
    env.withFrame do
      fireOp (ToastOp.add message toastType)

  -- Dismiss by ID function: fires dismissId operation
  let dismissFn : Nat → IO Unit := fun toastId => do
    env.withFrame do
      fireOp (ToastOp.dismissId toastId)

  -- Dismiss all function: fires dismissAll operation
  let dismissAllFn : IO Unit := do
    env.withFrame do
      fireOp ToastOp.dismissAll

  -- Render the toast stack
  let node ← toastsDyn.map' fun (toasts : Array Toast) => Id.run do
    if toasts.isEmpty then
      return RNode.empty
    else
      -- Build toast nodes
      let mut nodes : Array RNode := #[]
      for toast in toasts do
        let style := config.styleFor toast.toastType
        let borderStyle := toast.toastType.toBorderStyle

        if config.showBorder then
          -- Bordered toast with title
          -- Format: +-- Type --+
          --         | message  |
          --         +----------+
          let label := toast.toastType.label
          let msgLen := toast.message.length
          let labelLen := label.length + 4  -- " Type "
          let innerWidth := max msgLen labelLen

          -- Top border with label
          let topLeft := "+-"
          let topRight := "-+"
          let titlePart := s!" {label} "
          let dashCount := if innerWidth > titlePart.length
                           then innerWidth - titlePart.length
                           else 0
          let dashes := String.ofList (List.replicate dashCount '-')
          let topLine := topLeft ++ titlePart ++ dashes ++ topRight

          -- Message line with padding
          let msgPadding := if msgLen < innerWidth then innerWidth - msgLen else 0
          let paddedMsg := toast.message ++ String.ofList (List.replicate msgPadding ' ')
          let msgLine := s!"| {paddedMsg} |"

          -- Bottom border
          let bottomDashes := String.ofList (List.replicate (innerWidth + 2) '-')
          let bottomLine := s!"+{bottomDashes}+"

          -- Create bordered toast node
          let toastNode := RNode.column 0 {} #[
            RNode.text topLine borderStyle,
            RNode.text msgLine style,
            RNode.text bottomLine borderStyle
          ]
          nodes := nodes.push toastNode
        else
          -- Simple format: [symbol] message
          let symbol := toast.toastType.symbol
          let text := s!"[{symbol}] {toast.message}"
          nodes := nodes.push (RNode.text text style)

      -- Stack vertically with gap between toasts
      return RNode.column 1 {} nodes
  emit node

  pure {
    «show» := showFn
    dismiss := dismissFn
    dismissAll := dismissAllFn
    toasts := toastsDyn
  }

/-! ## Event-Driven Toast Widget

The `toastContainerWithEvents'` variant accepts Events instead of exposing
imperative show/dismiss methods. This enables fully declarative FRP composition.
-/

/-- Create an event-driven toast container widget.

    Unlike `toastContainer'` which returns imperative methods, this variant
    is driven by input Events. This makes it easy to compose toast
    notifications from multiple event sources using FRP primitives.

    Example:
    ```
    -- Create show events from form validation
    let validationErrors ← Event.mapM (fun err =>
      (err.message, ToastType.error)) formErrorEvent

    let successEvents ← Event.mapM (fun _ =>
      ("Saved!", ToastType.success)) saveSuccessEvent

    let allShowEvents ← Event.leftmostM [validationErrors, successEvents]

    -- Create dismiss events from escape key
    let escapeEvents ← useEscape
    let dismissAllEvents ← Event.voidM escapeEvents

    -- Event-driven toast container
    toastContainerWithEvents' allShowEvents dismissAllEvents { duration := 3000 }
    ```
-/
def toastContainerWithEvents'
    (showEvents : Reactive.Event Spider (String × ToastType))
    (dismissAllEvents : Reactive.Event Spider Unit)
    (config : ToastConfig := {}) : WidgetM (Dynamic Spider (Array Toast)) := do
  -- Get tick events for auto-dismiss timing
  let tickEvent ← useTickW

  -- Track current time from ticks
  let currentTimeDyn ← foldDyn (fun td _ => td.elapsedMs) 0 tickEvent

  -- Create toast entries from show events (attach current time)
  -- Note: ID is 0 for event-driven API since we don't support dismiss by ID here.
  -- Use the imperative toastContainer' API if you need dismiss by ID.
  let newToastEvents ← Event.attachWithM (fun currentTime pair =>
    let (message, toastType) := pair
    Toast.mk 0 message toastType currentTime
  ) currentTimeDyn.current showEvents

  -- Map each event type to ToastAction
  let addEvents ← Event.mapM ToastAction.add newToastEvents
  let dismissAllEvts ← Event.mapM (fun _ => ToastAction.dismissAll) dismissAllEvents
  let tickEventsAction ← Event.mapM (fun td => ToastAction.tick td.elapsedMs) tickEvent

  let allEvents ← Event.leftmostM [addEvents, dismissAllEvts, tickEventsAction]

  -- Fold over all events to maintain toast state
  let toastsDyn ← foldDyn (fun action toasts =>
    match action with
    | .add toast =>
      if toasts.size >= config.maxToasts then
        if config.newestFirst then
          #[toast] ++ toasts.extract 0 (toasts.size - 1)
        else
          toasts.extract 1 toasts.size |>.push toast
      else
        if config.newestFirst then
          #[toast] ++ toasts
        else
          toasts.push toast
    | .dismissId id =>
      toasts.filter fun t => t.id != id
    | .dismissOldest =>
      if toasts.isEmpty then toasts
      else if config.newestFirst then
        toasts.extract 0 (toasts.size - 1)
      else
        toasts.extract 1 toasts.size
    | .dismissAll => #[]
    | .tick currentTime =>
      if config.duration > 0 then
        toasts.filter fun toast =>
          currentTime - toast.createdAt < config.duration
      else
        toasts
  ) (#[] : Array Toast) allEvents

  -- Render the toast stack
  let node ← toastsDyn.map' fun toasts => Id.run do
    if toasts.isEmpty then
      return RNode.empty
    else
      let mut nodes : Array RNode := #[]
      for toast in toasts do
        let style := config.styleFor toast.toastType
        let borderStyle := toast.toastType.toBorderStyle

        if config.showBorder then
          let label := toast.toastType.label
          let msgLen := toast.message.length
          let labelLen := label.length + 4
          let innerWidth := max msgLen labelLen

          let topLeft := "+-"
          let topRight := "-+"
          let titlePart := s!" {label} "
          let dashCount := if innerWidth > titlePart.length
                           then innerWidth - titlePart.length
                           else 0
          let dashes := String.ofList (List.replicate dashCount '-')
          let topLine := topLeft ++ titlePart ++ dashes ++ topRight

          let msgPadding := if msgLen < innerWidth then innerWidth - msgLen else 0
          let paddedMsg := toast.message ++ String.ofList (List.replicate msgPadding ' ')
          let msgLine := s!"| {paddedMsg} |"

          let bottomDashes := String.ofList (List.replicate (innerWidth + 2) '-')
          let bottomLine := s!"+{bottomDashes}+"

          let toastNode := RNode.column 0 {} #[
            RNode.text topLine borderStyle,
            RNode.text msgLine style,
            RNode.text bottomLine borderStyle
          ]
          nodes := nodes.push toastNode
        else
          let symbol := toast.toastType.symbol
          let text := s!"[{symbol}] {toast.message}"
          nodes := nodes.push (RNode.text text style)

      return RNode.column 1 {} nodes
  emit node

  pure toastsDyn

/-! ## Convenience Functions -/

/-- Create a toast container at a specific position. -/
def toastArea' (position : ToastPosition) (duration : Nat := 3000)
    : WidgetM ToastManager :=
  toastContainer' { position, duration }

/-- Create persistent toasts (no auto-dismiss). -/
def persistentToasts' (maxToasts : Nat := 5) : WidgetM ToastManager :=
  toastContainer' { duration := 0, maxToasts }

/-- Create a quick toast helper for info-level messages.
    Returns a function that shows info toasts with the default duration. -/
def quickToast' (duration : Nat := 2000) : WidgetM (String → IO Unit) := do
  let mgr ← toastContainer' { duration, maxToasts := 1 }
  pure (fun msg => mgr.show msg .info)

/-- Create a toast container that only shows error toasts. -/
def errorToasts' (duration : Nat := 5000) : WidgetM (String → IO Unit) := do
  let mgr ← toastContainer' { duration, maxToasts := 3 }
  pure (fun msg => mgr.show msg .error)

/-- Create a toast container for success messages with shorter duration. -/
def successToasts' (duration : Nat := 2000) : WidgetM (String → IO Unit) := do
  let mgr ← toastContainer' { duration, maxToasts := 1 }
  pure (fun msg => mgr.show msg .success)

end Terminus.Reactive
