/-
  Terminus Reactive - Notification Widget
  Toast-style notifications with auto-dismiss.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Notification Level -/

/-- Notification severity level. -/
inductive NotificationLevel where
  | info
  | success
  | warning
  | error
  deriving Repr, BEq, Inhabited

namespace NotificationLevel

/-- Get the default text style for a notification level. -/
def toStyle : NotificationLevel → Style
  | .info    => { fg := .ansi .blue }
  | .success => { fg := .ansi .green }
  | .warning => { fg := .ansi .yellow }
  | .error   => { fg := .ansi .red }

/-- Get the default border style for a notification level. -/
def toBorderStyle : NotificationLevel → Style
  | .info    => { fg := .ansi .blue }
  | .success => { fg := .ansi .green }
  | .warning => { fg := .ansi .yellow }
  | .error   => { fg := .ansi .red }

/-- Get a symbol prefix for the notification level. -/
def symbol : NotificationLevel → String
  | .info    => "i"
  | .success => "+"
  | .warning => "!"
  | .error   => "x"

end NotificationLevel

/-! ## Notification Position -/

/-- Position for notification display. -/
inductive NotificationPosition where
  | topLeft
  | topCenter
  | topRight
  | bottomLeft
  | bottomCenter
  | bottomRight
  deriving Repr, BEq, Inhabited

/-! ## Internal Notification Entry -/

/-- Internal notification entry with timing info. -/
private structure NotificationEntry where
  id : Nat
  level : NotificationLevel
  message : String
  createdAt : Nat  -- Elapsed ms when created
  deriving Repr, Inhabited

/-! ## Notification Configuration -/

/-- Configuration for notification appearance and behavior. -/
structure NotificationConfig where
  /-- Auto-dismiss time in milliseconds (0 = no auto-dismiss). -/
  durationMs : Nat := 3000
  /-- Maximum number of visible notifications. -/
  maxVisible : Nat := 3
  /-- Display position. -/
  position : NotificationPosition := .topRight
  /-- Style for info notifications. -/
  infoStyle : Style := { fg := .ansi .blue }
  /-- Style for success notifications. -/
  successStyle : Style := { fg := .ansi .green }
  /-- Style for warning notifications. -/
  warningStyle : Style := { fg := .ansi .yellow }
  /-- Style for error notifications. -/
  errorStyle : Style := { fg := .ansi .red }
  deriving Repr, Inhabited

namespace NotificationConfig

/-- Get the style for a notification level. -/
def styleFor (config : NotificationConfig) : NotificationLevel → Style
  | .info    => config.infoStyle
  | .success => config.successStyle
  | .warning => config.warningStyle
  | .error   => config.errorStyle

end NotificationConfig

/-! ## Notification Result -/

/-- Result returned by notification widget. -/
structure NotificationResult where
  /-- Show a new notification. Safe to call from background tasks. -/
  «show» : NotificationLevel → String → IO Unit
  /-- Dismiss the oldest notification. -/
  dismiss : IO Unit
  /-- Dismiss all notifications. -/
  dismissAll : IO Unit

/-! ## Notification Widget -/

/-- Create a toast-style notification widget.

    The widget handles:
    - Multiple notification types with styling
    - Auto-dismiss timer
    - Stack multiple notifications
    - Manual dismiss support

    The `show`, `dismiss`, and `dismissAll` functions can be safely
    called from background tasks.

    Example:
    ```
    let notif ← notifications' { durationMs := 3000 }
    -- Later:
    notif.show .success "File saved successfully!"
    notif.show .error "Connection lost"
    ```
-/
def notifications' (config : NotificationConfig := {}) : WidgetM NotificationResult := do
  -- Get tick events for auto-dismiss timing
  let tickEvent ← useTickW
  let env ← SpiderM.getEnv

  -- State
  let entriesRef ← SpiderM.liftIO (IO.mkRef (#[] : Array NotificationEntry))
  let nextIdRef ← SpiderM.liftIO (IO.mkRef 0)
  let currentTimeRef ← SpiderM.liftIO (IO.mkRef 0)

  let (entriesEvent, fireEntries) ← newTriggerEvent (t := Spider) (a := Array NotificationEntry)
  let entriesDyn ← holdDyn #[] entriesEvent

  -- Subscribe to tick events for auto-dismiss
  let _unsub ← SpiderM.liftIO <| tickEvent.subscribe fun td => do
    currentTimeRef.set td.elapsedMs

    if config.durationMs > 0 then
      let entries ← entriesRef.get
      let currentTime := td.elapsedMs

      -- Filter out expired notifications
      let remaining := entries.filter fun entry =>
        currentTime - entry.createdAt < config.durationMs

      if remaining.size != entries.size then
        env.withFrame do
          entriesRef.set remaining
          fireEntries remaining

  -- Show function
  let showFn : NotificationLevel → String → IO Unit := fun level message => do
    env.withFrame do
      let currentTime ← currentTimeRef.get
      let id ← nextIdRef.modifyGet fun n => (n, n + 1)

      let entry : NotificationEntry := {
        id
        level
        message
        createdAt := currentTime
      }

      let entries ← entriesRef.get
      -- Add new entry, keeping only maxVisible
      let newEntries := if entries.size >= config.maxVisible then
        entries.extract 1 entries.size |>.push entry
      else
        entries.push entry

      entriesRef.set newEntries
      fireEntries newEntries

  -- Dismiss function (removes oldest)
  let dismissFn : IO Unit := do
    env.withFrame do
      let entries ← entriesRef.get
      if !entries.isEmpty then
        let remaining := entries.extract 1 entries.size
        entriesRef.set remaining
        fireEntries remaining

  -- Dismiss all function
  let dismissAllFn : IO Unit := do
    env.withFrame do
      entriesRef.set #[]
      fireEntries #[]

  -- Emit render function
  let node ← entriesDyn.map' (fun entries =>
    Id.run do
      if entries.isEmpty then
        return RNode.empty
      else
        -- Build notification nodes
        let mut nodes : Array RNode := #[]
        for entry in entries do
          let style := config.styleFor entry.level
          let symbol := entry.level.symbol

          -- Simple notification format: [symbol] message
          let text := s!"[{symbol}] {entry.message}"
          nodes := nodes.push (RNode.text text style)

        -- Stack vertically
        return RNode.column 0 {} nodes
  )
  emit node

  pure {
    «show» := showFn
    dismiss := dismissFn
    dismissAll := dismissAllFn
  }

/-! ## FRP-Friendly Notifications

The `notificationsWithEvents'` variant accepts Events instead of exposing imperative
show/dismiss methods. This allows for fully declarative FRP composition.
-/

/-- Create an event-driven notification widget.

    Unlike `notifications'` which returns imperative `show` and `dismiss` methods,
    this variant is driven by input Events. This makes it easy to compose
    notifications from multiple sources using FRP primitives.

    Example:
    ```
    -- Create show events from key presses
    let keyEvents ← useKeyEventW
    let notifyKeys ← Event.filterM (fun kd =>
      kd.event.code == .char 'n') keyEvents
    let showEvents ← Event.mapM (fun _ =>
      (NotificationLevel.success, "Action completed!")) notifyKeys

    -- Create dismiss events
    let dismissKeys ← Event.filterM (fun kd =>
      kd.event.code == .char 'd') keyEvents
    let dismissEvents ← Event.voidM dismissKeys

    -- Notifications driven by events
    notificationsWithEvents' showEvents dismissEvents dismissEvents { durationMs := 3000 }
    ```
-/
-- Actions for notification state machine
private inductive NotifAction where
  | add (entry : NotificationEntry)
  | dismissOne
  | dismissAll
  | tick (currentTime : Nat)

def notificationsWithEvents' (showEvents : Reactive.Event Spider (NotificationLevel × String))
    (dismissEvents : Reactive.Event Spider Unit)
    (dismissAllEvents : Reactive.Event Spider Unit)
    (config : NotificationConfig := {}) : WidgetM Unit := do
  -- Get tick events for auto-dismiss timing
  let tickEvent ← useTickW

  -- Track current time from ticks
  let currentTimeDyn ← Reactive.foldDyn (fun td _ => td.elapsedMs) 0 tickEvent

  -- Create entries from show events (attach current time)
  let newEntryEvents ← Event.attachWithM (fun currentTime pair =>
    let (level, message) := pair
    NotificationEntry.mk 0 level message currentTime
  ) currentTimeDyn.current showEvents

  -- Map each event type to NotifAction
  let addEvents ← Event.mapM NotifAction.add newEntryEvents
  let dismissOneEvents ← Event.mapM (fun _ => NotifAction.dismissOne) dismissEvents
  let dismissAllEvts ← Event.mapM (fun _ => NotifAction.dismissAll) dismissAllEvents
  let tickEventsAction ← Event.mapM (fun td => NotifAction.tick td.elapsedMs) tickEvent

  let allEvents ← Event.leftmostM [addEvents, dismissOneEvents, dismissAllEvts, tickEventsAction]

  -- Fold over all events to maintain entries state
  let entriesDyn ← Reactive.foldDyn (fun action entries =>
    match action with
    | .add entry =>
      if entries.size >= config.maxVisible then
        entries.extract 1 entries.size |>.push entry
      else
        entries.push entry
    | .dismissOne =>
      if entries.isEmpty then entries
      else entries.extract 1 entries.size
    | .dismissAll => #[]
    | .tick currentTime =>
      if config.durationMs > 0 then
        entries.filter fun entry =>
          currentTime - entry.createdAt < config.durationMs
      else
        entries
  ) (#[] : Array NotificationEntry) allEvents

  -- Emit render function
  let node ← entriesDyn.map' (fun entries =>
    Id.run do
      if entries.isEmpty then
        return RNode.empty
      else
        -- Build notification nodes
        let mut nodes : Array RNode := #[]
        for entry in entries do
          let style := config.styleFor entry.level
          let symbol := entry.level.symbol

          -- Simple notification format: [symbol] message
          let text := s!"[{symbol}] {entry.message}"
          nodes := nodes.push (RNode.text text style)

        -- Stack vertically
        return RNode.column 0 {} nodes
  )
  emit node

/-! ## Convenience Functions -/

/-- Create a notification area at a specific position. -/
def notificationArea' (position : NotificationPosition) (durationMs : Nat := 3000)
    : WidgetM NotificationResult :=
  notifications' { position, durationMs }

/-- Create persistent notifications (no auto-dismiss). -/
def persistentNotifications' (maxVisible : Nat := 5) : WidgetM NotificationResult :=
  notifications' { durationMs := 0, maxVisible }

/-- Create a quick toast notification helper.
    Returns a function that shows info-level notifications. -/
def toast' (durationMs : Nat := 2000) : WidgetM (String → IO Unit) := do
  let notif ← notifications' { durationMs, maxVisible := 1 }
  pure (notif.«show» .info)

end Terminus.Reactive
