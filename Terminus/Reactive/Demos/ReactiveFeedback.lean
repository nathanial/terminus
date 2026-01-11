/-
  Reactive Feedback Demo Widgets
  Demonstrates Spinner, Logger, and Notification widgets.
-/
import Terminus.Reactive

open Terminus.Reactive
open Reactive Reactive.Host

def reactiveFeedbackApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Track log count for demo purposes
  let logCountRef ← SpiderM.liftIO (IO.mkRef 0)

  -- Get key events
  let keyEvents ← useKeyEvent
  let events ← getEvents

  -- Create shared state for logger and notifications
  let loggerResultRef ← SpiderM.liftIO (IO.mkRef (none : Option LoggerResult))
  let notifResultRef ← SpiderM.liftIO (IO.mkRef (none : Option NotificationResult))

  -- Build the UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Feedback Demo ===" theme.heading1Style
      text' "L: log info | E: log error | W: warn | N: notify | D: dismiss | C: clear" theme.captionStyle

      -- Main content row
      row' (gap := 2) {} do
        -- Left column: Spinners
        column' (gap := 1) {} do
          titledBlock' "1. Animated Spinners" .rounded theme do
            -- Dots spinner
            row' (gap := 1) {} do
              text' "Dots:" theme.captionStyle
              let _ ← animatedSpinner' (some "Processing...") 100 { style := .dots }
              pure ()

            -- ASCII spinner
            row' (gap := 1) {} do
              text' "ASCII:" theme.captionStyle
              let _ ← animatedSpinner' (some "Loading...") 120 { style := .ascii }
              pure ()

            -- Blocks spinner
            row' (gap := 1) {} do
              text' "Blocks:" theme.captionStyle
              let _ ← animatedSpinner' (some "Working...") 150 { style := .blocks }
              pure ()

            -- Arc spinner
            row' (gap := 1) {} do
              text' "Arc:" theme.captionStyle
              let _ ← animatedSpinner' (some "Syncing...") 80 { style := .arc }
              pure ()

            -- Line spinner
            row' (gap := 1) {} do
              text' "Line:" theme.captionStyle
              let _ ← animatedSpinner' (some "Rotating...") 100 { style := .line }
              pure ()

        -- Middle column: Logger
        column' (gap := 1) {} do
          titledBlock' "2. Logger Widget" .rounded theme do
            text' "Press L/E/W to log messages:" theme.captionStyle
            let logResult ← logger' {
              maxLines := 8
              showLevel := true
              showTimestamp := false
            }
            SpiderM.liftIO (loggerResultRef.set (some logResult))

        -- Right column: Notifications
        column' (gap := 1) {} do
          titledBlock' "3. Notifications" .rounded theme do
            text' "Press N for notifications:" theme.captionStyle
            text' "Press D to dismiss one" theme.captionStyle
            let notifResult ← notifications' {
              durationMs := 0  -- Persistent for demo
              maxVisible := 5
            }
            SpiderM.liftIO (notifResultRef.set (some notifResult))

  -- Subscribe to key events for logging/notifications
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    match kd.event.code with
    | .char 'l' | .char 'L' =>
      let count ← logCountRef.get
      logCountRef.set (count + 1)
      match (← loggerResultRef.get) with
      | some logger => logger.log .info s!"Info message #{count + 1}"
      | none => pure ()
    | .char 'e' | .char 'E' =>
      let count ← logCountRef.get
      logCountRef.set (count + 1)
      match (← loggerResultRef.get) with
      | some logger => logger.log .error s!"Error message #{count + 1}"
      | none => pure ()
    | .char 'w' | .char 'W' =>
      let count ← logCountRef.get
      logCountRef.set (count + 1)
      match (← loggerResultRef.get) with
      | some logger => logger.log .warn s!"Warning message #{count + 1}"
      | none => pure ()
    | .char 'n' | .char 'N' =>
      let count ← logCountRef.get
      logCountRef.set (count + 1)
      match (← notifResultRef.get) with
      | some notif => notif.«show» .success s!"Notification #{count}!"
      | none => pure ()
    | .char 'd' | .char 'D' =>
      match (← notifResultRef.get) with
      | some notif => notif.dismiss
      | none => pure ()
    | .char 'c' | .char 'C' =>
      -- Clear both logger and notifications
      match (← loggerResultRef.get) with
      | some logger => logger.clear
      | none => pure ()
      match (← notifResultRef.get) with
      | some notif => notif.dismissAll
      | none => pure ()
    | _ => pure ()

  pure { render }
