import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Feedback Tab Content -/

def feedbackLoggingSection (theme : Theme) (unfocusedKeys : Reactive.Event Spider KeyData) : WidgetM Unit := do
  -- FRP: Key events for logging
  let infoLogKeys ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .char 'l' || kd.event.code == .char 'L') unfocusedKeys
  let errorLogKeys ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .char 'e' || kd.event.code == .char 'E') unfocusedKeys
  let warnLogKeys ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .char 'w' || kd.event.code == .char 'W') unfocusedKeys
  let clearKeys ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .char 'c' || kd.event.code == .char 'C') unfocusedKeys

  -- FRP: Key events for notifications
  let notifyKeys ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .char 'n' || kd.event.code == .char 'N') unfocusedKeys
  let dismissKeys ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .char 'd' || kd.event.code == .char 'D') unfocusedKeys

  -- FRP: Count log entries for numbering
  let allLogKeys ← Event.leftmostM [infoLogKeys, errorLogKeys, warnLogKeys, notifyKeys]
  let allLogVoid ← Event.voidM allLogKeys
  let logCountDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 allLogVoid

  -- FRP: Create log entry events with counts
  let infoEntries ← Event.attachWithM (fun count _ =>
    LogEntry.info s!"Info message #{count + 1}"
  ) logCountDyn.current infoLogKeys
  let errorEntries ← Event.attachWithM (fun count _ =>
    LogEntry.error s!"Error message #{count + 1}"
  ) logCountDyn.current errorLogKeys
  let warnEntries ← Event.attachWithM (fun count _ =>
    LogEntry.warn s!"Warning #{count + 1}"
  ) logCountDyn.current warnLogKeys
  let allLogEntries ← Event.leftmostM [infoEntries, errorEntries, warnEntries]
  let clearEvents ← Event.voidM clearKeys

  -- FRP: Create notification events
  let notifyEntries ← Event.attachWithM (fun count _ =>
    (NotificationLevel.success, s!"Notification #{count + 1}!")
  ) logCountDyn.current notifyKeys
  let dismissOneEvents ← Event.voidM dismissKeys
  let dismissAllEvents ← Event.voidM clearKeys

  row' (gap := 2) {} do
    -- Spinners
    column' (gap := 1) {} do
      titledBlock' "Spinners" .rounded theme none do
        row' (gap := 1) {} do
          text' "Dots:" theme.captionStyle
          let _ ← animatedSpinner' (some "Processing") 100 { style := .dots }

        row' (gap := 1) {} do
          text' "ASCII:" theme.captionStyle
          let _ ← animatedSpinner' (some "Loading") 120 { style := .ascii }

        row' (gap := 1) {} do
          text' "Blocks:" theme.captionStyle
          let _ ← animatedSpinner' none 150 { style := .blocks }

        row' (gap := 1) {} do
          text' "Arc:" theme.captionStyle
          let _ ← animatedSpinner' none 80 { style := .arc }

    -- Logger (FRP: event-driven)
    column' (gap := 1) {} do
      titledBlock' "Logger" .rounded theme none do
        let _ ← loggerWithEvents' allLogEntries clearEvents {
          maxLines := 6
          showLevel := true
          showTimestamp := false
        }

    -- Notifications (FRP: event-driven)
    column' (gap := 1) {} do
      titledBlock' "Notifications" .rounded theme none do
        notificationsWithEvents' notifyEntries dismissOneEvents dismissAllEvents {
          durationMs := 0
          maxVisible := 4
        }

def feedbackContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "L: log info | E: error | W: warn | N: notify | D: dismiss | C: clear" theme.captionStyle

    let keyEvents ← useKeyEventW
    let focusedInput ← useFocusedInputW

    -- Filter out keys when input dialog is focused
    let notDialogFocused ← Dynamic.map' focusedInput (· != some "input-dialog-field")
    let unfocusedKeys ← Event.gateM notDialogFocused.current keyEvents

    -- Sections
    feedbackLoggingSection theme unfocusedKeys

    row' (gap := 2) {} do
      -- Toast demo
      column' (gap := 1) {} do
        titledBlock' "Toast Notifications" .rounded theme none do
          text' "T: show toast | I: info | S: success | X: error" theme.captionStyle

          -- Create toast manager
          let toastMgr ← toastContainer' { duration := 2000, maxToasts := 3 }

          -- Wire key events to show toasts
          let toastInfoKeys ← Event.filterM (fun (kd : KeyData) =>
            kd.event.code == .char 't' || kd.event.code == .char 'T') unfocusedKeys
          let toastSuccessKeys ← Event.filterM (fun (kd : KeyData) =>
            kd.event.code == .char 's' || kd.event.code == .char 'S') unfocusedKeys
          let toastErrorKeys ← Event.filterM (fun (kd : KeyData) =>
            kd.event.code == .char 'x' || kd.event.code == .char 'X') unfocusedKeys

          let toastCount ← Reactive.foldDyn (fun _ n => n + 1) 0 (← Event.voidM (← Event.leftmostM [toastInfoKeys, toastSuccessKeys, toastErrorKeys]))

          performEvent_ (← Event.mapM (fun _ => do
            let count ← toastCount.sample
            toastMgr.show s!"Info toast #{count}" .info
          ) toastInfoKeys)

          performEvent_ (← Event.mapM (fun _ => do
            let count ← toastCount.sample
            toastMgr.show s!"Success #{count}!" .success
          ) toastSuccessKeys)

          performEvent_ (← Event.mapM (fun _ => do
            let count ← toastCount.sample
            toastMgr.show s!"Error #{count}!" .error
          ) toastErrorKeys)

      -- Badge demo
      column' (gap := 1) {} do
        titledBlock' "Badges" .rounded theme none do
          text' "Status indicators and counts" theme.captionStyle
          column' (gap := 1) {} do
            row' (gap := 2) {} do
              text' "Count:" theme.captionStyle
              badge' (.count 5) {}
              badge' (.count 150) {}  -- Shows 99+

            row' (gap := 2) {} do
              text' "Labels:" theme.captionStyle
              badge' (.label "NEW") { style := { fg := .ansi .white, bg := .ansi .green } }
              badge' (.label "BETA") { style := { fg := .ansi .black, bg := .ansi .yellow } }

            row' (gap := 2) {} do
              text' "Status:" theme.captionStyle
              statusDot' (.ansi .green)
              text' "Online" theme.bodyStyle
              statusDot' (.ansi .red)
              text' "Offline" theme.bodyStyle

            row' (gap := 2) {} do
              text' "With content:" theme.captionStyle
              withBadge' (.count 3) {} do
                text' "Messages" theme.bodyStyle
