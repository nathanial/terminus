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

def feedbackDialogsSection (theme : Theme) (unfocusedKeys : Reactive.Event Spider KeyData) : WidgetM Unit := do
  -- Dialog visibility triggers
  let (modalVisEvent, fireModalVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
  let modalVisible ← Reactive.holdDyn false modalVisEvent
  let (confirmVisEvent, fireConfirmVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
  let confirmVisible ← Reactive.holdDyn false confirmVisEvent
  let (messageVisEvent, fireMessageVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
  let messageVisible ← Reactive.holdDyn false messageVisEvent
  let (errorVisEvent, fireErrorVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
  let errorVisible ← Reactive.holdDyn false errorVisEvent
  let (warningVisEvent, fireWarningVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
  let warningVisible ← Reactive.holdDyn false warningVisEvent
  let (inputVisEvent, fireInputVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
  let inputVisible ← Reactive.holdDyn false inputVisEvent

  column' (gap := 1) {} do
    titledBlock' "Dialogs" .rounded theme none do
      text' "O: modal | F: confirm | M: message" theme.captionStyle
      text' "I: input | X: error | V: warning | Esc: close modal" theme.captionStyle

      let _ ← modalWhen' modalVisible "Modal" theme {} do
        text' "This is a modal dialog." theme.bodyStyle
        text' "Press Esc to close." theme.captionStyle

      let confirmResult ← confirmDialog' "Proceed with action?" confirmVisible theme
      let messageDismiss ← messageDialog' "Operation completed successfully!" messageVisible theme
      let errorDismiss ← errorDialog' "Something went wrong." errorVisible theme
      let warningDismiss ← warningDialog' "Please review the warning." warningVisible theme
      let inputResult ← inputDialog' "Enter your name:" inputVisible theme "Your name"

      -- FRP: Compose dialog status from all events
      let confirmedStatus ← Event.mapM (fun _ => "Confirmed") confirmResult.confirmed
      let cancelledStatus ← Event.mapM (fun _ => "Cancelled") confirmResult.cancelled
      let confirmStatusEvent ← Event.leftmostM [confirmedStatus, cancelledStatus]
      let messageStatusEvent ← Event.mapM (fun _ => "Message dismissed") messageDismiss
      let errorStatusEvent ← Event.mapM (fun _ => "Error dismissed") errorDismiss
      let warningStatusEvent ← Event.mapM (fun _ => "Warning dismissed") warningDismiss
      let inputSubmitStatusEvent ← Event.mapM (fun _ => "Input submitted") inputResult.submitted
      let inputCancelStatusEvent ← Event.mapM (fun _ => "Input cancelled") inputResult.cancelled

      -- Key open events for status
      let modalOpenKeys ← Event.filterM (fun (kd : KeyData) =>
        kd.event.code == .char 'o' || kd.event.code == .char 'O') unfocusedKeys
      let confirmOpenKeys ← Event.filterM (fun (kd : KeyData) =>
        kd.event.code == .char 'f' || kd.event.code == .char 'F') unfocusedKeys
      let messageOpenKeys ← Event.filterM (fun (kd : KeyData) =>
        kd.event.code == .char 'm' || kd.event.code == .char 'M') unfocusedKeys
      let inputOpenKeys ← Event.filterM (fun (kd : KeyData) =>
        kd.event.code == .char 'i' || kd.event.code == .char 'I') unfocusedKeys
      let errorOpenKeys ← Event.filterM (fun (kd : KeyData) =>
        kd.event.code == .char 'x' || kd.event.code == .char 'X') unfocusedKeys
      let warningOpenKeys ← Event.filterM (fun (kd : KeyData) =>
        kd.event.code == .char 'v' || kd.event.code == .char 'V') unfocusedKeys
      let escapeKeys ← Event.filterM (fun (kd : KeyData) => kd.event.code == .escape) unfocusedKeys

      let modalOpenStatus ← Event.mapM (fun _ => "Modal opened") modalOpenKeys
      let confirmOpenStatus ← Event.mapM (fun _ => "Confirm opened") confirmOpenKeys
      let messageOpenStatus ← Event.mapM (fun _ => "Message opened") messageOpenKeys
      let inputOpenStatus ← Event.mapM (fun _ => "Input opened") inputOpenKeys
      let errorOpenStatus ← Event.mapM (fun _ => "Error opened") errorOpenKeys
      let warningOpenStatus ← Event.mapM (fun _ => "Warning opened") warningOpenKeys

      -- Escape closes modal only when it's visible
      let escapeWhenModal ← Event.attachWithM (fun vis _ => vis) modalVisible.current escapeKeys
      let modalCloseEvents ← Event.filterM (fun vis => vis) escapeWhenModal
      let modalCloseStatus ← Event.mapM (fun _ => "Modal closed") modalCloseEvents

      let allStatusEvents ← Event.leftmostM [
        confirmStatusEvent, messageStatusEvent, errorStatusEvent,
        warningStatusEvent, inputSubmitStatusEvent, inputCancelStatusEvent,
        modalOpenStatus, confirmOpenStatus, messageOpenStatus,
        inputOpenStatus, errorOpenStatus, warningOpenStatus, modalCloseStatus
      ]
      let dialogStatusDyn ← Reactive.holdDyn "Idle" allStatusEvents

      -- FRP: Input value from dialog submissions
      let inputValueDyn ← Reactive.holdDyn "(none)" inputResult.submitted

      -- Wire key events to visibility triggers using performEvent_ (FRP-friendly)
      let modalOpenAction ← Event.mapM (fun _ => fireModalVisible true) modalOpenKeys
      let confirmOpenAction ← Event.mapM (fun _ => fireConfirmVisible true) confirmOpenKeys
      let messageOpenAction ← Event.mapM (fun _ => fireMessageVisible true) messageOpenKeys
      let inputOpenAction ← Event.mapM (fun _ => fireInputVisible true) inputOpenKeys
      let errorOpenAction ← Event.mapM (fun _ => fireErrorVisible true) errorOpenKeys
      let warningOpenAction ← Event.mapM (fun _ => fireWarningVisible true) warningOpenKeys
      let modalCloseAction ← Event.mapM (fun _ => fireModalVisible false) modalCloseEvents

      performEvent_ modalOpenAction
      performEvent_ confirmOpenAction
      performEvent_ messageOpenAction
      performEvent_ inputOpenAction
      performEvent_ errorOpenAction
      performEvent_ warningOpenAction
      performEvent_ modalCloseAction

      -- Wire dialog result events to close visibility using performEvent_
      let confirmCloseAction1 ← Event.mapM (fun _ => fireConfirmVisible false) confirmResult.confirmed
      let confirmCloseAction2 ← Event.mapM (fun _ => fireConfirmVisible false) confirmResult.cancelled
      let messageCloseAction ← Event.mapM (fun _ => fireMessageVisible false) messageDismiss
      let errorCloseAction ← Event.mapM (fun _ => fireErrorVisible false) errorDismiss
      let warningCloseAction ← Event.mapM (fun _ => fireWarningVisible false) warningDismiss
      let inputCloseAction1 ← Event.mapM (fun _ => fireInputVisible false) inputResult.submitted
      let inputCloseAction2 ← Event.mapM (fun _ => fireInputVisible false) inputResult.cancelled

      performEvent_ confirmCloseAction1
      performEvent_ confirmCloseAction2
      performEvent_ messageCloseAction
      performEvent_ errorCloseAction
      performEvent_ warningCloseAction
      performEvent_ inputCloseAction1
      performEvent_ inputCloseAction2

      spacer' 0 1

      emitDynamic do
        let status ← dialogStatusDyn.sample
        let inputValue ← inputValueDyn.sample
        pure (RNode.text s!"Last: {status} | Input: {inputValue}" theme.captionStyle)

def feedbackPopupSection (theme : Theme) (unfocusedKeys : Reactive.Event Spider KeyData) : WidgetM Unit := do
  -- Popup visibility as FRP toggle
  let popupToggleKeys ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .char 'p' || kd.event.code == .char 'P') unfocusedKeys
  let popupToggleVoid ← Event.voidM popupToggleKeys
  let popupVisible ← Reactive.foldDyn (fun _ v => !v) false popupToggleVoid

  -- Popup (FRP: visibility-driven)
  column' (gap := 1) {} do
    titledBlock' "Popup" .rounded theme none do
      text' "P: toggle popup" theme.captionStyle
      popupWhen' "demo-popup" popupVisible { title := some "Popup" } do
        text' "This is a popup panel." theme.bodyStyle
        text' "Press P to toggle visibility." theme.captionStyle
      emitDynamic do
        let visible ← popupVisible.sample
        let label := if visible then "Visible" else "Hidden"
        pure (RNode.text s!"Status: {label}" theme.captionStyle)

def feedbackContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "L: log info | E: error | W: warn | N: notify | D: dismiss | C: clear" theme.captionStyle
    text' "P: popup | O: modal | F: confirm | M: message | I: input | X: error | V: warning" theme.captionStyle

    let keyEvents ← useKeyEventW
    let focusedInput ← useFocusedInputW

    -- Filter out keys when input dialog is focused
    let notDialogFocused ← Dynamic.map' focusedInput (· != some "input-dialog-field")
    let unfocusedKeys ← Event.gateM notDialogFocused.current keyEvents

    -- Sections
    feedbackLoggingSection theme unfocusedKeys
    spacer' 0 1
    row' (gap := 2) {} do
      feedbackDialogsSection theme unfocusedKeys
      feedbackPopupSection theme unfocusedKeys
