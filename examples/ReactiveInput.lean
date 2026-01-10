/-
  Reactive Input Demo
  Demonstrates Phase 1 interactive components: TextInput, SelectableList, Modal.
-/
import Terminus.Reactive

open Terminus.Reactive
open Reactive Reactive.Host

def reactiveInputApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Track which demo section is active
  let (sectionEvent, _fireSection) ← newTriggerEvent (t := Spider) (a := Nat)
  let currentSection ← holdDyn 0 sectionEvent

  -- Track if confirm dialog is visible
  let (showConfirmEvent, fireShowConfirm) ← newTriggerEvent (t := Spider) (a := Bool)
  let showConfirm ← holdDyn false showConfirmEvent

  -- Track confirmation result message
  let (resultMsgEvent, fireResultMsg) ← newTriggerEvent (t := Spider) (a := String)
  let resultMsg ← holdDyn "" resultMsgEvent

  -- Subscribe to number keys to switch sections
  let keyEvents ← useKeyEvent
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    match kd.event.code with
    | .char '1' => fireShowConfirm false  -- Hide dialog when switching
    | .char '2' => fireShowConfirm false
    | .char '3' => fireShowConfirm false
    | .char 'c' | .char 'C' => fireShowConfirm true  -- Show confirm dialog
    | _ => pure ()

  -- Build the UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Input Demo ===" theme.heading1Style
      text' "Press 1/2/3 to focus sections, C for confirm dialog, Ctrl+C to quit" theme.captionStyle
      text' "" {}

      -- Section 1: Text Input Demo
      titledBlock' "1. Text Input" .rounded theme do
        text' "Type in the input field below:" theme.bodyStyle
        spacer' 1 1

        -- Create a text input
        let input ← textInput' "demo-input" "" {
          placeholder := "Enter text here..."
          width := 30
          focusedStyle := { fg := .ansi .cyan }
        }

        spacer' 1 1

        -- Show current value
        row' (gap := 1) {} do
          text' "Current value:" theme.captionStyle
          emitDynamic do
            let val ← input.value.sample
            pure (RNode.text (if val.isEmpty then "(empty)" else s!"\"{val}\"") theme.primaryStyle)

        -- Show submit events
        row' (gap := 1) {} do
          text' "Last submitted:" theme.captionStyle
          let submitted ← holdDyn "(none)" input.onSubmit
          emitDynamic do
            let val ← submitted.sample
            pure (RNode.text s!"\"{val}\"" theme.primaryStyle)

      -- Section 2: Selectable List Demo
      titledBlock' "2. Selectable List" .rounded theme do
        text' "Navigate with arrows/j/k, Enter to select:" theme.bodyStyle
        spacer' 1 1

        let fruits := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
        let list ← selectableList' fruits 0 {
          maxVisible := some 4
          selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
        }

        spacer' 1 1

        -- Show selection info
        row' (gap := 1) {} do
          text' "Selected index:" theme.captionStyle
          emitDynamic do
            let idx ← list.selectedIndex.sample
            pure (RNode.text (toString idx) theme.primaryStyle)

        row' (gap := 1) {} do
          text' "Selected item:" theme.captionStyle
          emitDynamic do
            let item ← list.selectedItem.sample
            let display := match item with
              | some s => s
              | none => "(none)"
            pure (RNode.text display theme.primaryStyle)

        -- Track last selected via Enter
        let lastSelected ← holdDyn "(none)" list.onSelect
        row' (gap := 1) {} do
          text' "Last confirmed:" theme.captionStyle
          emitDynamic do
            let sel ← lastSelected.sample
            pure (RNode.text sel theme.primaryStyle)

      -- Section 3: Numbered List Demo
      titledBlock' "3. Numbered List" .rounded theme do
        text' "Press 1-5 to quick-select:" theme.bodyStyle
        spacer' 1 1

        let colors := #["Red", "Green", "Blue", "Yellow", "Purple"]
        let numList ← numberedList' colors 0 {
          selectedStyle := { bg := .ansi .magenta, fg := .ansi .white }
        }

        spacer' 1 1

        row' (gap := 1) {} do
          text' "Selected:" theme.captionStyle
          emitDynamic do
            let item ← numList.selectedItem.sample
            let display := match item with
              | some s => s
              | none => "(none)"
            pure (RNode.text display theme.primaryStyle)

      -- Confirm Dialog (overlay)
      let confirm ← confirmDialog' "Do you want to proceed with this action?" showConfirm theme

      -- Handle confirm/cancel
      let _confirmUnsub ← SpiderM.liftIO <| confirm.confirmed.subscribe fun () => do
        fireResultMsg "Confirmed!"
        fireShowConfirm false

      let _cancelUnsub ← SpiderM.liftIO <| confirm.cancelled.subscribe fun () => do
        fireResultMsg "Cancelled."
        fireShowConfirm false

      -- Show result message
      emitDynamic do
        let msg ← resultMsg.sample
        if msg.isEmpty then
          pure RNode.empty
        else
          pure (RNode.text s!"Dialog result: {msg}" theme.primaryStyle)

      -- Status bar
      text' "" {}
      emitDynamic do
        let sectionIdx ← currentSection.sample
        pure (RNode.text s!"Section {sectionIdx + 1} focused" theme.captionStyle)

  pure { render }

def main : IO Unit :=
  runReactiveApp reactiveInputApp { debug := true, logPath := "reactive_input.log" }
