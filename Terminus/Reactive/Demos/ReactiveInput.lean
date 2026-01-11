/-
  Reactive Input Demo Widgets
  Shared widget tree for the reactive input demo (used by tests and the executable).
-/
import Terminus.Reactive

open Terminus.Reactive
open Reactive Reactive.Host

def reactiveInputApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Track if confirm dialog is visible
  let (showConfirmEvent, fireShowConfirm) ← newTriggerEvent (t := Spider) (a := Bool)
  let showConfirm ← holdDyn false showConfirmEvent

  -- Track confirmation result message
  let (resultMsgEvent, fireResultMsg) ← newTriggerEvent (t := Spider) (a := String)
  let resultMsg ← holdDyn "" resultMsgEvent

  -- Define focusable component names in order
  let focusableNames := #["demo-input", "fruit-list", "color-list", "scroll-demo"]
  let focusIndexRef ← SpiderM.liftIO (IO.mkRef 0)

  -- Subscribe to Tab key to cycle focus
  let keyEvents ← useKeyEvent
  let events ← getEvents
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    match kd.event.code with
    | .char 'c' | .char 'C' => fireShowConfirm true  -- Show confirm dialog
    | .tab =>
      -- Cycle focus to next component
      let idx ← focusIndexRef.get
      let nextIdx := (idx + 1) % focusableNames.size
      focusIndexRef.set nextIdx
      if h : nextIdx < focusableNames.size then
        events.registry.fireFocus (some focusableNames[nextIdx])
    | _ => pure ()

  -- Set initial focus to the fruit list
  SpiderM.liftIO <| events.registry.fireFocus (some "fruit-list")

  -- Build the UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Input Demo ===" theme.heading1Style
      text' "Tab: cycle focus | C: confirm dialog | Ctrl+C: quit" theme.captionStyle

      -- Row 1: Text Input (left) | Selectable List (right)
      row' (gap := 2) {} do
        -- Section 1: Text Input Demo
        titledBlock' "1. Text Input" .rounded theme do
          text' "Type in the field below:" theme.bodyStyle

          let input ← textInput' "demo-input" "" {
            placeholder := "Enter text..."
            width := 25
            focusedStyle := { fg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Value:" theme.captionStyle
            emitDynamic do
              let val ← input.value.sample
              pure (RNode.text (if val.isEmpty then "(empty)" else s!"\"{val}\"") theme.primaryStyle)

          let submitted ← holdDyn "(none)" input.onSubmit
          row' (gap := 1) {} do
            text' "Submitted:" theme.captionStyle
            emitDynamic do
              let val ← submitted.sample
              pure (RNode.text s!"\"{val}\"" theme.primaryStyle)

        -- Section 2: Selectable List Demo
        titledBlock' "2. Selectable List" .rounded theme do
          text' "Arrows/j/k, Enter to select:" theme.bodyStyle

          let fruits := #["Apple", "Banana", "Cherry", "Date", "Elderberry",
                          "Fig", "Grape", "Honeydew", "Kiwi", "Lemon"]
          let list ← selectableList' fruits 0 {
            maxVisible := some 4
            selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
            focusName := "fruit-list"
          }

          row' (gap := 1) {} do
            text' "Index:" theme.captionStyle
            emitDynamic do
              let idx ← list.selectedIndex.sample
              pure (RNode.text (toString idx) theme.primaryStyle)

          row' (gap := 1) {} do
            text' "Item:" theme.captionStyle
            emitDynamic do
              let item ← list.selectedItem.sample
              let display := match item with
                | some s => s
                | none => "(none)"
              pure (RNode.text display theme.primaryStyle)

          let lastSelected ← holdDyn "(none)" list.onSelect
          row' (gap := 1) {} do
            text' "Confirmed:" theme.captionStyle
            emitDynamic do
              let sel ← lastSelected.sample
              pure (RNode.text sel theme.primaryStyle)

      -- Row 2: Numbered List (left) | ScrollView (right)
      row' (gap := 2) {} do
        -- Section 3: Numbered List Demo
        titledBlock' "3. Numbered List" .rounded theme do
          text' "Press 1-5 to quick-select:" theme.bodyStyle

          let colors := #["Red", "Green", "Blue", "Yellow", "Purple"]
          let numList ← numberedList' colors 0 {
            selectedStyle := { bg := .ansi .magenta, fg := .ansi .white }
            focusName := "color-list"
          }

          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let item ← numList.selectedItem.sample
              let display := match item with
                | some s => s
                | none => "(none)"
              pure (RNode.text display theme.primaryStyle)

        -- Section 4: ScrollView Demo
        titledBlock' "4. ScrollView" .rounded theme do
          text' "Arrows/j/k to scroll:" theme.bodyStyle

          let _scroll ← scrollView' { maxVisible := 3, showVerticalScrollbar := true, focusName := "scroll-demo" } do
            text' "Line 1: The quick brown fox" theme.bodyStyle
            text' "Line 2: jumps over the lazy dog." theme.bodyStyle
            text' "Line 3: Pack my box with" theme.bodyStyle
            text' "Line 4: five dozen liquor jugs." theme.bodyStyle
            text' "Line 5: How vexingly quick" theme.bodyStyle
            text' "Line 6: daft zebras jump!" theme.bodyStyle
            pure ()

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

      -- Status bar showing current focus
      let focusedInput ← useFocusedInputW
      emitDynamic do
        let focused ← focusedInput.sample
        let focusName := focused.getD "(none)"
        pure (RNode.text s!"Focused: {focusName}" theme.captionStyle)

  pure { render }
