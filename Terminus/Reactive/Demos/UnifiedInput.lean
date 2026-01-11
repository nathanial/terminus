import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Input Tab Content -/

def inputContent (theme : Theme) (_events : TerminusEvents) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Text input, checkboxes, radio buttons, and lists." theme.captionStyle

    row' (gap := 2) {} do
      -- Text input
      column' (gap := 1) {} do
        titledBlock' "Text Input" .rounded theme do
          let input ← textInput' "demo-input" "" {
            placeholder := "Type here..."
            width := 20
            focusedStyle := { fg := .ansi .cyan, bg := .ansi .black }
          }
          row' (gap := 1) {} do
            text' "You typed:" theme.captionStyle
            emitDynamic do
              let val ← input.value.sample
              if val.isEmpty then
                pure (RNode.text "(empty)" { fg := .ansi .brightBlack })
              else
                pure (RNode.text val theme.primaryStyle)

      -- Checkbox
      column' (gap := 1) {} do
        titledBlock' "Checkbox" .rounded theme do
          let cb1 ← checkbox' "tos-check" "Terms of Service" false {
            checkedIcon := "[x] "
            uncheckedIcon := "[ ] "
          }

          row' (gap := 1) {} do
            text' "Agree:" theme.captionStyle
            emitDynamic do
              let checked ← cb1.checked.sample
              pure (RNode.text (if checked then "Yes" else "No") theme.primaryStyle)

      -- Radio group
      column' (gap := 1) {} do
        titledBlock' "Radio Group" .rounded theme do
          let radio ← radioGroup' "priority" #["Low", "Medium", "High"] (some 1) {
            selectedStyle := { fg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let label ← radio.selectedLabel.sample
              pure (RNode.text (label.getD "(none)") theme.primaryStyle)

    row' (gap := 2) {} do
      -- Selectable list
      column' (gap := 1) {} do
        titledBlock' "Selectable List" .rounded theme do
          let fruits := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
          let list ← selectableList' fruits 1 {
            focusName := "fruit-list"
            maxVisible := some 4
            selectedStyle := { fg := .ansi .black, bg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Fruit:" theme.captionStyle
            emitDynamic do
              let item ← list.selectedItem.sample
              pure (RNode.text (item.getD "(none)") theme.primaryStyle)

      -- Text Area
      column' (gap := 1) {} do
        titledBlock' "Text Area" .rounded theme do
          let editor ← textArea' "editor" "Hello, World!\nMulti-line text." {
            showLineNumbers := true
            maxVisibleLines := some 4
            minWidth := 25
          }

          row' (gap := 1) {} do
            text' "Cursor:" theme.captionStyle
            emitDynamic do
              let (line, col) ← editor.cursorPos.sample
              pure (RNode.text s!"Ln {line + 1}, Col {col + 1}" theme.primaryStyle)

    row' (gap := 2) {} do
      -- List variants
      column' (gap := 1) {} do
        titledBlock' "List Variants" .rounded theme do
          text' "String list:" theme.captionStyle
          let colors := #["Red", "Green", "Blue", "Orange"]
          let colorList ← stringList' colors 0 {
            maxVisible := some 3
            focusName := "color-list"
            selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
          }

          row' (gap := 1) {} do
            text' "Picked:" theme.captionStyle
            emitDynamic do
              let item ← colorList.selectedItem.sample
              pure (RNode.text (item.getD "(none)") theme.primaryStyle)

          spacer' 0 1

          text' "Numbered list (1-9):" theme.captionStyle
          let steps := #["One", "Two", "Three", "Four"]
          let numbered ← numberedList' steps 0 {
            maxVisible := some 4
            focusName := "numbered-list"
            selectedStyle := { fg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Chosen:" theme.captionStyle
            emitDynamic do
              let item ← numbered.selectedItem.sample
              pure (RNode.text (item.getD "(none)") theme.primaryStyle)

      -- Form
      column' (gap := 1) {} do
        titledBlock' "Form" .rounded theme do
          text' "Ctrl+Enter: submit | Esc: cancel" theme.captionStyle

          let fields := #[
            { label := "Name", name := "name", required := true, placeholder := "Ada" },
            { label := "Email", name := "email", required := true, placeholder := "ada@example.com",
              validate := fun v => if v.length < 3 then some "Too short" else none }
          ]

          let form ← form' fields { fieldGap := 1, inputWidth := 18 } theme

          -- FRP: Compose status from form events
          let submitStatus ← Event.mapM (fun _ => "Submitted") form.onSubmit
          let cancelStatus ← Event.mapM (fun _ => "Cancelled") form.onCancel
          let statusEvent ← Event.leftmostM [submitStatus, cancelStatus]
          let statusDyn ← Reactive.holdDyn "Idle" statusEvent

          spacer' 0 1

          emitDynamic do
            let ok ← form.isValid.sample
            let status ← statusDyn.sample
            let validity := if ok then "valid" else "invalid"
            pure (RNode.text s!"Status: {status} ({validity})" theme.captionStyle)
