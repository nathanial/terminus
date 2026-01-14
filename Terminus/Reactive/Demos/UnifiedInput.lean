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
        titledBlock' "Text Input" .rounded theme none do
          let input ← textInput' "demo-input" "" {
            placeholder := "Type here..."
            width := 20
            focusedStyle := { fg := .ansi .cyan, bg := .ansi .black }
          }
          row' (gap := 1) {} do
            text' "You typed:" theme.captionStyle
            dynText' input.value theme.primaryStyle

      -- Checkbox
      column' (gap := 1) {} do
        titledBlock' "Checkbox" .rounded theme none do
          let cb1 ← checkbox' "tos-check" "Terms of Service" false {
            checkedIcon := "[x] "
            uncheckedIcon := "[ ] "
          }

          row' (gap := 1) {} do
            text' "Agree:" theme.captionStyle
            let checkedStr ← Dynamic.map' cb1.checked fun c => if c then "Yes" else "No"
            dynText' checkedStr theme.primaryStyle

      -- Radio group
      column' (gap := 1) {} do
        titledBlock' "Radio Group" .rounded theme none do
          let radio ← radioGroup' "priority" #["Low", "Medium", "High"] (some 1) {
            selectedStyle := { fg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            let labelStr ← Dynamic.map' radio.selectedLabel (·.getD "(none)")
            dynText' labelStr theme.primaryStyle

    row' (gap := 2) {} do
      -- Selectable list
      column' (gap := 1) {} do
        titledBlock' "Selectable List" .rounded theme none do
          let fruits := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
          let list ← selectableList' fruits 1 {
            focusName := "fruit-list"
            maxVisible := some 4
            selectedStyle := { fg := .ansi .black, bg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Fruit:" theme.captionStyle
            let itemStr ← Dynamic.map' list.selectedItem (·.getD "(none)")
            dynText' itemStr theme.primaryStyle

      -- Text Area
      column' (gap := 1) {} do
        titledBlock' "Text Area" .rounded theme none do
          let editor ← textArea' "editor" "Hello, World!\nMulti-line text." {
            showLineNumbers := true
            maxVisibleLines := some 4
            minWidth := 25
          }

          row' (gap := 1) {} do
            text' "Cursor:" theme.captionStyle
            let cursorStr ← Dynamic.map' editor.cursorPos fun (line, col) => s!"Ln {line + 1}, Col {col + 1}"
            dynText' cursorStr theme.primaryStyle

    row' (gap := 2) {} do
      -- List variants
      column' (gap := 1) {} do
        titledBlock' "List Variants" .rounded theme none do
          text' "String list:" theme.captionStyle
          let colors := #["Red", "Green", "Blue", "Orange"]
          let colorList ← stringList' colors 0 {
            maxVisible := some 3
            focusName := "color-list"
            selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
          }

          row' (gap := 1) {} do
            text' "Picked:" theme.captionStyle
            let colorStr ← Dynamic.map' colorList.selectedItem (·.getD "(none)")
            dynText' colorStr theme.primaryStyle

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
            let numStr ← Dynamic.map' numbered.selectedItem (·.getD "(none)")
            dynText' numStr theme.primaryStyle

      -- Form
      column' (gap := 1) {} do
        titledBlock' "Form" .rounded theme none do
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

          let node ← form.isValid.zipWith' (fun ok status =>
            let validity := if ok then "valid" else "invalid"
            RNode.text s!"Status: {status} ({validity})" theme.captionStyle
          ) statusDyn
          emit node

    -- New Controls Row
    row' (gap := 2) {} do
      -- Slider
      column' (gap := 1) {} do
        titledBlock' "Slider" .rounded theme none do
          text' "Use ←/→ keys, Shift for 10x" theme.captionStyle
          let slider ← slider' "volume" 0.5 {
            width := 15
            showValue := true
          }
          row' (gap := 1) {} do
            text' "Value:" theme.captionStyle
            let pctStr ← Dynamic.map' slider.value fun v => s!"{(v * 100).toUInt32}%"
            dynText' pctStr theme.primaryStyle

      -- Switch
      column' (gap := 1) {} do
        titledBlock' "Switch" .rounded theme none do
          text' "Space/Enter to toggle" theme.captionStyle
          let sw ← switch' "darkMode" false {}
          row' (gap := 1) {} do
            text' "Dark Mode:" theme.captionStyle
            let modeStr ← Dynamic.map' sw.isOn fun on => if on then "Enabled" else "Disabled"
            dynText' modeStr theme.primaryStyle

      -- Stepper
      column' (gap := 1) {} do
        titledBlock' "Stepper" .rounded theme none do
          text' "Use ↑/↓ or +/- keys" theme.captionStyle
          let stepper ← stepper' "quantity" 5 {
            min := 0
            max := 99
            step := 1
          }
          row' (gap := 1) {} do
            text' "Qty:" theme.captionStyle
            let qtyStr ← Dynamic.map' stepper.value toString
            dynText' qtyStr theme.primaryStyle

    row' (gap := 2) {} do
      -- Password Input
      column' (gap := 1) {} do
        titledBlock' "Password Input" .rounded theme none do
          text' "Tab to toggle reveal" theme.captionStyle
          let pwd ← passwordInput' "password" {
            width := 15
            placeholder := "Password"
          }
          row' (gap := 1) {} do
            text' "Length:" theme.captionStyle
            let lenStr ← Dynamic.map' pwd.value fun v => toString v.length
            dynText' lenStr theme.primaryStyle

      -- Button
      column' (gap := 1) {} do
        titledBlock' "Button" .rounded theme none do
          text' "Space/Enter to click" theme.captionStyle
          let btn ← button' "action" "Click Me" {}
          let clickCount ← Reactive.foldDyn (fun _ n => n + 1) 0 btn.onClick
          row' (gap := 1) {} do
            text' "Clicks:" theme.captionStyle
            let countStr ← Dynamic.map' clickCount toString
            dynText' countStr theme.primaryStyle

      -- Autocomplete
      column' (gap := 1) {} do
        titledBlock' "Autocomplete" .rounded theme none do
          text' "Type to filter, ↑/↓ nav, Enter select" theme.captionStyle
          let commands := #["build", "test", "run", "clean", "help", "version", "init", "update"]
          let ac ← autocomplete' "command" commands {
            placeholder := "Type command..."
            width := 18
            maxSuggestions := 4
          }
          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            let selStr ← Dynamic.map' ac.selectedItem (·.getD "(none)")
            dynText' selStr theme.primaryStyle

    -- New Widgets Row: Dropdown, TagInput, RangeSlider
    row' (gap := 2) {} do
      -- Dropdown
      column' (gap := 1) {} do
        titledBlock' "Dropdown" .rounded theme none do
          text' "Space to open, arrows to nav" theme.captionStyle
          let options := #["Option A", "Option B", "Option C", "Option D", "Option E"]
          let dropdown ← selectDropdown' "demo-dropdown" options {
            placeholder := "Select..."
            width := 15
            maxVisible := 4
          }
          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            let selStr ← Dynamic.map' dropdown.selectedItem (·.getD "(none)")
            dynText' selStr theme.primaryStyle

      -- TagInput
      column' (gap := 1) {} do
        titledBlock' "TagInput" .rounded theme none do
          text' "Enter to add, Backspace to remove" theme.captionStyle
          let tags ← tagInput' "demo-tags" #["lean", "tui"] {
            placeholder := "Add tag..."
            width := 20
            maxTags := some 5
          }
          row' (gap := 1) {} do
            text' "Tags:" theme.captionStyle
            let tagStr ← Dynamic.map' tags.tags (fun arr => toString arr.size)
            dynText' tagStr theme.primaryStyle

      -- RangeSlider
      column' (gap := 1) {} do
        titledBlock' "RangeSlider" .rounded theme none do
          text' "Tab to switch handles, ←/→ to adjust" theme.captionStyle
          let range ← rangeSlider' "demo-range" 0.2 0.8 {
            width := 15
            showValues := true
          }
          row' (gap := 1) {} do
            text' "Range:" theme.captionStyle
            let rangeStr ← range.minValue.zipWith' (fun minV maxV =>
              s!"{(minV * 100).toUInt32}%-{(maxV * 100).toUInt32}%"
            ) range.maxValue
            dynText' rangeStr theme.primaryStyle
