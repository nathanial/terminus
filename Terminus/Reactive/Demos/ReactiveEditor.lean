/-
  Reactive Editor Demo Widget
  Showcases TextArea and Form components.
-/
import Terminus.Reactive

open Terminus Terminus.Reactive
open Reactive Reactive.Host

/-- Demo application showing TextArea and Form widgets. -/
def reactiveEditorApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Define focusable widget names in order
  let focusableNames := #["editor", "name", "email", "priority", "newsletter"]
  let focusIndexRef ← SpiderM.liftIO (IO.mkRef 0)

  -- Get events for focus cycling
  let keyEvents ← useKeyEvent
  let events ← getEvents

  -- Tab key cycles focus between widgets
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    let ke := kd.event
    match ke.code with
    | .tab =>
      let idx ← focusIndexRef.get
      let nextIdx := if ke.modifiers.shift then
        if idx > 0 then idx - 1 else focusableNames.size - 1
      else
        (idx + 1) % focusableNames.size
      focusIndexRef.set nextIdx
      if h : nextIdx < focusableNames.size then
        events.registry.fireFocus (some focusableNames[nextIdx])
    | _ => pure ()

  -- Set initial focus to the editor
  SpiderM.liftIO <| events.registry.fireFocus (some "editor")

  -- Build UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Editor Demo ===" theme.heading1Style
      text' "Tab: switch panels | Ctrl+Enter: submit | Esc: cancel | Ctrl+C: quit" theme.captionStyle

      row' (gap := 2) {} do
        -- Left panel: TextArea demo
        column' (gap := 1) {} do
          titledBlock' "Text Editor" .rounded theme do
            let editor ← textArea' "editor" "Hello, World!\n\nThis is a multi-line\ntext editor demo.\n\nTry editing this text!" {
              showLineNumbers := true
              maxVisibleLines := some 8
              minWidth := 35
            }

            -- Show cursor position
            row' (gap := 1) {} do
              text' "Cursor:" theme.captionStyle
              emitDynamic do
                let (line, col) ← editor.cursorPos.sample
                pure (RNode.text s!"Ln {line + 1}, Col {col + 1}" theme.primaryStyle)

          -- Character count
          titledBlock' "Stats" .single theme do
            emitDynamic do
              -- We'd need to track content, but for demo just show static
              pure (RNode.text "Lines: 6 | Chars: ~100" theme.captionStyle)

        -- Right panel: Form demo
        column' (gap := 1) {} do
          titledBlock' "Contact Form" .rounded theme do
            -- Name field
            row' (gap := 1) {} do
              text' "Name:" theme.bodyStyle
            let nameInput ← textInput' "name" "" {
              placeholder := "Enter your name"
              width := 25
            }

            spacer' 1 1

            -- Email field
            row' (gap := 1) {} do
              text' "Email:" theme.bodyStyle
            let emailInput ← textInput' "email" "" {
              placeholder := "you@example.com"
              width := 25
            }

            spacer' 1 1

            -- Priority selector
            row' (gap := 1) {} do
              text' "Priority:" theme.bodyStyle
            let priority ← optionSelector' "priority" #["Low", "Medium", "High"] 1 {
              horizontal := true
            }

            spacer' 1 1

            -- Newsletter checkbox
            let newsletter ← checkbox' "newsletter" "Subscribe to newsletter" false {}

            spacer' 1 1

            -- Show form state
            row' (gap := 2) {} do
              text' "[Submit]" theme.primaryStyle
              text' "[Cancel]" theme.captionStyle

            spacer' 1 1

            -- Form status (inline with form to access the bindings)
            titledBlock' "Form Status" .single theme do
              column' (gap := 0) {} do
                row' (gap := 1) {} do
                  text' "Name:" theme.captionStyle
                  emitDynamic do
                    let name ← nameInput.value.sample
                    let display := if name.isEmpty then "(empty)" else name
                    pure (RNode.text display theme.bodyStyle)

                row' (gap := 1) {} do
                  text' "Email:" theme.captionStyle
                  emitDynamic do
                    let email ← emailInput.value.sample
                    let display := if email.isEmpty then "(empty)" else email
                    pure (RNode.text display theme.bodyStyle)

                row' (gap := 1) {} do
                  text' "Priority:" theme.captionStyle
                  emitDynamic do
                    let prio ← priority.selectedValue.sample
                    pure (RNode.text prio theme.bodyStyle)

                row' (gap := 1) {} do
                  text' "Newsletter:" theme.captionStyle
                  emitDynamic do
                    let checked ← newsletter.checked.sample
                    let display := if checked == true then "Yes" else "No"
                    pure (RNode.text display theme.bodyStyle)

      -- Footer showing current focus
      let focusedInput ← useFocusedInputW
      emitDynamic do
        let focused ← focusedInput.sample
        let focusName := focused.getD "(none)"
        pure (RNode.text s!"Focused: {focusName}" theme.captionStyle)

  pure { render }
