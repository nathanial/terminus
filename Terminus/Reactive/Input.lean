/-
  Terminus Reactive - Input Components
  Text input widgets with focus and cursor management.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## TextInput Configuration -/

/-- Configuration for text input appearance and behavior. -/
structure TextInputConfig where
  /-- Placeholder text shown when empty. -/
  placeholder : String := ""
  /-- Maximum input length (none = unlimited). -/
  maxLength : Option Nat := none
  /-- Minimum display width in characters. -/
  width : Nat := 20
  /-- Style when unfocused. -/
  style : Style := {}
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan }
  /-- Cursor character. -/
  cursorChar : Char := '|'
  /-- Style for cursor. -/
  cursorStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Style for placeholder text. -/
  placeholderStyle : Style := { fg := .ansi .brightBlack }
  deriving Repr, Inhabited

/-! ## TextInput Result -/

/-- Result returned by textInput' containing reactive values and events. -/
structure TextInputResult where
  /-- Current text value as a Dynamic. -/
  value : Reactive.Dynamic Spider String
  /-- Event fired when Enter is pressed (contains current value). -/
  onSubmit : Reactive.Event Spider String
  /-- Event fired when Escape is pressed. -/
  onCancel : Reactive.Event Spider Unit
  /-- Event fired whenever the text changes. -/
  onChange : Reactive.Event Spider String


/-! ## TextInput State -/

/-- Internal state for text input. -/
structure TextInputState where
  /-- Current text content. -/
  text : String := ""
  /-- Cursor position (index into text). -/
  cursor : Nat := 0
  deriving Repr, Inhabited

namespace TextInputState

/-- Insert a character at cursor position. -/
def insertChar (s : TextInputState) (c : Char) (maxLen : Option Nat) : TextInputState :=
  match maxLen with
  | some max =>
    if s.text.length >= max then s
    else
      let before := s.text.take s.cursor
      let after := s.text.drop s.cursor
      { text := before ++ c.toString ++ after, cursor := s.cursor + 1 }
  | none =>
    let before := s.text.take s.cursor
    let after := s.text.drop s.cursor
    { text := before ++ c.toString ++ after, cursor := s.cursor + 1 }

/-- Delete character before cursor (backspace). -/
def backspace (s : TextInputState) : TextInputState :=
  if s.cursor == 0 then s
  else
    let before := s.text.take (s.cursor - 1)
    let after := s.text.drop s.cursor
    { text := before ++ after, cursor := s.cursor - 1 }

/-- Delete character at cursor (delete key). -/
def delete (s : TextInputState) : TextInputState :=
  if s.cursor >= s.text.length then s
  else
    let before := s.text.take s.cursor
    let after := s.text.drop (s.cursor + 1)
    { text := before ++ after, cursor := s.cursor }

/-- Move cursor left. -/
def moveLeft (s : TextInputState) : TextInputState :=
  if s.cursor == 0 then s
  else { s with cursor := s.cursor - 1 }

/-- Move cursor right. -/
def moveRight (s : TextInputState) : TextInputState :=
  if s.cursor >= s.text.length then s
  else { s with cursor := s.cursor + 1 }

/-- Move cursor to start. -/
def moveHome (s : TextInputState) : TextInputState :=
  { s with cursor := 0 }

/-- Move cursor to end. -/
def moveEnd (s : TextInputState) : TextInputState :=
  { s with cursor := s.text.length }

/-- Clear all text. -/
def clear (s : TextInputState) : TextInputState :=
  { text := "", cursor := 0 }

end TextInputState

/-! ## TextInput Widget -/

/-- Create a text input widget.

    The widget handles:
    - Character input (printable characters)
    - Cursor movement (left, right, home, end)
    - Deletion (backspace, delete)
    - Submit (Enter) and cancel (Escape) events

    Example:
    ```
    let input ← textInput' "search" "" { placeholder := "Search..." }
    -- Use input.value to get current text
    -- Use input.onSubmit to handle Enter key
    ```
-/
def textInput' (name : String) (initial : String := "")
    (config : TextInputConfig := {}) : WidgetM TextInputResult := do
  let events ← getEventsW

  -- Register as focusable input
  let widgetName ← registerComponentW "textInput" (isInput := true)

  -- Create trigger events for submit/cancel/change
  let (submitEvent, fireSubmit) ← newTriggerEvent (t := Spider) (a := String)
  let (cancelEvent, fireCancel) ← newTriggerEvent (t := Spider) (a := Unit)
  let (changeEvent, fireChange) ← newTriggerEvent (t := Spider) (a := String)

  -- Track internal state
  let initialState : TextInputState := { text := initial, cursor := initial.length }
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)

  -- Create dynamic for the text value
  let (valueEvent, fireValue) ← newTriggerEvent (t := Spider) (a := String)
  let valueDyn ← holdDyn initial valueEvent

  -- Get focus state
  let focusedInput ← useFocusedInputW

  -- Subscribe to key events when focused
  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    -- Check if this input is focused (use explicit name or auto-generated)
    let currentFocus ← focusedInput.sample
    let inputName := if name.isEmpty then widgetName else name
    let isFocused := currentFocus == some inputName

    if isFocused then
      let state ← stateRef.get
      let ke := kd.event

      -- Handle key
      let newState ← match ke.code with
        | .char c =>
          -- Only handle printable characters
          if c.val >= 32 then
            pure (state.insertChar c config.maxLength)
          else
            pure state
        | .backspace => pure state.backspace
        | .delete => pure state.delete
        | .left => pure state.moveLeft
        | .right => pure state.moveRight
        | .home => pure state.moveHome
        | .end => pure state.moveEnd
        | .enter =>
          fireSubmit state.text
          pure state
        | .escape =>
          fireCancel ()
          pure state
        | _ => pure state

      -- Update state and fire events if text changed
      if newState.text != state.text then
        stateRef.set newState
        fireValue newState.text
        fireChange newState.text
      else if newState.cursor != state.cursor then
        stateRef.set newState

  -- Emit render function
  emit do
    let state ← stateRef.get
    let currentFocus ← focusedInput.sample
    let inputName := if name.isEmpty then widgetName else name
    let isFocused := currentFocus == some inputName

    -- Build display string
    let displayText := if state.text.isEmpty && !isFocused then
      config.placeholder
    else
      state.text

    -- Determine style based on focus
    let textStyle := if state.text.isEmpty && !isFocused then
      config.placeholderStyle
    else if isFocused then
      config.focusedStyle
    else
      config.style

    -- Build the display with cursor if focused
    if isFocused && !state.text.isEmpty then
      -- Split text at cursor and insert cursor character
      let before := state.text.take state.cursor
      let after := state.text.drop state.cursor
      let cursorStr := config.cursorChar.toString

      -- Create row with before, cursor, after
      let beforeNode := if before.isEmpty then RNode.empty else RNode.text before textStyle
      let cursorNode := RNode.text cursorStr config.cursorStyle
      let afterNode := if after.isEmpty then RNode.empty else RNode.text after textStyle

      -- Pad to minimum width
      let currentLen := state.text.length + 1  -- +1 for cursor
      let padding := if currentLen < config.width then config.width - currentLen else 0
      let padNode := if padding > 0 then RNode.text (String.ofList (List.replicate padding ' ')) textStyle else RNode.empty

      -- Filter out empty nodes
      let mut nodes : Array RNode := #[]
      if !before.isEmpty then nodes := nodes.push beforeNode
      nodes := nodes.push cursorNode
      if !after.isEmpty then nodes := nodes.push afterNode
      if padding > 0 then nodes := nodes.push padNode
      pure (RNode.row 0 {} nodes)
    else if isFocused then
      -- Empty text with cursor
      let cursorNode := RNode.text config.cursorChar.toString config.cursorStyle
      let padding := if config.width > 1 then config.width - 1 else 0
      let padNode := if padding > 0 then RNode.text (String.ofList (List.replicate padding ' ')) textStyle else RNode.empty
      pure (RNode.row 0 {} #[cursorNode, padNode])
    else
      -- Not focused - just show text (or placeholder)
      let padding := if displayText.length < config.width then config.width - displayText.length else 0
      let padNode := if padding > 0 then RNode.text (String.ofList (List.replicate padding ' ')) textStyle else RNode.empty
      if displayText.isEmpty then
        pure padNode
      else
        pure (RNode.row 0 {} #[RNode.text displayText textStyle, padNode])

  pure {
    value := valueDyn
    onSubmit := submitEvent
    onCancel := cancelEvent
    onChange := changeEvent
  }

/-- Create a labeled text input with the label above. -/
def labeledTextInput' (label : String) (name : String) (initial : String := "")
    (config : TextInputConfig := {}) (theme : Theme := .dark) : WidgetM TextInputResult := do
  emit (pure (RNode.text label theme.bodyStyle))
  textInput' name initial config

/-- Create a text input that auto-focuses on creation. -/
def focusedTextInput' (name : String) (initial : String := "")
    (config : TextInputConfig := {}) : WidgetM TextInputResult := do
  let events ← getEventsW
  let inputName := if name.isEmpty then "textInput-auto" else name
  SpiderM.liftIO <| events.registry.fireFocus (some inputName)
  textInput' name initial config

end Terminus.Reactive
