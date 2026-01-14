/-
  Terminus Reactive - TagInput Widget
  Input field for adding and removing tags.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## TagInput Configuration -/

/-- Configuration for tag input appearance and behavior. -/
structure TagInputConfig where
  /-- Placeholder text when input is empty. -/
  placeholder : String := "Add tag..."
  /-- Minimum display width in characters. -/
  width : Nat := 30
  /-- Maximum number of tags (none = unlimited). -/
  maxTags : Option Nat := none
  /-- Style for tag labels. -/
  tagStyle : Style := { fg := .ansi .cyan }
  /-- Style for input text. -/
  inputStyle : Style := {}
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .white }
  /-- Style for placeholder text. -/
  placeholderStyle : Style := { fg := .ansi .brightBlack }
  /-- Separator between tags. -/
  separator : String := " "
  /-- Cursor character. -/
  cursorChar : Char := '|'
  /-- Style for cursor. -/
  cursorStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  deriving Inhabited

/-! ## TagInput Result -/

/-- Result returned by tagInput' containing reactive values and events. -/
structure TagInputResult where
  /-- Current tags as a Dynamic. -/
  tags : Reactive.Dynamic Spider (Array String)
  /-- Current input text as a Dynamic. -/
  currentInput : Reactive.Dynamic Spider String
  /-- Event fired when a tag is added. -/
  onTagAdded : Reactive.Event Spider String
  /-- Event fired when a tag is removed. -/
  onTagRemoved : Reactive.Event Spider String

/-! ## TagInput Internal State -/

/-- Internal state for tag input. -/
private structure TagInputState where
  /-- Current tags. -/
  tags : Array String := #[]
  /-- Current input text. -/
  input : String := ""
  /-- Cursor position within input. -/
  cursor : Nat := 0
  deriving Repr, Inhabited

namespace TagInputState

/-- Insert a character at cursor position. -/
def insertChar (s : TagInputState) (c : Char) : TagInputState :=
  let before := s.input.take s.cursor
  let after := s.input.drop s.cursor
  { s with input := before ++ c.toString ++ after, cursor := s.cursor + 1 }

/-- Delete character before cursor (backspace). -/
def backspace (s : TagInputState) : TagInputState :=
  if s.cursor == 0 then s
  else
    let before := s.input.take (s.cursor - 1)
    let after := s.input.drop s.cursor
    { s with input := before ++ after, cursor := s.cursor - 1 }

/-- Delete character at cursor (delete key). -/
def delete (s : TagInputState) : TagInputState :=
  if s.cursor >= s.input.length then s
  else
    let before := s.input.take s.cursor
    let after := s.input.drop (s.cursor + 1)
    { s with input := before ++ after }

/-- Move cursor left. -/
def moveLeft (s : TagInputState) : TagInputState :=
  if s.cursor == 0 then s
  else { s with cursor := s.cursor - 1 }

/-- Move cursor right. -/
def moveRight (s : TagInputState) : TagInputState :=
  if s.cursor >= s.input.length then s
  else { s with cursor := s.cursor + 1 }

/-- Move cursor to start. -/
def moveHome (s : TagInputState) : TagInputState :=
  { s with cursor := 0 }

/-- Move cursor to end. -/
def moveEnd (s : TagInputState) : TagInputState :=
  { s with cursor := s.input.length }

/-- Add current input as a tag (if non-empty) and clear input. -/
def addTag (s : TagInputState) (maxTags : Option Nat) : TagInputState × Option String :=
  let trimmed := s.input.trim
  if trimmed.isEmpty then
    (s, none)
  else if s.tags.contains trimmed then
    -- Don't add duplicate tags
    ({ s with input := "", cursor := 0 }, none)
  else
    match maxTags with
    | some max =>
      if s.tags.size >= max then (s, none)
      else ({ s with tags := s.tags.push trimmed, input := "", cursor := 0 }, some trimmed)
    | none =>
      ({ s with tags := s.tags.push trimmed, input := "", cursor := 0 }, some trimmed)

/-- Remove the last tag (for backspace on empty input). -/
def removeLastTag (s : TagInputState) : TagInputState × Option String :=
  if s.tags.isEmpty then
    (s, none)
  else
    let removed := s.tags.back!
    ({ s with tags := s.tags.pop }, some removed)

end TagInputState

/-! ## TagInput Widget -/

/-- Create a tag input widget.

    The widget handles:
    - Character input for typing tag names
    - Enter to add the current input as a tag
    - Backspace on empty input to remove the last tag
    - Arrow keys for cursor movement within input

    Visual format: `[tag1] [tag2] |input...|`

    Example:
    ```
    let tagInput ← tagInput' "tags" {}
    -- Use tagInput.tags to get current tags
    -- Use tagInput.onTagAdded to handle new tags
    ```
-/
def tagInput' (name : String) (initialTags : Array String := #[])
    (config : TagInputConfig := {}) : WidgetM TagInputResult := do
  -- Register as focusable input
  let widgetName ← registerComponentW "tagInput" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Create trigger events
  let (tagAddedEvent, fireTagAdded) ← newTriggerEvent (t := Spider) (a := String)
  let (tagRemovedEvent, fireTagRemoved) ← newTriggerEvent (t := Spider) (a := String)
  let (tagsEvent, fireTags) ← newTriggerEvent (t := Spider) (a := Array String)
  let (inputEvent, fireInput) ← newTriggerEvent (t := Spider) (a := String)
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := TagInputState)

  -- Track internal state
  let initialState : TagInputState := { tags := initialTags, input := "", cursor := 0 }
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
  let stateDyn ← holdDyn initialState stateEvent

  -- Create dynamics for tags and input
  let tagsDyn ← holdDyn initialTags tagsEvent
  let inputDyn ← holdDyn "" inputEvent

  -- Get focus state (for rendering)
  let focusedInput ← useFocusedInputW

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    let state ← stateRef.get
    let ke := kd.event

    -- Handle key
    let (newState, tagAdded, tagRemoved) ← match ke.code with
      | .char c =>
        -- Only handle printable characters
        if c.val >= 32 then
          pure (state.insertChar c, none, none)
        else
          pure (state, none, none)
      | .backspace =>
        if state.input.isEmpty then
          -- Remove last tag when backspacing on empty input
          let (newState, removed) := state.removeLastTag
          pure (newState, none, removed)
        else
          pure (state.backspace, none, none)
      | .delete => pure (state.delete, none, none)
      | .left => pure (state.moveLeft, none, none)
      | .right => pure (state.moveRight, none, none)
      | .home => pure (state.moveHome, none, none)
      | .end => pure (state.moveEnd, none, none)
      | .enter =>
        let (newState, added) := state.addTag config.maxTags
        pure (newState, added, none)
      | _ => pure (state, none, none)

    -- Update state and fire events
    if newState.tags != state.tags || newState.input != state.input || newState.cursor != state.cursor then
      stateRef.set newState
      fireState newState

      if newState.tags != state.tags then
        fireTags newState.tags

      if newState.input != state.input then
        fireInput newState.input

      match tagAdded with
      | some tag => fireTagAdded tag
      | none => pure ()

      match tagRemoved with
      | some tag => fireTagRemoved tag
      | none => pure ()

  -- Emit render function
  let node ← focusedInput.zipWith' (fun currentFocus state =>
    Id.run do
      let isFocused := currentFocus == some inputName

      -- Build tag nodes
      let mut nodes : Array RNode := #[]

      for tag in state.tags do
        nodes := nodes.push (RNode.text s!"[{tag}]" config.tagStyle)
        if config.separator.length > 0 then
          nodes := nodes.push (RNode.text config.separator {})

      -- Build input section
      let inputStyle := if isFocused then config.focusedStyle else config.inputStyle
      let showPlaceholder := state.input.isEmpty && !isFocused

      if isFocused && !state.input.isEmpty then
        -- Show input with cursor
        let before := state.input.take state.cursor
        let after := state.input.drop state.cursor

        if !before.isEmpty then
          nodes := nodes.push (RNode.text before inputStyle)
        nodes := nodes.push (RNode.text (String.singleton config.cursorChar) config.cursorStyle)
        if !after.isEmpty then
          nodes := nodes.push (RNode.text after inputStyle)
      else if isFocused then
        -- Show just cursor when input is empty
        nodes := nodes.push (RNode.text (String.singleton config.cursorChar) config.cursorStyle)
      else if showPlaceholder then
        -- Show placeholder
        nodes := nodes.push (RNode.text config.placeholder config.placeholderStyle)
      else if !state.input.isEmpty then
        -- Show input text (unfocused)
        nodes := nodes.push (RNode.text state.input inputStyle)

      -- Calculate padding to reach minimum width
      let tagsWidth := state.tags.foldl (fun acc tag => acc + tag.length + 2 + config.separator.length) 0
      let inputWidth := if isFocused then
          state.input.length + 1  -- +1 for cursor
        else if showPlaceholder then
          config.placeholder.length
        else
          state.input.length
      let currentWidth := tagsWidth + inputWidth
      let padding := if currentWidth < config.width then config.width - currentWidth else 0

      if padding > 0 then
        nodes := nodes.push (RNode.text (String.ofList (List.replicate padding ' ')) {})

      RNode.row 0 {} nodes
  ) stateDyn
  emit node

  pure {
    tags := tagsDyn
    currentInput := inputDyn
    onTagAdded := tagAddedEvent
    onTagRemoved := tagRemovedEvent
  }

/-- Create a labeled tag input with the label above. -/
def labeledTagInput' (label : String) (name : String) (initialTags : Array String := #[])
    (config : TagInputConfig := {}) (theme : Theme := .dark) : WidgetM TagInputResult := do
  emitStatic (RNode.text label theme.bodyStyle)
  tagInput' name initialTags config

/-- Create a tag input that auto-focuses on creation. -/
def focusedTagInput' (name : String) (initialTags : Array String := #[])
    (config : TagInputConfig := {}) : WidgetM TagInputResult := do
  let events ← getEventsW
  let inputName := if name.isEmpty then "tagInput-auto" else name
  SpiderM.liftIO <| events.registry.fireFocus (some inputName)
  tagInput' name initialTags config

end Terminus.Reactive
