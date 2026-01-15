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
  /-- Tag that was just added (for deriving events). -/
  lastAddedTag : Option String := none
  /-- Tag that was just removed (for deriving events). -/
  lastRemovedTag : Option String := none
  deriving Repr, Inhabited, BEq

namespace TagInputState

/-- Insert a character at cursor position. -/
def insertChar (s : TagInputState) (c : Char) : TagInputState :=
  let before := s.input.take s.cursor
  let after := s.input.drop s.cursor
  { s with
    input := before ++ c.toString ++ after
    cursor := s.cursor + 1
    lastAddedTag := none
    lastRemovedTag := none }

/-- Delete character before cursor (backspace). -/
def backspace (s : TagInputState) : TagInputState :=
  if s.cursor == 0 then { s with lastAddedTag := none, lastRemovedTag := none }
  else
    let before := s.input.take (s.cursor - 1)
    let after := s.input.drop s.cursor
    { s with
      input := before ++ after
      cursor := s.cursor - 1
      lastAddedTag := none
      lastRemovedTag := none }

/-- Delete character at cursor (delete key). -/
def delete (s : TagInputState) : TagInputState :=
  if s.cursor >= s.input.length then { s with lastAddedTag := none, lastRemovedTag := none }
  else
    let before := s.input.take s.cursor
    let after := s.input.drop (s.cursor + 1)
    { s with
      input := before ++ after
      lastAddedTag := none
      lastRemovedTag := none }

/-- Move cursor left. -/
def moveLeft (s : TagInputState) : TagInputState :=
  if s.cursor == 0 then { s with lastAddedTag := none, lastRemovedTag := none }
  else { s with cursor := s.cursor - 1, lastAddedTag := none, lastRemovedTag := none }

/-- Move cursor right. -/
def moveRight (s : TagInputState) : TagInputState :=
  if s.cursor >= s.input.length then { s with lastAddedTag := none, lastRemovedTag := none }
  else { s with cursor := s.cursor + 1, lastAddedTag := none, lastRemovedTag := none }

/-- Move cursor to start. -/
def moveHome (s : TagInputState) : TagInputState :=
  { s with cursor := 0, lastAddedTag := none, lastRemovedTag := none }

/-- Move cursor to end. -/
def moveEnd (s : TagInputState) : TagInputState :=
  { s with cursor := s.input.length, lastAddedTag := none, lastRemovedTag := none }

/-- Add current input as a tag (if non-empty) and clear input. -/
def addTag (s : TagInputState) (maxTags : Option Nat) : TagInputState :=
  let trimmed := s.input.trim
  if trimmed.isEmpty then
    { s with lastAddedTag := none, lastRemovedTag := none }
  else if s.tags.contains trimmed then
    -- Don't add duplicate tags
    { s with input := "", cursor := 0, lastAddedTag := none, lastRemovedTag := none }
  else
    match maxTags with
    | some max =>
      if s.tags.size >= max then
        { s with lastAddedTag := none, lastRemovedTag := none }
      else
        { s with
          tags := s.tags.push trimmed
          input := ""
          cursor := 0
          lastAddedTag := some trimmed
          lastRemovedTag := none }
    | none =>
      { s with
        tags := s.tags.push trimmed
        input := ""
        cursor := 0
        lastAddedTag := some trimmed
        lastRemovedTag := none }

/-- Remove the last tag (for backspace on empty input). -/
def removeLastTag (s : TagInputState) : TagInputState :=
  if s.tags.isEmpty then
    { s with lastAddedTag := none, lastRemovedTag := none }
  else
    let removed := s.tags.back!
    { s with
      tags := s.tags.pop
      lastAddedTag := none
      lastRemovedTag := some removed }

end TagInputState

/-! ## State Operation Type -/

/-- A state operation transforms TagInputState. -/
private abbrev StateOp := TagInputState → TagInputState

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

  -- Initial state
  let initialState : TagInputState := {
    tags := initialTags
    input := ""
    cursor := 0
    lastAddedTag := none
    lastRemovedTag := none
  }

  -- Get focus state (for rendering)
  let focusedInput ← useFocusedInputW

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    let ke := kd.event
    match ke.code with
    | .char c =>
      -- Only handle printable characters
      if c.val >= 32 then
        some fun (state : TagInputState) => state.insertChar c
      else
        none
    | .backspace =>
      some fun (state : TagInputState) =>
        if state.input.isEmpty then
          state.removeLastTag
        else
          state.backspace
    | .delete =>
      some fun (state : TagInputState) => state.delete
    | .left =>
      some fun (state : TagInputState) => state.moveLeft
    | .right =>
      some fun (state : TagInputState) => state.moveRight
    | .home =>
      some fun (state : TagInputState) => state.moveHome
    | .end =>
      some fun (state : TagInputState) => state.moveEnd
    | .enter =>
      some fun (state : TagInputState) => state.addTag config.maxTags
    | _ => none) keyEvents

  -- Fold state operations into state dynamic
  let stateDyn ← foldDyn (fun op state => op state) initialState stateOps

  -- Derive dynamics for tags and input
  let tagsDyn ← stateDyn.map' (·.tags)
  let inputDyn ← stateDyn.map' (·.input)

  -- Derive tag added/removed events from state updates
  let tagAddedEvent ← Event.mapMaybeM (fun (state : TagInputState) => state.lastAddedTag) stateDyn.updated

  let tagRemovedEvent ← Event.mapMaybeM (fun (state : TagInputState) => state.lastRemovedTag) stateDyn.updated

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
