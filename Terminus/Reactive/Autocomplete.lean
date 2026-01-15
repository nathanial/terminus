/-
  Terminus Reactive - Autocomplete Widget
  Text input with filtered dropdown suggestions for search and selection.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Autocomplete Configuration -/

/-- Configuration for autocomplete appearance and behavior. -/
structure AutocompleteConfig where
  /-- Placeholder text shown when empty. -/
  placeholder : String := ""
  /-- Minimum display width in characters. -/
  width : Nat := 20
  /-- Maximum number of visible suggestions. -/
  maxSuggestions : Nat := 5
  /-- Minimum characters before showing suggestions. -/
  minChars : Nat := 1
  /-- Whether matching is case-sensitive. -/
  caseSensitive : Bool := false
  /-- Style when unfocused. -/
  style : Style := {}
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan }
  /-- Style for suggestion items. -/
  suggestionStyle : Style := {}
  /-- Style for highlighted suggestion. -/
  highlightStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Cursor character. -/
  cursorChar : Char := '|'
  /-- Style for cursor. -/
  cursorStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Style for placeholder text. -/
  placeholderStyle : Style := { fg := .ansi .brightBlack }
  /-- Prefix for highlighted suggestion. -/
  highlightPrefix : String := ">"
  /-- Prefix for non-highlighted suggestions. -/
  normalPrefix : String := " "
  /-- Border character for suggestion box. -/
  borderChar : Char := '│'
  /-- Style for suggestion box border. -/
  borderStyle : Style := { fg := .ansi .brightBlack }
  deriving Repr, Inhabited

/-! ## Autocomplete Result -/

/-- Result returned by autocomplete' containing reactive values and events. -/
structure AutocompleteResult where
  /-- Current input text value as a Dynamic. -/
  value : Reactive.Dynamic Spider String
  /-- Currently highlighted suggestion (none if no suggestions visible). -/
  selectedItem : Reactive.Dynamic Spider (Option String)
  /-- Event fired when a suggestion is selected (via Enter or Tab). -/
  onSelect : Reactive.Event Spider String
  /-- Event fired when Enter is pressed (with current text or selected suggestion). -/
  onSubmit : Reactive.Event Spider String

/-! ## Autocomplete State -/

/-- Internal state for autocomplete input. -/
structure AutocompleteState where
  /-- Current text content. -/
  text : String := ""
  /-- Cursor position (index into text). -/
  cursor : Nat := 0
  /-- Highlighted suggestion index (0-based). -/
  highlightIndex : Nat := 0
  /-- Whether the suggestion dropdown is visible. -/
  showSuggestions : Bool := false
  deriving Repr, Inhabited, BEq

namespace AutocompleteState

/-- Insert a character at cursor position. -/
def insertChar (s : AutocompleteState) (c : Char) : AutocompleteState :=
  let before := s.text.take s.cursor
  let after := s.text.drop s.cursor
  { s with
    text := before ++ c.toString ++ after
    cursor := s.cursor + 1
    highlightIndex := 0
    showSuggestions := true }

/-- Delete character before cursor (backspace). -/
def backspace (s : AutocompleteState) : AutocompleteState :=
  if s.cursor == 0 then s
  else
    let before := s.text.take (s.cursor - 1)
    let after := s.text.drop s.cursor
    { s with
      text := before ++ after
      cursor := s.cursor - 1
      highlightIndex := 0
      showSuggestions := true }

/-- Delete character at cursor (delete key). -/
def delete (s : AutocompleteState) : AutocompleteState :=
  if s.cursor >= s.text.length then s
  else
    let before := s.text.take s.cursor
    let after := s.text.drop (s.cursor + 1)
    { s with
      text := before ++ after
      highlightIndex := 0
      showSuggestions := true }

/-- Move cursor left. -/
def moveLeft (s : AutocompleteState) : AutocompleteState :=
  if s.cursor == 0 then s
  else { s with cursor := s.cursor - 1 }

/-- Move cursor right. -/
def moveRight (s : AutocompleteState) : AutocompleteState :=
  if s.cursor >= s.text.length then s
  else { s with cursor := s.cursor + 1 }

/-- Move cursor to start. -/
def moveHome (s : AutocompleteState) : AutocompleteState :=
  { s with cursor := 0 }

/-- Move cursor to end. -/
def moveEnd (s : AutocompleteState) : AutocompleteState :=
  { s with cursor := s.text.length }

/-- Move highlight up in suggestions. -/
def highlightUp (s : AutocompleteState) (suggestionCount : Nat) : AutocompleteState :=
  if suggestionCount == 0 then s
  else if s.highlightIndex == 0 then
    { s with highlightIndex := suggestionCount - 1 }
  else
    { s with highlightIndex := s.highlightIndex - 1 }

/-- Move highlight down in suggestions. -/
def highlightDown (s : AutocompleteState) (suggestionCount : Nat) : AutocompleteState :=
  if suggestionCount == 0 then s
  else if s.highlightIndex >= suggestionCount - 1 then
    { s with highlightIndex := 0 }
  else
    { s with highlightIndex := s.highlightIndex + 1 }

/-- Hide the suggestion dropdown. -/
def hideSuggestions (s : AutocompleteState) : AutocompleteState :=
  { s with showSuggestions := false }

/-- Set the text (used when selecting a suggestion). -/
def setText (s : AutocompleteState) (text : String) : AutocompleteState :=
  { s with
    text := text
    cursor := text.length
    highlightIndex := 0
    showSuggestions := false }

/-- Clear all text. -/
def clear (_ : AutocompleteState) : AutocompleteState :=
  { text := "", cursor := 0, highlightIndex := 0, showSuggestions := false }

end AutocompleteState

/-! ## Suggestion Filtering -/

/-- Check if a suggestion matches the input text (prefix match). -/
private def matchesSuggestion (input : String) (suggestion : String)
    (caseSensitive : Bool) : Bool :=
  if input.isEmpty then true
  else
    let inputLower := if caseSensitive then input else input.toLower
    let suggestionLower := if caseSensitive then suggestion else suggestion.toLower
    suggestionLower.startsWith inputLower

/-- Filter suggestions based on input text. -/
private def filterSuggestions (input : String) (suggestions : Array String)
    (caseSensitive : Bool) (maxCount : Nat) : Array String :=
  let filtered := suggestions.filter (matchesSuggestion input · caseSensitive)
  filtered.toList.take maxCount |>.toArray

/-! ## Render Helpers -/

/-- Build the input text line node. -/
private def buildInputNode (state : AutocompleteState) (isFocused : Bool)
    (config : AutocompleteConfig) : RNode := Id.run do
  let displayText := if state.text.isEmpty && !isFocused then
    config.placeholder
  else
    state.text

  let textStyle := if state.text.isEmpty && !isFocused then
    config.placeholderStyle
  else if isFocused then
    config.focusedStyle
  else
    config.style

  if isFocused && !state.text.isEmpty then
    let before := state.text.take state.cursor
    let after := state.text.drop state.cursor
    let cursorStr := config.cursorChar.toString

    let beforeNode := if before.isEmpty then RNode.empty else RNode.text before textStyle
    let cursorNode := RNode.text cursorStr config.cursorStyle
    let afterNode := if after.isEmpty then RNode.empty else RNode.text after textStyle

    let currentLen := state.text.length + 1
    let padding := if currentLen < config.width then config.width - currentLen else 0
    let padNode := if padding > 0 then
      RNode.text (String.ofList (List.replicate padding ' ')) textStyle
    else
      RNode.empty

    let mut nodes : Array RNode := #[]
    if !before.isEmpty then nodes := nodes.push beforeNode
    nodes := nodes.push cursorNode
    if !after.isEmpty then nodes := nodes.push afterNode
    if padding > 0 then nodes := nodes.push padNode
    return RNode.row 0 {} nodes
  else if isFocused then
    let cursorNode := RNode.text config.cursorChar.toString config.cursorStyle
    let padding := if config.width > 1 then config.width - 1 else 0
    let padNode := if padding > 0 then
      RNode.text (String.ofList (List.replicate padding ' ')) textStyle
    else
      RNode.empty
    return RNode.row 0 {} #[cursorNode, padNode]
  else
    let padding := if displayText.length < config.width then
      config.width - displayText.length
    else
      0
    let padNode := if padding > 0 then
      RNode.text (String.ofList (List.replicate padding ' ')) textStyle
    else
      RNode.empty
    if displayText.isEmpty then
      return padNode
    else
      return RNode.row 0 {} #[RNode.text displayText textStyle, padNode]

/-- Build a single suggestion row node. -/
private def buildSuggestionNode (suggestion : String) (isHighlighted : Bool)
    (config : AutocompleteConfig) : RNode :=
  let itemPrefix := if isHighlighted then config.highlightPrefix else config.normalPrefix
  let itemStyle := if isHighlighted then config.highlightStyle else config.suggestionStyle
  let borderNode := RNode.text config.borderChar.toString config.borderStyle
  let prefixNode := RNode.text itemPrefix itemStyle
  let textNode := RNode.text suggestion itemStyle

  -- Pad to width
  let contentLen := itemPrefix.length + suggestion.length + 1
  let itemPadding := if contentLen < config.width then config.width - contentLen else 0
  let itemPadNode := if itemPadding > 0 then
    RNode.text (String.ofList (List.replicate itemPadding ' ')) itemStyle
  else
    RNode.empty

  RNode.row 0 {} #[borderNode, prefixNode, textNode, itemPadNode]

/-- Build the suggestions dropdown nodes. -/
private def buildSuggestionsNodes (suggestions : Array String) (highlightIndex : Nat)
    (config : AutocompleteConfig) : Array RNode := Id.run do
  let mut rows : Array RNode := #[]
  for i in [:suggestions.size] do
    if h : i < suggestions.size then
      let suggestion := suggestions[i]
      let isHighlighted := i == highlightIndex
      rows := rows.push (buildSuggestionNode suggestion isHighlighted config)
  return rows

/-! ## Autocomplete Widget -/

/-- Create an autocomplete widget with static suggestions.

    The widget handles:
    - Character input (printable characters)
    - Cursor movement (left, right, home, end)
    - Deletion (backspace, delete)
    - Up/Down arrows to navigate suggestions
    - Enter to select highlighted suggestion and submit
    - Tab to autocomplete with highlighted suggestion
    - Escape to close suggestions dropdown

    This implementation uses FRP-idiomatic state management via foldDyn,
    avoiding imperative subscribe patterns with IO.mkRef.

    Example:
    ```
    let suggestions := #["apple", "apricot", "banana", "blueberry"]
    let auto ← autocomplete' "search" suggestions { placeholder := "Search fruits..." }
    -- Use auto.value to get current text
    -- Use auto.onSelect to handle selection
    -- Use auto.onSubmit to handle Enter key
    ```
-/
def autocomplete' (name : String) (suggestions : Array String)
    (config : AutocompleteConfig := {}) : WidgetM AutocompleteResult := do
  -- Register as focusable input
  let widgetName ← registerComponentW "autocomplete" (isInput := true) (nameOverride := name)

  -- Compute input name for focus handling
  let inputName := if name.isEmpty then widgetName else name

  -- Get focus state
  let focusedInput ← useFocusedInputW

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Constants for state transitions
  let caseSens := config.caseSensitive
  let maxSugg := config.maxSuggestions
  let minChars := config.minChars

  -- Initial state
  let initialState : AutocompleteState := {}

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    let ke := kd.event
    match ke.code with
    | .char c =>
      -- Only handle printable characters
      if c.val >= 32 then
        some fun (s : AutocompleteState) => s.insertChar c
      else
        none
    | .backspace => some fun (s : AutocompleteState) => s.backspace
    | .delete => some fun (s : AutocompleteState) => s.delete
    | .left => some fun (s : AutocompleteState) => s.moveLeft
    | .right => some fun (s : AutocompleteState) => s.moveRight
    | .home => some fun (s : AutocompleteState) => s.moveHome
    | .end => some fun (s : AutocompleteState) => s.moveEnd
    | .up => some fun (s : AutocompleteState) =>
      let currentSuggestions := filterSuggestions s.text suggestions caseSens maxSugg
      let suggestionCount := currentSuggestions.size
      if s.showSuggestions && suggestionCount > 0 then
        s.highlightUp suggestionCount
      else
        s
    | .down => some fun (s : AutocompleteState) =>
      let currentSuggestions := filterSuggestions s.text suggestions caseSens maxSugg
      let suggestionCount := currentSuggestions.size
      if s.showSuggestions && suggestionCount > 0 then
        s.highlightDown suggestionCount
      else if s.text.length >= minChars then
        { s with showSuggestions := true, highlightIndex := 0 }
      else
        s
    | .tab => some fun (s : AutocompleteState) =>
      -- Autocomplete with highlighted suggestion
      let currentSuggestions := filterSuggestions s.text suggestions caseSens maxSugg
      let suggestionCount := currentSuggestions.size
      if s.showSuggestions && suggestionCount > 0 then
        if h : s.highlightIndex < currentSuggestions.size then
          let selected := currentSuggestions[s.highlightIndex]
          s.setText selected
        else
          s
      else
        s
    | .enter => some fun (s : AutocompleteState) =>
      -- Select highlighted suggestion if visible, otherwise just hide
      let currentSuggestions := filterSuggestions s.text suggestions caseSens maxSugg
      let suggestionCount := currentSuggestions.size
      if s.showSuggestions && suggestionCount > 0 then
        if h : s.highlightIndex < currentSuggestions.size then
          let selected := currentSuggestions[s.highlightIndex]
          s.setText selected
        else
          s.hideSuggestions
      else
        s.hideSuggestions
    | .escape => some fun (s : AutocompleteState) => s.hideSuggestions
    | _ => none) keyEvents

  -- Fold state operations - no subscribe needed!
  let stateDyn ← foldDyn id initialState stateOps

  -- Derive value dynamic from state
  let valueDyn ← stateDyn.map' (·.text)

  -- Derive selected item from state
  let selectedItemDyn ← stateDyn.map' fun (s : AutocompleteState) =>
    let currentSuggestions := filterSuggestions s.text suggestions caseSens maxSugg
    if s.showSuggestions && currentSuggestions.size > 0 then
      if h : s.highlightIndex < currentSuggestions.size then
        some currentSuggestions[s.highlightIndex]
      else
        none
    else
      none

  -- For selection events on Tab (autocomplete)
  let tabEvents ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .tab) keyEvents

  -- Attach current state to Tab events to get selected suggestion
  let selectEventFromTab ← Event.mapMaybeM (fun (s : AutocompleteState) =>
    let currentSuggestions := filterSuggestions s.text suggestions caseSens maxSugg
    if s.showSuggestions && currentSuggestions.size > 0 then
      if h : s.highlightIndex < currentSuggestions.size then
        some currentSuggestions[s.highlightIndex]
      else
        none
    else
      none)
    (← Event.attachWithM (fun (s : AutocompleteState) _ => s) stateDyn.current tabEvents)

  -- For Enter events
  let enterEvents ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .enter) keyEvents

  -- Attach current state to Enter events to get selected suggestion or current text
  let enterEventWithState ← Event.attachWithM
    (fun (s : AutocompleteState) _ => s) stateDyn.current enterEvents

  -- Select event from Enter (only when suggestions are visible and selected)
  let selectEventFromEnter ← Event.mapMaybeM (fun (s : AutocompleteState) =>
    let currentSuggestions := filterSuggestions s.text suggestions caseSens maxSugg
    if s.showSuggestions && currentSuggestions.size > 0 then
      if h : s.highlightIndex < currentSuggestions.size then
        some currentSuggestions[s.highlightIndex]
      else
        none
    else
      none) enterEventWithState

  -- Submit event from Enter (selected suggestion or current text)
  let submitEvent ← Event.mapM (fun (s : AutocompleteState) =>
    let currentSuggestions := filterSuggestions s.text suggestions caseSens maxSugg
    if s.showSuggestions && currentSuggestions.size > 0 then
      if h : s.highlightIndex < currentSuggestions.size then
        currentSuggestions[s.highlightIndex]
      else
        s.text
    else
      s.text) enterEventWithState

  -- Merge select events from Tab and Enter
  let selectEvent ← Event.leftmostM [selectEventFromTab, selectEventFromEnter]

  -- Emit render function
  let node ← focusedInput.zipWith' (fun currentFocus (state : AutocompleteState) =>
    let isFocused := currentFocus == some inputName

    -- Filter suggestions
    let currentSuggestions := filterSuggestions state.text suggestions
      config.caseSensitive config.maxSuggestions
    let shouldShowSuggestions := isFocused && state.showSuggestions
      && state.text.length >= config.minChars && currentSuggestions.size > 0

    -- Build input line
    let inputNode := buildInputNode state isFocused config

    -- Build suggestion dropdown if needed
    if shouldShowSuggestions then
      let suggestionNodes := buildSuggestionsNodes currentSuggestions state.highlightIndex config
      RNode.column 0 {} (#[inputNode] ++ suggestionNodes)
    else
      inputNode
  ) stateDyn
  emit node

  pure {
    value := valueDyn
    selectedItem := selectedItemDyn
    onSelect := selectEvent
    onSubmit := submitEvent
  }

/-- Create an autocomplete widget with dynamic suggestions.

    The suggestions array can change over time, useful for async search
    or when suggestions depend on external state.

    This implementation uses FRP-idiomatic state management via foldDyn,
    avoiding imperative subscribe patterns with IO.mkRef.

    Example:
    ```
    let searchResults ← someDynamicSearchFunction
    let auto ← dynAutocomplete' "search" searchResults { placeholder := "Search..." }
    ```
-/
def dynAutocomplete' (name : String) (suggestions : Dynamic Spider (Array String))
    (config : AutocompleteConfig := {}) : WidgetM AutocompleteResult := do
  -- Register as focusable input
  let widgetName ← registerComponentW "dynAutocomplete" (isInput := true) (nameOverride := name)

  -- Compute input name for focus handling
  let inputName := if name.isEmpty then widgetName else name

  -- Get focus state
  let focusedInput ← useFocusedInputW

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Constants for state transitions
  let caseSens := config.caseSensitive
  let maxSugg := config.maxSuggestions
  let minChars := config.minChars

  -- Initial state
  let initialState : AutocompleteState := {}

  -- Attach suggestions to key events
  let keyEventsWithSuggestions ← Event.attachWithM
    (fun (currentSuggestions : Array String) (kd : KeyData) => (currentSuggestions, kd))
    suggestions.current keyEvents

  -- Map key events (with suggestions) to state transformation functions
  let keyStateOps ← Event.mapMaybeM (fun ((currentSuggestionsAll, kd) : Array String × KeyData) =>
    let ke := kd.event
    match ke.code with
    | .char c =>
      -- Only handle printable characters
      if c.val >= 32 then
        some fun (s : AutocompleteState) => s.insertChar c
      else
        none
    | .backspace => some fun (s : AutocompleteState) => s.backspace
    | .delete => some fun (s : AutocompleteState) => s.delete
    | .left => some fun (s : AutocompleteState) => s.moveLeft
    | .right => some fun (s : AutocompleteState) => s.moveRight
    | .home => some fun (s : AutocompleteState) => s.moveHome
    | .end => some fun (s : AutocompleteState) => s.moveEnd
    | .up => some fun (s : AutocompleteState) =>
      let currentSuggestions := filterSuggestions s.text currentSuggestionsAll caseSens maxSugg
      let suggestionCount := currentSuggestions.size
      if s.showSuggestions && suggestionCount > 0 then
        s.highlightUp suggestionCount
      else
        s
    | .down => some fun (s : AutocompleteState) =>
      let currentSuggestions := filterSuggestions s.text currentSuggestionsAll caseSens maxSugg
      let suggestionCount := currentSuggestions.size
      if s.showSuggestions && suggestionCount > 0 then
        s.highlightDown suggestionCount
      else if s.text.length >= minChars then
        { s with showSuggestions := true, highlightIndex := 0 }
      else
        s
    | .tab => some fun (s : AutocompleteState) =>
      -- Autocomplete with highlighted suggestion
      let currentSuggestions := filterSuggestions s.text currentSuggestionsAll caseSens maxSugg
      let suggestionCount := currentSuggestions.size
      if s.showSuggestions && suggestionCount > 0 then
        if h : s.highlightIndex < currentSuggestions.size then
          let selected := currentSuggestions[s.highlightIndex]
          s.setText selected
        else
          s
      else
        s
    | .enter => some fun (s : AutocompleteState) =>
      -- Select highlighted suggestion if visible, otherwise just hide
      let currentSuggestions := filterSuggestions s.text currentSuggestionsAll caseSens maxSugg
      let suggestionCount := currentSuggestions.size
      if s.showSuggestions && suggestionCount > 0 then
        if h : s.highlightIndex < currentSuggestions.size then
          let selected := currentSuggestions[s.highlightIndex]
          s.setText selected
        else
          s.hideSuggestions
      else
        s.hideSuggestions
    | .escape => some fun (s : AutocompleteState) => s.hideSuggestions
    | _ => none) keyEventsWithSuggestions

  -- Fold state operations - no subscribe needed!
  let stateDyn ← foldDyn id initialState keyStateOps

  -- Derive value dynamic from state
  let valueDyn ← stateDyn.map' (·.text)

  -- Derive selected item from state (needs suggestions)
  let selectedItemDyn ← stateDyn.zipWith' (fun (s : AutocompleteState) (currentSuggestionsAll : Array String) =>
    let currentSuggestions := filterSuggestions s.text currentSuggestionsAll caseSens maxSugg
    if s.showSuggestions && currentSuggestions.size > 0 then
      if h : s.highlightIndex < currentSuggestions.size then
        some currentSuggestions[s.highlightIndex]
      else
        none
    else
      none) suggestions

  -- For selection events on Tab (autocomplete)
  let tabEventsWithSuggestions ← Event.filterM (fun ((_, kd) : Array String × KeyData) =>
    kd.event.code == .tab) keyEventsWithSuggestions

  -- Attach current state to Tab events to get selected suggestion
  let selectEventFromTab ← Event.mapMaybeM (fun ((currentSuggestionsAll, _), s) =>
    let currentSuggestions := filterSuggestions s.text currentSuggestionsAll caseSens maxSugg
    if s.showSuggestions && currentSuggestions.size > 0 then
      if h : s.highlightIndex < currentSuggestions.size then
        some currentSuggestions[s.highlightIndex]
      else
        none
    else
      none)
    (← Event.attachWithM (fun (s : AutocompleteState) (data : Array String × KeyData) => (data, s))
      stateDyn.current tabEventsWithSuggestions)

  -- For Enter events
  let enterEventsWithSuggestions ← Event.filterM (fun ((_, kd) : Array String × KeyData) =>
    kd.event.code == .enter) keyEventsWithSuggestions

  -- Attach current state to Enter events to get selected suggestion or current text
  let enterEventWithState ← Event.attachWithM
    (fun (s : AutocompleteState) (data : Array String × KeyData) => (data, s))
    stateDyn.current enterEventsWithSuggestions

  -- Select event from Enter (only when suggestions are visible and selected)
  let selectEventFromEnter ← Event.mapMaybeM (fun ((currentSuggestionsAll, _), s) =>
    let currentSuggestions := filterSuggestions s.text currentSuggestionsAll caseSens maxSugg
    if s.showSuggestions && currentSuggestions.size > 0 then
      if h : s.highlightIndex < currentSuggestions.size then
        some currentSuggestions[s.highlightIndex]
      else
        none
    else
      none) enterEventWithState

  -- Submit event from Enter (selected suggestion or current text)
  let submitEvent ← Event.mapM (fun ((currentSuggestionsAll, _), s) =>
    let currentSuggestions := filterSuggestions s.text currentSuggestionsAll caseSens maxSugg
    if s.showSuggestions && currentSuggestions.size > 0 then
      if h : s.highlightIndex < currentSuggestions.size then
        currentSuggestions[s.highlightIndex]
      else
        s.text
    else
      s.text) enterEventWithState

  -- Merge select events from Tab and Enter
  let selectEvent ← Event.leftmostM [selectEventFromTab, selectEventFromEnter]

  -- Emit render function
  let node ← focusedInput.zipWith3' (fun currentFocus (state : AutocompleteState) currentSuggestionsAll =>
    let isFocused := currentFocus == some inputName

    -- Filter suggestions
    let currentSuggestions := filterSuggestions state.text currentSuggestionsAll
      config.caseSensitive config.maxSuggestions
    let shouldShowSuggestions := isFocused && state.showSuggestions
      && state.text.length >= config.minChars && currentSuggestions.size > 0

    -- Build input line
    let inputNode := buildInputNode state isFocused config

    -- Build suggestion dropdown if needed
    if shouldShowSuggestions then
      let suggestionNodes := buildSuggestionsNodes currentSuggestions state.highlightIndex config
      RNode.column 0 {} (#[inputNode] ++ suggestionNodes)
    else
      inputNode
  ) stateDyn suggestions
  emit node

  pure {
    value := valueDyn
    selectedItem := selectedItemDyn
    onSelect := selectEvent
    onSubmit := submitEvent
  }

/-! ## Convenience Functions -/

/-- Create a simple autocomplete with default styling. -/
def simpleAutocomplete' (name : String) (suggestions : Array String)
    (placeholder : String := "") (width : Nat := 20) : WidgetM AutocompleteResult :=
  autocomplete' name suggestions { placeholder, width }

/-- Create an autocomplete that shows all suggestions on focus (minChars = 0). -/
def dropdownAutocomplete' (name : String) (suggestions : Array String)
    (config : AutocompleteConfig := {}) : WidgetM AutocompleteResult :=
  autocomplete' name suggestions { config with minChars := 0 }

end Terminus.Reactive
