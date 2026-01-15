/-
  Terminus Reactive - Dropdown Widget
  A click-to-open select menu that shows options in a dropdown list.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Dropdown Configuration -/

/-- Configuration for dropdown appearance and behavior. -/
structure DropdownConfig where
  /-- Placeholder text when no item is selected. -/
  placeholder : String := "Select..."
  /-- Minimum width for the dropdown display. -/
  width : Nat := 20
  /-- Maximum number of visible items when open. -/
  maxVisible : Nat := 5
  /-- Character shown when dropdown is open. -/
  openChar : String := "▼"
  /-- Character shown when dropdown is closed. -/
  closedChar : String := "▶"
  /-- Style for the selected/highlighted item in the list. -/
  selectedStyle : Style := { fg := .ansi .cyan }
  /-- Style when the dropdown is focused. -/
  focusedStyle : Style := { fg := .ansi .white, bg := .ansi .blue }
  /-- Style for unselected items. -/
  itemStyle : Style := {}
  /-- Style for the dropdown header when closed. -/
  headerStyle : Style := {}
  /-- Border type for the dropdown list. -/
  borderType : BorderType := .single
  /-- Style for borders. -/
  borderStyle : Style := { fg := .ansi .brightBlack }
  /-- Whether to wrap around at list boundaries. -/
  wrapAround : Bool := true
  deriving Inhabited

/-! ## Dropdown Result -/

/-- Result returned by dropdown' containing reactive values and events. -/
structure DropdownResult where
  /-- Currently selected index (none if no selection). -/
  selectedIndex : Reactive.Dynamic Spider (Option Nat)
  /-- Currently selected item text (none if no selection). -/
  selectedItem : Reactive.Dynamic Spider (Option String)
  /-- Whether the dropdown is currently open. -/
  isOpen : Reactive.Dynamic Spider Bool
  /-- Event fired when an item is selected. -/
  onSelect : Reactive.Event Spider Nat

/-! ## Dropdown State -/

/-- Internal state for dropdown. -/
private structure DropdownState where
  /-- Whether the dropdown is open. -/
  isOpen : Bool := false
  /-- Currently highlighted index (when navigating open list). -/
  highlightedIndex : Nat := 0
  /-- Selected index (persisted selection). -/
  selectedIndex : Option Nat := none
  /-- Scroll offset for list scrolling. -/
  scrollOffset : Nat := 0
  deriving Repr, Inhabited, BEq

namespace DropdownState

/-- Open the dropdown, setting highlight to current selection. -/
def openMenu (s : DropdownState) : DropdownState :=
  { s with
    isOpen := true
    highlightedIndex := s.selectedIndex.getD 0
  }

/-- Close the dropdown. -/
def closeMenu (s : DropdownState) : DropdownState :=
  { s with isOpen := false }

/-- Toggle open/close state. -/
def toggle (s : DropdownState) : DropdownState :=
  if s.isOpen then s.closeMenu else s.openMenu

/-- Move highlight up. -/
def moveUp (s : DropdownState) (itemCount : Nat) (wrap : Bool) : DropdownState :=
  if itemCount == 0 then s
  else if s.highlightedIndex == 0 then
    if wrap then { s with highlightedIndex := itemCount - 1 }
    else s
  else
    { s with highlightedIndex := s.highlightedIndex - 1 }

/-- Move highlight down. -/
def moveDown (s : DropdownState) (itemCount : Nat) (wrap : Bool) : DropdownState :=
  if itemCount == 0 then s
  else if s.highlightedIndex >= itemCount - 1 then
    if wrap then { s with highlightedIndex := 0 }
    else s
  else
    { s with highlightedIndex := s.highlightedIndex + 1 }

/-- Jump to first item. -/
def moveToFirst (s : DropdownState) : DropdownState :=
  { s with highlightedIndex := 0, scrollOffset := 0 }

/-- Jump to last item. -/
def moveToLast (s : DropdownState) (itemCount : Nat) : DropdownState :=
  if itemCount == 0 then s
  else { s with highlightedIndex := itemCount - 1 }

/-- Select the currently highlighted item and close. -/
def selectHighlighted (s : DropdownState) : DropdownState :=
  { s with
    selectedIndex := some s.highlightedIndex
    isOpen := false
  }

/-- Adjust scroll offset to keep highlighted item visible. -/
def adjustScroll (s : DropdownState) (maxVisible : Nat) : DropdownState :=
  if maxVisible == 0 then s
  else
    let newOffset := if s.highlightedIndex < s.scrollOffset then
      s.highlightedIndex
    else if s.highlightedIndex >= s.scrollOffset + maxVisible then
      s.highlightedIndex - maxVisible + 1
    else
      s.scrollOffset
    { s with scrollOffset := newOffset }

end DropdownState

/-! ## Dropdown Widget -/

/-- Create a select dropdown widget.

    A click-to-open select menu that shows the currently selected value when closed
    and displays a dropdown list of options when opened.

    The widget handles:
    - Space/Enter to toggle open/close
    - Arrow keys to navigate when open
    - Enter to select highlighted item
    - Escape to close without selection
    - Vim keys (j/k) for navigation

    Example:
    ```
    let options := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
    let dropdown ← selectDropdown' "fruit" options { placeholder := "Choose fruit..." }
    -- Use dropdown.selectedItem to get current selection
    -- Use dropdown.onSelect to handle selection changes
    ```
-/
def selectDropdown' (name : String) (items : Array String) (config : DropdownConfig := {})
    : WidgetM DropdownResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "dropdown" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Constants for state transitions
  let itemCount := items.size
  let wrap := config.wrapAround
  let maxVis := config.maxVisible

  -- Initial state
  let initialState : DropdownState := {}

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    let ke := kd.event
    some fun (state : DropdownState) =>
      if state.isOpen then
        -- Handle navigation when open
        match ke.code with
        | .up | .char 'k' =>
          state.moveUp itemCount wrap |>.adjustScroll maxVis
        | .down | .char 'j' =>
          state.moveDown itemCount wrap |>.adjustScroll maxVis
        | .home =>
          state.moveToFirst.adjustScroll maxVis
        | .end =>
          state.moveToLast itemCount |>.adjustScroll maxVis
        | .enter | .space =>
          -- Select the highlighted item
          state.selectHighlighted
        | .escape =>
          -- Close without selecting
          state.closeMenu
        | _ => state
      else
        -- Handle activation when closed
        match ke.code with
        | .enter | .space | .down | .char 'j' =>
          state.openMenu.adjustScroll maxVis
        | _ => state) keyEvents

  -- Fold state operations - NO subscribe needed!
  let stateDyn ← foldDyn (fun op state => op state) initialState stateOps

  -- Derive dynamics from state
  let isOpenDyn ← stateDyn.map' (·.isOpen)
  let selectedIndexDyn ← stateDyn.map' (·.selectedIndex)
  let selectedItemDyn ← stateDyn.map' fun (state : DropdownState) =>
    match state.selectedIndex with
    | some idx => if h : idx < items.size then some items[idx] else none
    | none => none

  -- For selection event, filter enter/space events when open and attach state
  let enterSpaceEvents ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .enter || kd.event.code == .space) keyEvents

  -- Attach current state to enter/space events, then filter for when open
  let selectEvent ← Event.mapMaybeM (fun (state : DropdownState) =>
      -- Only fire if the dropdown was open (before this selection closed it)
      -- and the highlighted index is valid
      if h : state.highlightedIndex < items.size then
        some state.highlightedIndex
      else
        none)
    (← Event.attachWithM (fun (state : DropdownState) _ => state) stateDyn.current enterSpaceEvents)

  -- Focus for rendering
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render the dropdown
  let node ← focusDyn.zipWith' (fun isFocused state => Id.run do
    let mut rows : Array RNode := #[]

    -- Render header (selected value or placeholder)
    let headerText := match state.selectedIndex with
      | some idx =>
        if h : idx < items.size then items[idx]
        else config.placeholder
      | none => config.placeholder

    let arrow := if state.isOpen then config.openChar else config.closedChar
    let headerStyle := if isFocused then config.focusedStyle else config.headerStyle

    -- Pad header to width
    let displayText := headerText
    let padding := if displayText.length < config.width - 2
                   then config.width - 2 - displayText.length else 0
    let paddedText := displayText ++ String.ofList (List.replicate padding ' ')

    let headerNode := RNode.text s!"[{arrow} {paddedText}]" headerStyle
    rows := rows.push headerNode

    -- Render dropdown list if open
    if state.isOpen && !items.isEmpty then
      let startIdx := state.scrollOffset
      let endIdx := min (startIdx + config.maxVisible) items.size

      let mut listRows : Array RNode := #[]

      -- Top scroll indicator
      if startIdx > 0 then
        listRows := listRows.push (RNode.text "  ..." config.borderStyle)

      -- Render visible items
      for i in [startIdx:endIdx] do
        if h : i < items.size then
          let item := items[i]
          let isHighlighted := i == state.highlightedIndex
          let itemPrefix := if isHighlighted then "> " else "  "
          let style := if isHighlighted then config.selectedStyle else config.itemStyle

          -- Pad item to width
          let itemPadding := if item.length < config.width - 2
                             then config.width - 2 - item.length else 0
          let paddedItem := item ++ String.ofList (List.replicate itemPadding ' ')

          listRows := listRows.push (RNode.text s!"{itemPrefix}{paddedItem}" style)

      -- Bottom scroll indicator
      if endIdx < items.size then
        listRows := listRows.push (RNode.text "  ..." config.borderStyle)

      -- Wrap list in a simple border (just indent for now)
      let listColumn := RNode.column 0 {} listRows
      rows := rows.push listColumn

    return RNode.column 0 {} rows
  ) stateDyn
  emit node

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    isOpen := isOpenDyn
    onSelect := selectEvent
  }

/-- Create a select dropdown with an initial selection. -/
def selectDropdownWithDefault' (name : String) (items : Array String) (initialIndex : Nat)
    (config : DropdownConfig := {}) : WidgetM DropdownResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "dropdown" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Constants for state transitions
  let itemCount := items.size
  let wrap := config.wrapAround
  let maxVis := config.maxVisible

  -- Track internal state with initial selection
  let clampedInitial := if initialIndex < items.size then initialIndex else 0
  let initialState : DropdownState := {
    selectedIndex := if items.isEmpty then none else some clampedInitial
    highlightedIndex := clampedInitial
  }

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    let ke := kd.event
    some fun (state : DropdownState) =>
      if state.isOpen then
        -- Handle navigation when open
        match ke.code with
        | .up | .char 'k' =>
          state.moveUp itemCount wrap |>.adjustScroll maxVis
        | .down | .char 'j' =>
          state.moveDown itemCount wrap |>.adjustScroll maxVis
        | .home =>
          state.moveToFirst.adjustScroll maxVis
        | .end =>
          state.moveToLast itemCount |>.adjustScroll maxVis
        | .enter | .space =>
          state.selectHighlighted
        | .escape =>
          state.closeMenu
        | _ => state
      else
        -- Handle activation when closed
        match ke.code with
        | .enter | .space | .down | .char 'j' =>
          state.openMenu.adjustScroll maxVis
        | _ => state) keyEvents

  -- Fold state operations - NO subscribe needed!
  let stateDyn ← foldDyn (fun op state => op state) initialState stateOps

  -- Derive dynamics from state
  let isOpenDyn ← stateDyn.map' (·.isOpen)
  let selectedIndexDyn ← stateDyn.map' (·.selectedIndex)
  let selectedItemDyn ← stateDyn.map' fun (state : DropdownState) =>
    match state.selectedIndex with
    | some idx => if h : idx < items.size then some items[idx] else none
    | none => none

  -- For selection event, filter enter/space events when open and attach state
  let enterSpaceEvents ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .enter || kd.event.code == .space) keyEvents

  -- Attach current state to enter/space events, then filter for when open
  let selectEvent ← Event.mapMaybeM (fun (state : DropdownState) =>
      -- Only fire if the highlighted index is valid
      if h : state.highlightedIndex < items.size then
        some state.highlightedIndex
      else
        none)
    (← Event.attachWithM (fun (state : DropdownState) _ => state) stateDyn.current enterSpaceEvents)

  -- Focus for rendering
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render (same as dropdown')
  let node ← focusDyn.zipWith' (fun isFocused state => Id.run do
    let mut rows : Array RNode := #[]

    let headerText := match state.selectedIndex with
      | some idx =>
        if h : idx < items.size then items[idx]
        else config.placeholder
      | none => config.placeholder

    let arrow := if state.isOpen then config.openChar else config.closedChar
    let headerStyle := if isFocused then config.focusedStyle else config.headerStyle

    let displayText := headerText
    let padding := if displayText.length < config.width - 2
                   then config.width - 2 - displayText.length else 0
    let paddedText := displayText ++ String.ofList (List.replicate padding ' ')

    let headerNode := RNode.text s!"[{arrow} {paddedText}]" headerStyle
    rows := rows.push headerNode

    if state.isOpen && !items.isEmpty then
      let startIdx := state.scrollOffset
      let endIdx := min (startIdx + config.maxVisible) items.size

      let mut listRows : Array RNode := #[]

      if startIdx > 0 then
        listRows := listRows.push (RNode.text "  ..." config.borderStyle)

      for i in [startIdx:endIdx] do
        if h : i < items.size then
          let item := items[i]
          let isHighlighted := i == state.highlightedIndex
          let itemPrefix := if isHighlighted then "> " else "  "
          let style := if isHighlighted then config.selectedStyle else config.itemStyle

          let itemPadding := if item.length < config.width - 2
                             then config.width - 2 - item.length else 0
          let paddedItem := item ++ String.ofList (List.replicate itemPadding ' ')

          listRows := listRows.push (RNode.text s!"{itemPrefix}{paddedItem}" style)

      if endIdx < items.size then
        listRows := listRows.push (RNode.text "  ..." config.borderStyle)

      let listColumn := RNode.column 0 {} listRows
      rows := rows.push listColumn

    return RNode.column 0 {} rows
  ) stateDyn
  emit node

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    isOpen := isOpenDyn
    onSelect := selectEvent
  }

end Terminus.Reactive
