/-
  Terminus Reactive - List Components
  Selectable list widgets with keyboard navigation.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## List Configuration -/

/-- Configuration for selectable list appearance and behavior. -/
structure ListConfig where
  /-- Style for unselected items. -/
  style : Style := {}
  /-- Style for the selected/highlighted item. -/
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Maximum number of visible items (none = show all). -/
  maxVisible : Option Nat := none
  /-- Prefix for selected item (e.g., "> "). -/
  selectedPrefix : String := "> "
  /-- Prefix for unselected items (for alignment). -/
  unselectedPrefix : String := "  "
  /-- Whether to wrap around at list boundaries. -/
  wrapAround : Bool := true
  /-- Show scroll indicators when list is truncated. -/
  showScrollIndicators : Bool := true
  /-- Style for scroll indicators. -/
  scrollIndicatorStyle : Style := { fg := .ansi .brightBlack }
  /-- Focus name for this list (for keyboard routing). Empty = auto-generated. -/
  focusName : String := ""
  /-- Whether this list responds to keys without focus (legacy behavior). -/
  globalKeys : Bool := false
  deriving Repr, Inhabited

/-! ## List Result -/

/-- Result returned by selectableList' containing reactive values and events. -/
structure ListResult (α : Type) where
  /-- Currently selected index. -/
  selectedIndex : Reactive.Dynamic Spider Nat
  /-- Currently selected item (none if list is empty). -/
  selectedItem : Reactive.Dynamic Spider (Option α)
  /-- Event fired when Enter is pressed on an item. -/
  onSelect : Reactive.Event Spider α
  /-- Event fired when selection changes. -/
  onSelectionChange : Reactive.Event Spider Nat


/-! ## List State -/

/-- Internal state for list navigation. -/
structure ListState where
  /-- Currently selected index. -/
  selected : Nat := 0
  /-- Scroll offset (first visible item index). -/
  scrollOffset : Nat := 0
  deriving Repr, Inhabited

namespace ListState

/-- Move selection up. -/
def moveUp (s : ListState) (itemCount : Nat) (wrap : Bool) : ListState :=
  if itemCount == 0 then s
  else if s.selected == 0 then
    if wrap then { s with selected := itemCount - 1 }
    else s
  else
    { s with selected := s.selected - 1 }

/-- Move selection down. -/
def moveDown (s : ListState) (itemCount : Nat) (wrap : Bool) : ListState :=
  if itemCount == 0 then s
  else if s.selected >= itemCount - 1 then
    if wrap then { s with selected := 0 }
    else s
  else
    { s with selected := s.selected + 1 }

/-- Jump to first item. -/
def moveToFirst (s : ListState) : ListState :=
  { s with selected := 0, scrollOffset := 0 }

/-- Jump to last item. -/
def moveToLast (s : ListState) (itemCount : Nat) : ListState :=
  if itemCount == 0 then s
  else { s with selected := itemCount - 1 }

/-- Adjust scroll offset to keep selection visible. -/
def adjustScroll (s : ListState) (maxVisible : Nat) : ListState :=
  if maxVisible == 0 then s
  else
    let newOffset := if s.selected < s.scrollOffset then
      s.selected
    else if s.selected >= s.scrollOffset + maxVisible then
      s.selected - maxVisible + 1
    else
      s.scrollOffset
    { s with scrollOffset := newOffset }

end ListState

/-! ## Selectable List Widget -/

/-- Create a selectable list widget.

    The widget handles:
    - Arrow key navigation (up/down)
    - Vim keys (j/k)
    - Home/End to jump to first/last
    - Enter to select
    - Scrolling when list exceeds maxVisible

    Example:
    ```
    let items := #["Apple", "Banana", "Cherry"]
    let list ← selectableList' items 0 { maxVisible := some 5 }
    -- Use list.selectedItem to get current selection
    -- Use list.onSelect to handle Enter key
    ```
-/
def selectableList' [ToString α] (items : Array α) (initial : Nat := 0)
    (config : ListConfig := {}) : WidgetM (ListResult α) := do
  let events ← getEventsW

  -- Register as focusable component
  let widgetName ← registerComponentW "selectableList" (isInput := true)
  let focusedInput ← useFocusedInputW

  -- Create trigger events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := α)
  let (selectionChangeEvent, fireSelectionChange) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Nat)
  let (itemEvent, fireItem) ← newTriggerEvent (t := Spider) (a := Option α)

  -- Track internal state
  let initialSelected := if initial < items.size then initial else 0
  let maxVis := config.maxVisible.getD items.size
  let initialState : ListState :=
    ({ selected := initialSelected, scrollOffset := 0 } : ListState).adjustScroll maxVis
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)

  -- Create dynamics
  let selectedIndexDyn ← holdDyn initialSelected indexEvent
  let initialItem := if h : initialSelected < items.size then some items[initialSelected] else none
  let selectedItemDyn ← holdDyn initialItem itemEvent

  -- Determine the list's focus name
  let listName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    -- Check focus (unless globalKeys is enabled)
    let currentFocus ← focusedInput.sample
    let isFocused := config.globalKeys || currentFocus == some listName

    if items.isEmpty || !isFocused then pure ()
    else
      let state ← stateRef.get
      let ke := kd.event

      let maxVis := config.maxVisible.getD items.size

      -- Handle navigation keys
      let newState ← match ke.code with
        | .up => pure (state.moveUp items.size config.wrapAround |>.adjustScroll maxVis)
        | .down => pure (state.moveDown items.size config.wrapAround |>.adjustScroll maxVis)
        | .char 'k' => pure (state.moveUp items.size config.wrapAround |>.adjustScroll maxVis)
        | .char 'j' => pure (state.moveDown items.size config.wrapAround |>.adjustScroll maxVis)
        | .home => pure (state.moveToFirst.adjustScroll maxVis)
        | .end => pure (state.moveToLast items.size |>.adjustScroll maxVis)
        | .enter =>
          if h : state.selected < items.size then
            fireSelect items[state.selected]
          pure state
        | .space =>
          if h : state.selected < items.size then
            fireSelect items[state.selected]
          pure state
        | _ => pure state

      -- Update state and fire events if selection changed
      if newState.selected != state.selected then
        stateRef.set newState
        fireIndex newState.selected
        fireSelectionChange newState.selected
        if h : newState.selected < items.size then
          fireItem (some items[newState.selected])
        else
          fireItem none
      else if newState.scrollOffset != state.scrollOffset then
        stateRef.set newState

  -- Emit render function
  emit do
    if items.isEmpty then
      pure (RNode.text "(empty)" config.style)
    else
      let state ← stateRef.get
      let maxVis := config.maxVisible.getD items.size

      -- Calculate visible range
      let startIdx := state.scrollOffset
      let endIdx := min (startIdx + maxVis) items.size

      -- Build visible items
      let mut rows : Array RNode := #[]

      -- Show "more above" indicator
      if config.showScrollIndicators && startIdx > 0 then
        rows := rows.push (RNode.text "  ..." config.scrollIndicatorStyle)

      -- Render visible items
      for i in [startIdx:endIdx] do
        if h : i < items.size then
          let item := items[i]
          let isSelected := i == state.selected
          let itemPrefix := if isSelected then config.selectedPrefix else config.unselectedPrefix
          let style := if isSelected then config.selectedStyle else config.style
          let text := itemPrefix ++ toString item
          rows := rows.push (RNode.text text style)

      -- Show "more below" indicator
      if config.showScrollIndicators && endIdx < items.size then
        rows := rows.push (RNode.text "  ..." config.scrollIndicatorStyle)

      pure (RNode.column 0 {} rows)

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    onSelect := selectEvent
    onSelectionChange := selectionChangeEvent
  }

/-- Create a selectable list with a dynamic item array.
    The list updates when items change, preserving selection where possible. -/
def dynSelectableList' [ToString α] (items : Reactive.Dynamic Spider (Array α)) (initial : Nat := 0)
    (config : ListConfig := {}) : WidgetM (ListResult α) := do
  let events ← getEventsW

  -- Register as focusable component
  let widgetName ← registerComponentW "dynSelectableList" (isInput := true)
  let focusedInput ← useFocusedInputW

  -- Create trigger events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := α)
  let (selectionChangeEvent, fireSelectionChange) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Nat)
  let (itemEvent, fireItem) ← newTriggerEvent (t := Spider) (a := Option α)

  -- Track internal state
  let initialItems ← SpiderM.liftIO items.sample
  let initialSelected := if initial < initialItems.size then initial else 0
  let initialState : ListState := { selected := initialSelected, scrollOffset := 0 }
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)

  -- Create dynamics
  let selectedIndexDyn ← holdDyn initialSelected indexEvent
  let initialItem := if h : initialSelected < initialItems.size then some initialItems[initialSelected] else none
  let selectedItemDyn ← holdDyn initialItem itemEvent

  -- Determine the list's focus name
  let listName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    -- Check focus (unless globalKeys is enabled)
    let currentFocus ← focusedInput.sample
    let isFocused := config.globalKeys || currentFocus == some listName

    let currentItems ← items.sample
    if currentItems.isEmpty || !isFocused then pure ()
    else
      let state ← stateRef.get
      let ke := kd.event

      let maxVis := config.maxVisible.getD currentItems.size

      -- Handle navigation keys
      let newState ← match ke.code with
        | .up => pure (state.moveUp currentItems.size config.wrapAround |>.adjustScroll maxVis)
        | .down => pure (state.moveDown currentItems.size config.wrapAround |>.adjustScroll maxVis)
        | .char 'k' => pure (state.moveUp currentItems.size config.wrapAround |>.adjustScroll maxVis)
        | .char 'j' => pure (state.moveDown currentItems.size config.wrapAround |>.adjustScroll maxVis)
        | .home => pure (state.moveToFirst.adjustScroll maxVis)
        | .end => pure (state.moveToLast currentItems.size |>.adjustScroll maxVis)
        | .enter =>
          if h : state.selected < currentItems.size then
            fireSelect currentItems[state.selected]
          pure state
        | .space =>
          if h : state.selected < currentItems.size then
            fireSelect currentItems[state.selected]
          pure state
        | _ => pure state

      -- Update state and fire events if selection changed
      if newState.selected != state.selected then
        stateRef.set newState
        fireIndex newState.selected
        fireSelectionChange newState.selected
        if h : newState.selected < currentItems.size then
          fireItem (some currentItems[newState.selected])
        else
          fireItem none
      else if newState.scrollOffset != state.scrollOffset then
        stateRef.set newState

  -- Subscribe to item changes to adjust selection
  let _unsub2 ← SpiderM.liftIO <| items.updated.subscribe fun newItems => do
    let state ← stateRef.get
    -- Clamp selection to new bounds
    if state.selected >= newItems.size then
      let newSelected := if newItems.isEmpty then 0 else newItems.size - 1
      let newState := { state with selected := newSelected }
      stateRef.set newState
      fireIndex newSelected
      if h : newSelected < newItems.size then
        fireItem (some newItems[newSelected])
      else
        fireItem none

  -- Emit render function
  emit do
    let currentItems ← items.sample
    if currentItems.isEmpty then
      pure (RNode.text "(empty)" config.style)
    else
      let state ← stateRef.get
      let maxVis := config.maxVisible.getD currentItems.size

      -- Ensure selection is still valid
      let selected := if state.selected >= currentItems.size then
        if currentItems.isEmpty then 0 else currentItems.size - 1
      else
        state.selected

      -- Calculate visible range
      let startIdx := state.scrollOffset
      let endIdx := min (startIdx + maxVis) currentItems.size

      -- Build visible items
      let mut rows : Array RNode := #[]

      -- Show "more above" indicator
      if config.showScrollIndicators && startIdx > 0 then
        rows := rows.push (RNode.text "  ..." config.scrollIndicatorStyle)

      -- Render visible items
      for i in [startIdx:endIdx] do
        if h : i < currentItems.size then
          let item := currentItems[i]
          let isSelected := i == selected
          let itemPrefix := if isSelected then config.selectedPrefix else config.unselectedPrefix
          let style := if isSelected then config.selectedStyle else config.style
          let text := itemPrefix ++ toString item
          rows := rows.push (RNode.text text style)

      -- Show "more below" indicator
      if config.showScrollIndicators && endIdx < currentItems.size then
        rows := rows.push (RNode.text "  ..." config.scrollIndicatorStyle)

      pure (RNode.column 0 {} rows)

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    onSelect := selectEvent
    onSelectionChange := selectionChangeEvent
  }

/-- Create a simple string list. -/
def stringList' (items : Array String) (initial : Nat := 0)
    (config : ListConfig := {}) : WidgetM (ListResult String) :=
  selectableList' items initial config

/-- Create a numbered list (1. Item, 2. Item, ...). -/
def numberedList' [ToString α] (items : Array α) (initial : Nat := 0)
    (config : ListConfig := {}) : WidgetM (ListResult α) := do
  let events ← getEventsW

  -- Register as focusable component
  let widgetName ← registerComponentW "numberedList" (isInput := true)
  let focusedInput ← useFocusedInputW

  -- Create trigger events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := α)
  let (selectionChangeEvent, fireSelectionChange) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Nat)
  let (itemEvent, fireItem) ← newTriggerEvent (t := Spider) (a := Option α)

  -- Track internal state
  let initialSelected := if initial < items.size then initial else 0
  let stateRef ← SpiderM.liftIO (IO.mkRef initialSelected)

  -- Create dynamics
  let selectedIndexDyn ← holdDyn initialSelected indexEvent
  let initialItem := if h : initialSelected < items.size then some items[initialSelected] else none
  let selectedItemDyn ← holdDyn initialItem itemEvent

  -- Determine the list's focus name
  let listName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Subscribe to key events for number shortcuts (1-9)
  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    -- Check focus (unless globalKeys is enabled)
    let currentFocus ← focusedInput.sample
    let isFocused := config.globalKeys || currentFocus == some listName

    if items.isEmpty || !isFocused then pure ()
    else
      let state ← stateRef.get
      let ke := kd.event

      match ke.code with
      | .char c =>
        if c >= '1' && c <= '9' then
          let idx := c.toNat - '1'.toNat
          if h : idx < items.size then
            stateRef.set idx
            fireIndex idx
            fireSelectionChange idx
            fireItem (some items[idx])
            fireSelect items[idx]
      | .up =>
        if state > 0 then
          let newIdx := state - 1
          stateRef.set newIdx
          fireIndex newIdx
          fireSelectionChange newIdx
          if h : newIdx < items.size then
            fireItem (some items[newIdx])
      | .down =>
        if state < items.size - 1 then
          let newIdx := state + 1
          stateRef.set newIdx
          fireIndex newIdx
          fireSelectionChange newIdx
          if h : newIdx < items.size then
            fireItem (some items[newIdx])
      | .enter =>
        if h : state < items.size then
          fireSelect items[state]
      | _ => pure ()

  -- Emit render function
  emit do
    if items.isEmpty then
      pure (RNode.text "(empty)" config.style)
    else
      let selected ← stateRef.get
      let mut rows : Array RNode := #[]

      for i in [0:items.size] do
        if h : i < items.size then
          let item := items[i]
          let isSelected := i == selected
          let num := i + 1
          let itemPrefix := if isSelected then s!"> {num}. " else s!"  {num}. "
          let style := if isSelected then config.selectedStyle else config.style
          let text := itemPrefix ++ toString item
          rows := rows.push (RNode.text text style)

      pure (RNode.column 0 {} rows)

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    onSelect := selectEvent
    onSelectionChange := selectionChangeEvent
  }

end Terminus.Reactive
