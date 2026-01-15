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
  /-- External scroll control callback for auto-scroll to selection.
      Called with the selected index when selection changes. -/
  scrollToY : Option (Nat → IO Unit) := none
  deriving Inhabited

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
def selectableList' [ToString α] [BEq α] (items : Array α) (initial : Nat := 0)
    (config : ListConfig := {}) : WidgetM (ListResult α) := do
  -- Register as focusable component
  let widgetName ← registerComponentW "selectableList" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the list's focus name before calling useFocusedKeyEventsW
  let listName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW listName config.globalKeys

  -- Constants for state transitions
  let maxVis := config.maxVisible.getD items.size
  let itemCount := items.size
  let wrap := config.wrapAround

  -- Initial state
  let initialSelected := if initial < items.size then initial else 0
  let initialState : ListState :=
    ({ selected := initialSelected, scrollOffset := 0 } : ListState).adjustScroll maxVis

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    match kd.event.code with
    | .up => some fun (s : ListState) => s.moveUp itemCount wrap |>.adjustScroll maxVis
    | .down => some fun (s : ListState) => s.moveDown itemCount wrap |>.adjustScroll maxVis
    | .char 'k' => some fun (s : ListState) => s.moveUp itemCount wrap |>.adjustScroll maxVis
    | .char 'j' => some fun (s : ListState) => s.moveDown itemCount wrap |>.adjustScroll maxVis
    | .home => some fun (s : ListState) => s.moveToFirst.adjustScroll maxVis
    | .end => some fun (s : ListState) => s.moveToLast itemCount |>.adjustScroll maxVis
    | _ => none) keyEvents

  -- Fold state operations - no subscribe needed!
  let stateDyn ← foldDyn id initialState stateOps

  -- Derive selectedIndex from state
  let selectedIndexDyn ← stateDyn.map' (·.selected)

  -- Derive selectedItem from state
  let selectedItemDyn ← stateDyn.map' fun (s : ListState) =>
    if h : s.selected < items.size then some items[s.selected] else none

  -- For selection events on Enter/Space
  let enterSpaceEvents ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .enter || kd.event.code == .space) keyEvents

  -- Attach current state to Enter/Space events to get selected item
  let selectEvent ← Event.mapMaybeM (fun (s : ListState) =>
    if h : s.selected < items.size then some items[s.selected] else none)
    (← Event.attachWithM (fun (s : ListState) _ => s) stateDyn.current enterSpaceEvents)

  -- Selection change event derived from state updates
  let selectionChangeEvent ← Event.mapM (·.selected) stateDyn.updated

  -- Handle scrollToY callback if provided (side effect on selection change)
  match config.scrollToY with
  | some scrollFn =>
    let _unsub ← SpiderM.liftIO <| selectionChangeEvent.subscribe scrollFn
  | none => pure ()

  let node ← stateDyn.map' fun state =>
    Id.run do
      if items.isEmpty then
        return RNode.text "(empty)" config.style
      else
        let maxVis := config.maxVisible.getD items.size

        let startIdx := state.scrollOffset
        let endIdx := min (startIdx + maxVis) items.size

        let mut rows : Array RNode := #[]

        if config.showScrollIndicators && startIdx > 0 then
          rows := rows.push (RNode.text "  ..." config.scrollIndicatorStyle)

        for i in [startIdx:endIdx] do
          if h : i < items.size then
            let item := items[i]
            let isSelected := i == state.selected
            let itemPrefix := if isSelected then config.selectedPrefix else config.unselectedPrefix
            let style := if isSelected then config.selectedStyle else config.style
            let text := itemPrefix ++ toString item
            rows := rows.push (RNode.text text style)

        if config.showScrollIndicators && endIdx < items.size then
          rows := rows.push (RNode.text "  ..." config.scrollIndicatorStyle)

        return RNode.column 0 {} rows
  emit node

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    onSelect := selectEvent
    onSelectionChange := selectionChangeEvent
  }

/-- Create a selectable list with a dynamic item array.
    The list updates when items change, preserving selection where possible.

    This implementation uses FRP-idiomatic state management via foldDyn,
    avoiding imperative subscribe patterns with IO.mkRef. -/
def dynSelectableList' [ToString α] [BEq α] (items : Reactive.Dynamic Spider (Array α)) (initial : Nat := 0)
    (config : ListConfig := {}) : WidgetM (ListResult α) := do
  -- Register as focusable component
  let widgetName ← registerComponentW "dynSelectableList" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the list's focus name before calling useFocusedKeyEventsW
  let listName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW listName config.globalKeys

  -- Initial state
  let initialState : ListState := { selected := initial, scrollOffset := 0 }

  -- Attach items to key events
  let keyEventsWithItems ← Event.attachWithM (fun (currentItems : Array α) (kd : KeyData) =>
    (currentItems, kd)) items.current keyEvents

  -- Map key events to state operations (depends on current items)
  let keyStateOps ← Event.mapMaybeM (fun ((currentItems, kd) : Array α × KeyData) =>
    if currentItems.isEmpty then none
    else
      let maxVis := config.maxVisible.getD currentItems.size
      let itemCount := currentItems.size
      let wrap := config.wrapAround
      match kd.event.code with
      | .up => some fun (s : ListState) => s.moveUp itemCount wrap |>.adjustScroll maxVis
      | .down => some fun (s : ListState) => s.moveDown itemCount wrap |>.adjustScroll maxVis
      | .char 'k' => some fun (s : ListState) => s.moveUp itemCount wrap |>.adjustScroll maxVis
      | .char 'j' => some fun (s : ListState) => s.moveDown itemCount wrap |>.adjustScroll maxVis
      | .home => some fun (s : ListState) => s.moveToFirst.adjustScroll maxVis
      | .end => some fun (s : ListState) => s.moveToLast itemCount |>.adjustScroll maxVis
      | _ => none) keyEventsWithItems

  -- Map item changes to state operations (clamp selection if needed)
  let itemChangeOps ← Event.mapM (fun (newItems : Array α) =>
    fun (s : ListState) =>
      let maxVis := config.maxVisible.getD newItems.size
      if s.selected >= newItems.size then
        let newSelected := if newItems.isEmpty then 0 else newItems.size - 1
        { s with selected := newSelected }.adjustScroll maxVis
      else
        s.adjustScroll maxVis) items.updated

  -- Merge key operations and item change operations
  let allStateOps ← Event.mergeM keyStateOps itemChangeOps

  -- Fold all state operations
  let stateDyn ← foldDyn id initialState allStateOps

  -- Derive selectedIndex from state
  let selectedIndexDyn ← stateDyn.map' (·.selected)

  -- Derive selectedItem by combining state with items
  let selectedItemDyn ← stateDyn.zipWith' (fun (state : ListState) (currentItems : Array α) =>
    if h : state.selected < currentItems.size then some currentItems[state.selected]
    else none
  ) items

  -- For selection events on Enter/Space
  let enterSpaceEvents ← Event.filterM (fun ((_, kd) : Array α × KeyData) =>
    kd.event.code == .enter || kd.event.code == .space) keyEventsWithItems

  -- Attach current state to Enter/Space events and extract selected item
  let selectEvent ← Event.mapMaybeM (fun ((currentItems, _), state) =>
    if h : state.selected < currentItems.size then some currentItems[state.selected]
    else none)
    (← Event.attachWithM (fun (s : ListState) (data : Array α × KeyData) => (data, s))
      stateDyn.current enterSpaceEvents)

  -- Selection change event derived from state updates
  let selectionChangeEvent ← Event.mapM (·.selected) stateDyn.updated

  -- Handle scrollToY callback if provided (side effect on selection change)
  match config.scrollToY with
  | some scrollFn =>
    let _unsub ← SpiderM.liftIO <| selectionChangeEvent.subscribe scrollFn
  | none => pure ()

  -- Render the list
  let node ← stateDyn.zipWith' (fun state currentItems =>
    Id.run do
      if currentItems.isEmpty then
        return RNode.text "(empty)" config.style
      else
        let maxVis := config.maxVisible.getD currentItems.size

        let selected := if state.selected >= currentItems.size then
          if currentItems.isEmpty then 0 else currentItems.size - 1
        else
          state.selected

        let startIdx := state.scrollOffset
        let endIdx := min (startIdx + maxVis) currentItems.size

        let mut rows : Array RNode := #[]

        if config.showScrollIndicators && startIdx > 0 then
          rows := rows.push (RNode.text "  ..." config.scrollIndicatorStyle)

        for i in [startIdx:endIdx] do
          if h : i < currentItems.size then
            let item := currentItems[i]
            let isSelected := i == selected
            let itemPrefix := if isSelected then config.selectedPrefix else config.unselectedPrefix
            let style := if isSelected then config.selectedStyle else config.style
            let text := itemPrefix ++ toString item
            rows := rows.push (RNode.text text style)

        if config.showScrollIndicators && endIdx < currentItems.size then
          rows := rows.push (RNode.text "  ..." config.scrollIndicatorStyle)

        return RNode.column 0 {} rows
  ) items
  emit node

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
def numberedList' [ToString α] [BEq α] (items : Array α) (initial : Nat := 0)
    (config : ListConfig := {}) : WidgetM (ListResult α) := do
  -- Register as focusable component
  let widgetName ← registerComponentW "numberedList" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the list's focus name before calling useFocusedKeyEventsW
  let listName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW listName config.globalKeys

  -- Constants
  let itemCount := items.size

  -- Initial state (just the selected index for numbered list)
  let initialSelected := if initial < items.size then initial else 0

  -- Map key events to state operations (including number shortcuts 1-9)
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    if items.isEmpty then none
    else
      match kd.event.code with
      | .char c =>
        if c >= '1' && c <= '9' then
          let idx := c.toNat - '1'.toNat
          if idx < itemCount then some fun (_ : Nat) => idx
          else none
        else none
      | .up => some fun (state : Nat) => if state > 0 then state - 1 else state
      | .down => some fun (state : Nat) => if state < itemCount - 1 then state + 1 else state
      | _ => none) keyEvents

  -- Fold state operations
  let selectedIndexDyn ← foldDyn id initialSelected stateOps

  -- Derive selectedItem from state
  let selectedItemDyn ← selectedIndexDyn.map' fun (idx : Nat) =>
    if h : idx < items.size then some items[idx] else none

  -- For selection events on Enter
  let enterEvents ← Event.filterM (fun (kd : KeyData) => kd.event.code == .enter) keyEvents

  -- Attach current state to Enter events to get selected item
  let selectEventFromEnter ← Event.mapMaybeM (fun (idx : Nat) =>
    if h : idx < items.size then some items[idx] else none)
    (← Event.attachWithM (fun (idx : Nat) _ => idx) selectedIndexDyn.current enterEvents)

  -- Number keys (1-9) also trigger selection immediately
  let numberSelectEvents ← Event.mapMaybeM (fun (kd : KeyData) =>
    match kd.event.code with
    | .char c =>
      if c >= '1' && c <= '9' then
        let idx := c.toNat - '1'.toNat
        if h : idx < items.size then some items[idx] else none
      else none
    | _ => none) keyEvents

  -- Merge enter selection and number selection
  let selectEvent ← Event.mergeM selectEventFromEnter numberSelectEvents

  -- Selection change event derived from state updates
  let selectionChangeEvent := selectedIndexDyn.updated

  -- Handle scrollToY callback if provided (side effect on selection change)
  match config.scrollToY with
  | some scrollFn =>
    let _unsub ← SpiderM.liftIO <| selectionChangeEvent.subscribe scrollFn
  | none => pure ()

  let node ← selectedIndexDyn.map' fun selected =>
    Id.run do
      if items.isEmpty then
        return RNode.text "(empty)" config.style
      else
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

        return RNode.column 0 {} rows
  emit node

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    onSelect := selectEvent
    onSelectionChange := selectionChangeEvent
  }

end Terminus.Reactive
