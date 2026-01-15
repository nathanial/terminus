/-
  Terminus Reactive - VirtualList Widget
  Performance-optimized list that only renders visible items.

  VirtualList is designed for efficiently displaying large lists (1000s of items)
  by only creating RNodes for items within the visible window. This provides
  O(visibleRows) rendering complexity instead of O(totalItems).
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## VirtualList Configuration -/

/-- Configuration for VirtualList appearance and behavior. -/
structure VirtualListConfig where
  /-- Number of visible rows in the viewport. -/
  visibleRows : Nat := 10
  /-- Height of each item in rows (for uniform item heights). -/
  itemHeight : Nat := 1
  /-- Style for unselected items. -/
  style : Style := {}
  /-- Style for the selected/highlighted item. -/
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Prefix for selected item (e.g., "> "). -/
  selectedPrefix : String := "> "
  /-- Prefix for unselected items (for alignment). -/
  unselectedPrefix : String := "  "
  /-- Show scroll position indicator on right edge. -/
  scrollIndicator : Bool := true
  /-- Character for scroll indicator track. -/
  trackChar : Char := '░'
  /-- Character for scroll indicator thumb. -/
  thumbChar : Char := '█'
  /-- Style for scroll indicator track. -/
  trackStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for scroll indicator thumb. -/
  thumbStyle : Style := {}
  /-- Whether to wrap around at list boundaries. -/
  wrapAround : Bool := false
  /-- Focus name for keyboard routing. Empty = auto-generated. -/
  focusName : String := ""
  /-- Whether this list responds to keys without focus (legacy behavior). -/
  globalKeys : Bool := false
  deriving Inhabited

/-! ## VirtualList Result -/

/-- Result returned by virtualList' containing reactive values and events. -/
structure VirtualListResult (α : Type) where
  /-- Currently selected index. -/
  selectedIndex : Reactive.Dynamic Spider Nat
  /-- Currently selected item (none if list is empty). -/
  selectedItem : Reactive.Dynamic Spider (Option α)
  /-- Visible range as (startIndex, endIndex) - endIndex is exclusive. -/
  visibleRange : Reactive.Dynamic Spider (Nat × Nat)
  /-- Event fired when Enter is pressed on an item. -/
  onSelect : Reactive.Event Spider Nat
  /-- Event fired when selection changes to a new item. -/
  onChange : Reactive.Event Spider α

/-! ## VirtualList State -/

/-- Internal state for virtual list navigation. -/
structure VirtualListState where
  /-- Currently selected index. -/
  selected : Nat := 0
  /-- Scroll offset (first visible item index). -/
  scrollOffset : Nat := 0
  deriving Repr, Inhabited, BEq

namespace VirtualListState

/-- Move selection up by one item. -/
def moveUp (s : VirtualListState) (itemCount : Nat) (wrap : Bool) : VirtualListState :=
  if itemCount == 0 then s
  else if s.selected == 0 then
    if wrap then { s with selected := itemCount - 1 }
    else s
  else
    { s with selected := s.selected - 1 }

/-- Move selection down by one item. -/
def moveDown (s : VirtualListState) (itemCount : Nat) (wrap : Bool) : VirtualListState :=
  if itemCount == 0 then s
  else if s.selected >= itemCount - 1 then
    if wrap then { s with selected := 0 }
    else s
  else
    { s with selected := s.selected + 1 }

/-- Move selection up by a page (visibleRows items). -/
def pageUp (s : VirtualListState) (visibleRows : Nat) : VirtualListState :=
  let step := if visibleRows > 1 then visibleRows - 1 else 1
  { s with selected := s.selected - min step s.selected }

/-- Move selection down by a page (visibleRows items). -/
def pageDown (s : VirtualListState) (itemCount visibleRows : Nat) : VirtualListState :=
  if itemCount == 0 then s
  else
    let step := if visibleRows > 1 then visibleRows - 1 else 1
    let maxIdx := itemCount - 1
    { s with selected := min (s.selected + step) maxIdx }

/-- Jump to first item. -/
def moveToFirst (s : VirtualListState) : VirtualListState :=
  { s with selected := 0, scrollOffset := 0 }

/-- Jump to last item. -/
def moveToLast (s : VirtualListState) (itemCount : Nat) : VirtualListState :=
  if itemCount == 0 then s
  else { s with selected := itemCount - 1 }

/-- Adjust scroll offset to keep selection visible within the viewport.
    This ensures the selected item is always within [scrollOffset, scrollOffset + visibleRows). -/
def adjustScroll (s : VirtualListState) (visibleRows : Nat) : VirtualListState :=
  if visibleRows == 0 then s
  else
    let newOffset := if s.selected < s.scrollOffset then
      -- Selection is above viewport - scroll up
      s.selected
    else if s.selected >= s.scrollOffset + visibleRows then
      -- Selection is below viewport - scroll down
      s.selected - visibleRows + 1
    else
      -- Selection is within viewport - keep current offset
      s.scrollOffset
    { s with scrollOffset := newOffset }

/-- Compute visible range as (startIndex, endIndex). endIndex is exclusive. -/
def visibleRange (s : VirtualListState) (itemCount visibleRows : Nat) : Nat × Nat :=
  let startIdx := s.scrollOffset
  let endIdx := min (startIdx + visibleRows) itemCount
  (startIdx, endIdx)

end VirtualListState

/-! ## Scroll Indicator Rendering -/

/-- Compute scroll indicator thumb size and position.
    Returns (thumbSize, thumbPosition) for the scrollbar. -/
def computeScrollThumb (itemCount visibleRows scrollOffset trackHeight : Nat) : (Nat × Nat) :=
  if itemCount <= visibleRows || trackHeight == 0 then
    (trackHeight, 0)
  else
    -- Thumb size proportional to visible/total ratio
    let thumbSize := max 1 (trackHeight * visibleRows / itemCount)
    -- Thumb position proportional to scroll offset
    let maxOffset := itemCount - visibleRows
    let scrollableTrack := trackHeight - thumbSize
    let thumbPos := if maxOffset > 0 then scrollableTrack * scrollOffset / maxOffset else 0
    (thumbSize, thumbPos)

/-- Render a vertical scroll indicator column. -/
def renderScrollIndicator (state : VirtualListState) (itemCount : Nat)
    (config : VirtualListConfig) : RNode :=
  if config.visibleRows == 0 then .empty
  else Id.run do
    let trackHeight := config.visibleRows
    let (thumbSize, thumbPos) := computeScrollThumb
      itemCount config.visibleRows state.scrollOffset trackHeight

    let mut chars : Array RNode := #[]
    for i in [:trackHeight] do
      let inThumb := i >= thumbPos && i < thumbPos + thumbSize
      let char := if inThumb then config.thumbChar else config.trackChar
      let style := if inThumb then config.thumbStyle else config.trackStyle
      chars := chars.push (.text char.toString style)

    .column 0 {} chars

/-! ## VirtualList Widget -/

/-- Create a virtual list widget optimized for large datasets.

    The widget only renders items within the visible window, providing O(visibleRows)
    performance instead of O(totalItems). This makes it suitable for lists with
    thousands of items.

    Keyboard bindings (when focused):
    - Up/Down, j/k: Move selection by one item
    - PageUp/PageDown: Move selection by page
    - Home/End: Jump to first/last item
    - Enter/Space: Select current item

    Example:
    ```lean
    let items := Array.range 10000 |>.map (s!"Item {·}")
    let list ← virtualList' "bigList" items { visibleRows := 20 }
    -- Use list.selectedItem to get current selection
    -- Use list.onSelect to handle Enter key
    ```

    Visual layout (showing items 50-59 of 1000):
    ```
      Item 50
      Item 51
    > Item 52  <- selected
      Item 53
      Item 54
      ...       |  <- scroll indicator
    ```
-/
def virtualList' [ToString α] [BEq α] (name : String) (items : Array α)
    (config : VirtualListConfig := {}) : WidgetM (VirtualListResult α) := do
  -- Register as focusable component
  let widgetName ← registerComponentW "virtualList" (isInput := true)
    (nameOverride := if config.focusName.isEmpty then name else config.focusName)

  -- Determine the list's focus name
  let listName := if config.focusName.isEmpty then
    if name.isEmpty then widgetName else name
  else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW listName config.globalKeys

  -- Initialize state
  let initialState : VirtualListState := (VirtualListState.mk 0 0).adjustScroll config.visibleRows

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    if items.isEmpty then none
    else match kd.event.code with
    | .up => some fun (state : VirtualListState) =>
        state.moveUp items.size config.wrapAround |>.adjustScroll config.visibleRows
    | .down => some fun (state : VirtualListState) =>
        state.moveDown items.size config.wrapAround |>.adjustScroll config.visibleRows
    | .char 'k' => some fun (state : VirtualListState) =>
        state.moveUp items.size config.wrapAround |>.adjustScroll config.visibleRows
    | .char 'j' => some fun (state : VirtualListState) =>
        state.moveDown items.size config.wrapAround |>.adjustScroll config.visibleRows
    | .pageUp => some fun (state : VirtualListState) =>
        state.pageUp config.visibleRows |>.adjustScroll config.visibleRows
    | .pageDown => some fun (state : VirtualListState) =>
        state.pageDown items.size config.visibleRows |>.adjustScroll config.visibleRows
    | .home => some fun (state : VirtualListState) =>
        state.moveToFirst.adjustScroll config.visibleRows
    | .end => some fun (state : VirtualListState) =>
        state.moveToLast items.size |>.adjustScroll config.visibleRows
    | _ => none) keyEvents

  -- Fold state operations - no subscribe needed!
  let stateDyn ← foldDyn id initialState stateOps

  -- Derive dynamics from state
  let selectedIndexDyn ← stateDyn.map' (·.selected)
  let selectedItemDyn ← stateDyn.map' fun (state : VirtualListState) =>
    if h : state.selected < items.size then some items[state.selected] else none
  let visibleRangeDyn ← stateDyn.map' fun (state : VirtualListState) =>
    state.visibleRange items.size config.visibleRows

  -- Filter for enter/space key events for selection
  let selectKeyEvents ← Event.filterM (fun (kd : KeyData) =>
    kd.event.code == .enter || kd.event.code == .space) keyEvents

  -- Derive select event (fires selected index when Enter/Space pressed)
  let selectEvent ← Event.attachWithM
    (fun (state : VirtualListState) (_ : KeyData) => state.selected)
    stateDyn.current selectKeyEvents

  -- Derive change event from state updates (fires when selection changes to new item)
  let changeEvent ← Event.mapMaybeM (fun (state : VirtualListState) =>
    if h : state.selected < items.size then some items[state.selected] else none) stateDyn.updated

  -- Render the list
  let node ← stateDyn.map' fun state =>
    Id.run do
      if items.isEmpty then
        return RNode.text "(empty)" config.style
      else
        let (startIdx, endIdx) := state.visibleRange items.size config.visibleRows

        let mut rows : Array RNode := #[]

        -- Only render visible items
        for i in [startIdx:endIdx] do
          if h : i < items.size then
            let item := items[i]
            let isSelected := i == state.selected
            let itemPrefix := if isSelected then config.selectedPrefix else config.unselectedPrefix
            let style := if isSelected then config.selectedStyle else config.style
            let text := itemPrefix ++ toString item
            rows := rows.push (RNode.text text style)

        let contentNode := RNode.column 0 {} rows

        -- Add scroll indicator if enabled and needed
        if config.scrollIndicator && items.size > config.visibleRows then
          let scrollbar := renderScrollIndicator state items.size config
          RNode.row 0 {} #[contentNode, scrollbar]
        else
          contentNode
  emit node

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    visibleRange := visibleRangeDyn
    onSelect := selectEvent
    onChange := changeEvent
  }

/-! ## Dynamic VirtualList Widget -/

/-- Create a virtual list with a dynamic item array.
    The list updates when items change, preserving selection where possible.

    This variant is useful when the list contents can change at runtime,
    such as filtered search results or real-time data feeds.

    Example:
    ```lean
    let searchResults ← someSearchDynamic
    let list ← dynVirtualList' "results" searchResults { visibleRows := 15 }
    ```
-/
def dynVirtualList' [ToString α] [BEq α] [Inhabited α] (name : String)
    (items : Reactive.Dynamic Spider (Array α))
    (config : VirtualListConfig := {}) : WidgetM (VirtualListResult α) := do
  -- Register as focusable component
  let widgetName ← registerComponentW "dynVirtualList" (isInput := true)
    (nameOverride := if config.focusName.isEmpty then name else config.focusName)

  -- Determine the list's focus name
  let listName := if config.focusName.isEmpty then
    if name.isEmpty then widgetName else name
  else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW listName config.globalKeys

  -- Initialize state
  let initialState : VirtualListState := (VirtualListState.mk 0 0).adjustScroll config.visibleRows

  -- Attach items to key events to get item count for state transitions
  let keyEventsWithItems ← Event.attachWithM (fun (currentItems : Array α) (kd : KeyData) =>
    (currentItems, kd)) items.current keyEvents

  -- Map key events with items to state transformation functions
  let keyStateOps ← Event.mapMaybeM (fun ((currentItems, kd) : Array α × KeyData) =>
    if currentItems.isEmpty then none
    else match kd.event.code with
    | .up => some fun (state : VirtualListState) =>
        state.moveUp currentItems.size config.wrapAround |>.adjustScroll config.visibleRows
    | .down => some fun (state : VirtualListState) =>
        state.moveDown currentItems.size config.wrapAround |>.adjustScroll config.visibleRows
    | .char 'k' => some fun (state : VirtualListState) =>
        state.moveUp currentItems.size config.wrapAround |>.adjustScroll config.visibleRows
    | .char 'j' => some fun (state : VirtualListState) =>
        state.moveDown currentItems.size config.wrapAround |>.adjustScroll config.visibleRows
    | .pageUp => some fun (state : VirtualListState) =>
        state.pageUp config.visibleRows |>.adjustScroll config.visibleRows
    | .pageDown => some fun (state : VirtualListState) =>
        state.pageDown currentItems.size config.visibleRows |>.adjustScroll config.visibleRows
    | .home => some fun (state : VirtualListState) =>
        state.moveToFirst.adjustScroll config.visibleRows
    | .end => some fun (state : VirtualListState) =>
        state.moveToLast currentItems.size |>.adjustScroll config.visibleRows
    | _ => none) keyEventsWithItems

  -- Map item changes to state adjustment functions (clamp selection when items shrink)
  let itemAdjustOps ← Event.mapMaybeM (fun (newItems : Array α) =>
    -- Always return a function that clamps if needed
    some fun (state : VirtualListState) =>
      if state.selected >= newItems.size then
        let newSelected := if newItems.isEmpty then 0 else newItems.size - 1
        { state with selected := newSelected }.adjustScroll config.visibleRows
      else
        state
  ) items.updated

  -- Merge both event streams into single state operations stream
  let allStateOps ← Event.mergeM keyStateOps itemAdjustOps

  -- Fold all state operations - no subscribe needed!
  let stateDyn ← foldDyn id initialState allStateOps

  -- Derive selectedIndex from state
  let selectedIndexDyn ← stateDyn.map' (·.selected)

  -- Derive selectedItem by combining state with items
  let selectedItemDyn ← stateDyn.zipWith' (fun (state : VirtualListState) (currentItems : Array α) =>
    if h : state.selected < currentItems.size then some currentItems[state.selected]
    else none
  ) items

  -- Derive visibleRange from state and items
  let visibleRangeDyn ← stateDyn.zipWith' (fun (state : VirtualListState) (currentItems : Array α) =>
    state.visibleRange currentItems.size config.visibleRows
  ) items

  -- Filter for enter/space key events for selection
  let selectKeyEventsWithItems ← Event.filterM (fun ((_, kd) : Array α × KeyData) =>
    kd.event.code == .enter || kd.event.code == .space) keyEventsWithItems

  -- Derive select event (fires selected index when Enter/Space pressed)
  let selectEvent ← Event.attachWithM
    (fun (state : VirtualListState) (_ : Array α × KeyData) => state.selected)
    stateDyn.current selectKeyEventsWithItems

  -- Derive change event: attach items to select event and extract selected item
  let changeEvent ← Event.attachWithM
    (fun (currentItems : Array α) (idx : Nat) =>
      if h : idx < currentItems.size then currentItems[idx] else currentItems[0]!)
    items.current selectEvent

  -- Render the list
  let node ← stateDyn.zipWith' (fun state currentItems =>
    Id.run do
      if currentItems.isEmpty then
        return RNode.text "(empty)" config.style
      else
        let selected := if state.selected >= currentItems.size then
          if currentItems.isEmpty then 0 else currentItems.size - 1
        else
          state.selected

        let (startIdx, endIdx) := state.visibleRange currentItems.size config.visibleRows

        let mut rows : Array RNode := #[]

        -- Only render visible items
        for i in [startIdx:endIdx] do
          if h : i < currentItems.size then
            let item := currentItems[i]
            let isSelected := i == selected
            let itemPrefix := if isSelected then config.selectedPrefix else config.unselectedPrefix
            let style := if isSelected then config.selectedStyle else config.style
            let text := itemPrefix ++ toString item
            rows := rows.push (RNode.text text style)

        let contentNode := RNode.column 0 {} rows

        -- Add scroll indicator if enabled and needed
        if config.scrollIndicator && currentItems.size > config.visibleRows then
          let scrollbar := renderScrollIndicator state currentItems.size config
          RNode.row 0 {} #[contentNode, scrollbar]
        else
          contentNode
  ) items
  emit node

  pure {
    selectedIndex := selectedIndexDyn
    selectedItem := selectedItemDyn
    visibleRange := visibleRangeDyn
    onSelect := selectEvent
    onChange := changeEvent
  }

end Terminus.Reactive
