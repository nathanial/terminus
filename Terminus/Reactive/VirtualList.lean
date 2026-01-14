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
def virtualList' [ToString α] (name : String) (items : Array α)
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

  -- Create trigger events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Nat)
  let (changeEvent, fireChange) ← newTriggerEvent (t := Spider) (a := α)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Nat)
  let (itemEvent, fireItem) ← newTriggerEvent (t := Spider) (a := Option α)
  let (rangeEvent, fireRange) ← newTriggerEvent (t := Spider) (a := Nat × Nat)

  -- Initialize state
  let initialState : VirtualListState := (VirtualListState.mk 0 0).adjustScroll config.visibleRows
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := VirtualListState)
  let stateDyn ← holdDyn initialState stateEvent

  -- Create dynamics
  let selectedIndexDyn ← holdDyn 0 indexEvent
  let initialItem := if h : 0 < items.size then some items[0] else none
  let selectedItemDyn ← holdDyn initialItem itemEvent
  let initialRange := initialState.visibleRange items.size config.visibleRows
  let visibleRangeDyn ← holdDyn initialRange rangeEvent

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    if items.isEmpty then pure ()
    else
      let state ← stateRef.get
      let ke := kd.event

      -- Handle navigation keys
      let newState ← match ke.code with
        | .up => pure (state.moveUp items.size config.wrapAround |>.adjustScroll config.visibleRows)
        | .down => pure (state.moveDown items.size config.wrapAround |>.adjustScroll config.visibleRows)
        | .char 'k' => pure (state.moveUp items.size config.wrapAround |>.adjustScroll config.visibleRows)
        | .char 'j' => pure (state.moveDown items.size config.wrapAround |>.adjustScroll config.visibleRows)
        | .pageUp => pure (state.pageUp config.visibleRows |>.adjustScroll config.visibleRows)
        | .pageDown => pure (state.pageDown items.size config.visibleRows |>.adjustScroll config.visibleRows)
        | .home => pure (state.moveToFirst.adjustScroll config.visibleRows)
        | .end => pure (state.moveToLast items.size |>.adjustScroll config.visibleRows)
        | .enter =>
          if h : state.selected < items.size then
            fireSelect state.selected
          pure state
        | .space =>
          if h : state.selected < items.size then
            fireSelect state.selected
          pure state
        | _ => pure state

      -- Update state and fire events if selection changed
      if newState.selected != state.selected then
        stateRef.set newState
        fireState newState
        fireIndex newState.selected
        let newRange := newState.visibleRange items.size config.visibleRows
        fireRange newRange
        if h : newState.selected < items.size then
          fireItem (some items[newState.selected])
          fireChange items[newState.selected]
        else
          fireItem none
      else if newState.scrollOffset != state.scrollOffset then
        stateRef.set newState
        fireState newState
        let newRange := newState.visibleRange items.size config.visibleRows
        fireRange newRange

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
def dynVirtualList' [ToString α] [BEq α] (name : String)
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

  -- Create trigger events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Nat)
  let (changeEvent, fireChange) ← newTriggerEvent (t := Spider) (a := α)

  -- Get initial items to determine initial state
  let initialItems ← SpiderM.liftIO items.sample
  let initialState : VirtualListState := (VirtualListState.mk 0 0).adjustScroll config.visibleRows

  -- Create a trigger event for state updates
  let (stateUpdateEvent, fireStateUpdate) ← newTriggerEvent (t := Spider) (a := VirtualListState)

  -- Build stateDyn from the state update events
  let stateDyn ← holdDyn initialState stateUpdateEvent

  -- Attach items to key events to compute new state
  let keyEventsWithItems ← Event.attachWithM (fun (currentItems : Array α) (kd : KeyData) =>
    (currentItems, kd)) items.current keyEvents

  -- Attach current state to key events to compute state transitions
  let keyEventsWithState ← Event.attachWithM
    (fun (state : VirtualListState) (itemsAndKey : Array α × KeyData) => (state, itemsAndKey))
    stateDyn.current keyEventsWithItems

  -- Subscribe to key events to compute and fire new state
  let _keyUnsub ← SpiderM.liftIO <| keyEventsWithState.subscribe fun (state, (currentItems, kd)) => do
    if currentItems.isEmpty then pure ()
    else
      let ke := kd.event
      let newState := match ke.code with
        | .up => state.moveUp currentItems.size config.wrapAround |>.adjustScroll config.visibleRows
        | .down => state.moveDown currentItems.size config.wrapAround |>.adjustScroll config.visibleRows
        | .char 'k' => state.moveUp currentItems.size config.wrapAround |>.adjustScroll config.visibleRows
        | .char 'j' => state.moveDown currentItems.size config.wrapAround |>.adjustScroll config.visibleRows
        | .pageUp => state.pageUp config.visibleRows |>.adjustScroll config.visibleRows
        | .pageDown => state.pageDown currentItems.size config.visibleRows |>.adjustScroll config.visibleRows
        | .home => state.moveToFirst.adjustScroll config.visibleRows
        | .end => state.moveToLast currentItems.size |>.adjustScroll config.visibleRows
        | .enter => state
        | .space => state
        | _ => state

      -- Handle enter/space selection
      match ke.code with
      | .enter | .space =>
        if h : state.selected < currentItems.size then
          fireSelect state.selected
          fireChange currentItems[state.selected]
      | _ => pure ()

      if newState.selected != state.selected || newState.scrollOffset != state.scrollOffset then
        fireStateUpdate newState

  -- Attach current state to item changes to clamp selection
  let itemChangesWithState ← Event.attachWithM
    (fun (state : VirtualListState) (newItems : Array α) => (state, newItems))
    stateDyn.current items.updated

  -- Subscribe to item changes to adjust selection
  let _itemUnsub ← SpiderM.liftIO <| itemChangesWithState.subscribe fun (state, newItems) => do
    if state.selected >= newItems.size then
      let newSelected := if newItems.isEmpty then 0 else newItems.size - 1
      let newState := { state with selected := newSelected }.adjustScroll config.visibleRows
      fireStateUpdate newState

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
