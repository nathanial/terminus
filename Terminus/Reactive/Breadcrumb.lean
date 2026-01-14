/-
  Terminus Reactive - Breadcrumb Component
  Navigation path trail widget showing hierarchy.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Breadcrumb Configuration -/

/-- Configuration for breadcrumb appearance and behavior. -/
structure BreadcrumbConfig where
  /-- Separator between items (e.g., " > ", " / "). -/
  separator : String := " > "
  /-- Style for non-active, non-focused items. -/
  itemStyle : Style := { fg := .ansi .blue }
  /-- Style for the last item (current location). -/
  activeStyle : Style := { fg := .ansi .white }
  /-- Style for the currently focused item. -/
  focusedStyle : Style := { fg := .ansi .cyan, modifier := { underline := true } }
  /-- Style for the separator text. -/
  separatorStyle : Style := { fg := .ansi .brightBlack }
  /-- Whether to wrap around at boundaries when navigating. -/
  wrapAround : Bool := true
  /-- Focus name for this breadcrumb. Empty = auto-generated. -/
  focusName : String := ""
  /-- Whether breadcrumb responds to keys without focus (legacy behavior). -/
  globalKeys : Bool := false
  deriving Repr, Inhabited

/-! ## Breadcrumb Result -/

/-- Result returned by breadcrumb containing reactive values and events. -/
structure BreadcrumbResult where
  /-- Currently focused item index (None if widget not focused). -/
  focusedIndex : Reactive.Dynamic Spider (Option Nat)
  /-- Event fired when an item is selected (Enter pressed). -/
  onNavigate : Reactive.Event Spider Nat

/-! ## Breadcrumb Widget -/

/-- Create a breadcrumb navigation trail.
    The last item is considered "active" (current location).

    The widget handles:
    - Left/Right arrow key navigation between items
    - Home/End keys for first/last item
    - Enter triggers onNavigate event with the focused index

    Example:
    ```
    let bc ← breadcrumb' "nav" #["Home", "Products", "Electronics", "Phones"] {}
    -- Use bc.focusedIndex to track focused item
    -- Use bc.onNavigate to handle navigation clicks
    ```
-/
def breadcrumb' (name : String) (items : Array String)
    (config : BreadcrumbConfig := {}) : WidgetM BreadcrumbResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "breadcrumb" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the breadcrumb's focus name
  let breadcrumbName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW breadcrumbName config.globalKeys

  -- Check if this widget is focused
  let isFocusedDyn ← useIsFocused breadcrumbName

  -- Create trigger events
  let (navigateEvent, fireNavigate) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Nat)

  -- Track internal state
  let initialIndex := if items.isEmpty then 0 else items.size - 1  -- Start at last item
  let indexRef ← SpiderM.liftIO (IO.mkRef initialIndex)

  -- Create dynamics
  let internalIndexDyn ← holdDyn initialIndex indexEvent

  -- Combine focus state with index to get focusedIndex
  let focusedIndexDyn ← internalIndexDyn.zipWith' (fun idx focused =>
    if focused then some idx else none
  ) isFocusedDyn

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    if items.isEmpty then pure ()
    else
      let currentIndex ← indexRef.get
      let ke := kd.event

      -- Handle navigation keys
      let newIndex ← match ke.code with
        | .left =>
          if currentIndex == 0 then
            if config.wrapAround then pure (items.size - 1) else pure 0
          else
            pure (currentIndex - 1)
        | .right =>
          if currentIndex >= items.size - 1 then
            if config.wrapAround then pure 0 else pure (items.size - 1)
          else
            pure (currentIndex + 1)
        | .home => pure 0
        | .end => pure (items.size - 1)
        | .enter =>
          -- Fire navigation event and keep index
          fireNavigate currentIndex
          pure currentIndex
        | _ => pure currentIndex

      -- Update state if changed
      if newIndex != currentIndex then
        indexRef.set newIndex
        fireIndex newIndex

  -- Render the breadcrumb
  let node ← internalIndexDyn.zipWith' (fun currentIndex focused =>
    Id.run do
      if items.isEmpty then
        return RNode.text "(no items)" config.itemStyle
      else
        let idx := if currentIndex >= items.size then
          if items.isEmpty then 0 else items.size - 1
        else
          currentIndex

        let mut nodes : Array RNode := #[]

        for h : i in [:items.size] do
          let item := items[i]
          let isActive := i == items.size - 1  -- Last item is active
          let isFocused := focused && i == idx

          if i > 0 then
            nodes := nodes.push (RNode.text config.separator config.separatorStyle)

          let itemStyle :=
            if isFocused then config.focusedStyle
            else if isActive then config.activeStyle
            else config.itemStyle

          nodes := nodes.push (RNode.text item itemStyle)

        return RNode.row 0 {} nodes
  ) isFocusedDyn
  emit node

  pure {
    focusedIndex := focusedIndexDyn
    onNavigate := navigateEvent
  }

/-- Create breadcrumb from a Dynamic array (for changing paths).
    The breadcrumb updates when items change, preserving selection where possible.

    Example:
    ```
    let pathDyn : Dynamic Spider (Array String) := ...
    let bc ← breadcrumbDyn' "nav" pathDyn {}
    ```
-/
def breadcrumbDyn' (name : String) (items : Dynamic Spider (Array String))
    (config : BreadcrumbConfig := {}) : WidgetM BreadcrumbResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "breadcrumb" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the breadcrumb's focus name
  let breadcrumbName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW breadcrumbName config.globalKeys

  -- Check if this widget is focused
  let isFocusedDyn ← useIsFocused breadcrumbName

  -- Create trigger events
  let (navigateEvent, fireNavigate) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexUpdateEvent, fireIndexUpdate) ← newTriggerEvent (t := Spider) (a := Nat)

  -- Get initial items to determine initial state
  let initialItems ← SpiderM.liftIO items.sample
  let initialIndex := if initialItems.isEmpty then 0 else initialItems.size - 1

  -- Build index dynamic from update events
  let internalIndexDyn ← holdDyn initialIndex indexUpdateEvent

  -- Combine focus state with index to get focusedIndex
  let focusedIndexDyn ← internalIndexDyn.zipWith' (fun idx focused =>
    if focused then some idx else none
  ) isFocusedDyn

  -- Attach items to key events
  let keyEventsWithItems ← Event.attachWithM (fun (currentItems : Array String) (kd : KeyData) =>
    (currentItems, kd)) items.current keyEvents

  -- Attach current index to key events
  let keyEventsWithState ← Event.attachWithM
    (fun (currentIndex : Nat) (itemsAndKey : Array String × KeyData) => (currentIndex, itemsAndKey))
    internalIndexDyn.current keyEventsWithItems

  -- Subscribe to key events
  let _keyUnsub ← SpiderM.liftIO <| keyEventsWithState.subscribe fun (currentIndex, (currentItems, kd)) => do
    if currentItems.isEmpty then pure ()
    else
      let itemCount := currentItems.size
      let ke := kd.event
      let (newIndex, shouldNavigate) := match ke.code with
        | .left =>
          if currentIndex == 0 then
            if config.wrapAround then (itemCount - 1, false) else (0, false)
          else
            (currentIndex - 1, false)
        | .right =>
          if currentIndex >= itemCount - 1 then
            if config.wrapAround then (0, false) else (itemCount - 1, false)
          else
            (currentIndex + 1, false)
        | .home => (0, false)
        | .end => (itemCount - 1, false)
        | .enter => (currentIndex, true)
        | _ => (currentIndex, false)

      if shouldNavigate then
        fireNavigate currentIndex
      if newIndex != currentIndex then
        fireIndexUpdate newIndex

  -- Attach current index to item changes
  let itemChangesWithState ← Event.attachWithM
    (fun (currentIndex : Nat) (newItems : Array String) => (currentIndex, newItems))
    internalIndexDyn.current items.updated

  -- Subscribe to item changes to clamp index
  let _itemUnsub ← SpiderM.liftIO <| itemChangesWithState.subscribe fun (currentIndex, newItems) => do
    if currentIndex >= newItems.size then
      let newIndex := if newItems.isEmpty then 0 else newItems.size - 1
      fireIndexUpdate newIndex

  -- Render the breadcrumb
  let node ← internalIndexDyn.zipWith3' (fun currentIndex currentItems focused =>
    Id.run do
      if currentItems.isEmpty then
        return RNode.text "(no items)" config.itemStyle
      else
        let idx := if currentIndex >= currentItems.size then
          if currentItems.isEmpty then 0 else currentItems.size - 1
        else
          currentIndex

        let mut nodes : Array RNode := #[]

        for h : i in [:currentItems.size] do
          let item := currentItems[i]
          let isActive := i == currentItems.size - 1  -- Last item is active
          let isFocused := focused && i == idx

          if i > 0 then
            nodes := nodes.push (RNode.text config.separator config.separatorStyle)

          let itemStyle :=
            if isFocused then config.focusedStyle
            else if isActive then config.activeStyle
            else config.itemStyle

          nodes := nodes.push (RNode.text item itemStyle)

        return RNode.row 0 {} nodes
  ) items isFocusedDyn
  emit node

  pure {
    focusedIndex := focusedIndexDyn
    onNavigate := navigateEvent
  }

end Terminus.Reactive
