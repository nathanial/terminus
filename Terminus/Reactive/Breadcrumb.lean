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
def breadcrumb' (_name : String) (items : Array String)
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

  -- Track internal state
  let initialIndex := if items.isEmpty then 0 else items.size - 1  -- Start at last item
  let itemCount := items.size

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    if items.isEmpty then none
    else match kd.event.code with
      | .left => some fun (idx : Nat) =>
          if idx == 0 then
            if config.wrapAround then itemCount - 1 else 0
          else idx - 1
      | .right => some fun (idx : Nat) =>
          if idx >= itemCount - 1 then
            if config.wrapAround then 0 else itemCount - 1
          else idx + 1
      | .home => some fun (_ : Nat) => 0
      | .end => some fun (_ : Nat) => itemCount - 1
      | _ => none) keyEvents

  -- Fold state operations to get index dynamic
  let internalIndexDyn ← foldDyn (fun op state => op state) initialIndex stateOps

  -- Combine focus state with index to get focusedIndex
  let focusedIndexDyn ← internalIndexDyn.zipWith' (fun idx focused =>
    if focused then some idx else none
  ) isFocusedDyn

  -- Filter for Enter key events and attach current index to create navigate event
  let enterEvents ← Event.filterM (fun (kd : KeyData) => kd.event.code == .enter) keyEvents
  let navigateEvent ← Event.attachWithM (fun (idx : Nat) (_ : KeyData) => idx)
    internalIndexDyn.current enterEvents

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
def breadcrumbDyn' (_name : String) (items : Dynamic Spider (Array String))
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

  -- Get initial items to determine initial state
  let initialItems ← SpiderM.liftIO items.sample
  let initialIndex := if initialItems.isEmpty then 0 else initialItems.size - 1

  -- Attach items to key events to get item count for navigation
  let keyEventsWithItems ← Event.attachWithM (fun (currentItems : Array String) (kd : KeyData) =>
    (currentItems, kd)) items.current keyEvents

  -- Map key events to state transformation functions (with item count context)
  let keyStateOps ← Event.mapMaybeM (fun ((currentItems, kd) : Array String × KeyData) =>
    if currentItems.isEmpty then none
    else
      let itemCount := currentItems.size
      match kd.event.code with
        | .left => some fun (idx : Nat) =>
            if idx == 0 then
              if config.wrapAround then itemCount - 1 else 0
            else idx - 1
        | .right => some fun (idx : Nat) =>
            if idx >= itemCount - 1 then
              if config.wrapAround then 0 else itemCount - 1
            else idx + 1
        | .home => some fun (_ : Nat) => 0
        | .end => some fun (_ : Nat) => itemCount - 1
        | _ => none) keyEventsWithItems

  -- Map item changes to clamping operations (when items shrink)
  let itemClampOps ← Event.mapM (fun (newItems : Array String) =>
    fun (idx : Nat) =>
      if idx >= newItems.size then
        if newItems.isEmpty then 0 else newItems.size - 1
      else idx) items.updated

  -- Merge key ops and item clamp ops (key ops take precedence via leftmost)
  let allStateOps ← Event.leftmostM [keyStateOps, itemClampOps]

  -- Fold all state operations to get index dynamic
  let internalIndexDyn ← foldDyn (fun op state => op state) initialIndex allStateOps

  -- Combine focus state with index to get focusedIndex
  let focusedIndexDyn ← internalIndexDyn.zipWith' (fun idx focused =>
    if focused then some idx else none
  ) isFocusedDyn

  -- Filter for Enter key events and attach current index to create navigate event
  let enterEvents ← Event.filterM (fun ((_, kd) : Array String × KeyData) =>
    kd.event.code == .enter) keyEventsWithItems
  let navigateEvent ← Event.attachWithM (fun (idx : Nat) (_ : Array String × KeyData) => idx)
    internalIndexDyn.current enterEvents

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
