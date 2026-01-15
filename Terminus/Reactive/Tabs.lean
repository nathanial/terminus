/-
  Terminus Reactive - Tabs Component
  Tab bar widget with keyboard navigation.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Tabs Configuration -/

/-- Configuration for tabs appearance and behavior. -/
structure TabsConfig where
  /-- Style for inactive tabs. -/
  style : Style := {}
  /-- Style for active tab. -/
  activeStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Separator between tabs. -/
  separator : String := " | "
  /-- Style for separator. -/
  separatorStyle : Style := { fg := .ansi .brightBlack }
  /-- Highlight symbol before active tab. -/
  highlightSymbol : String := ""
  /-- Show number prefixes (1. 2. 3.). -/
  showNumbers : Bool := false
  /-- Whether to wrap around at boundaries. -/
  wrapAround : Bool := true
  /-- Focus name for this tab bar. Empty = auto-generated. -/
  focusName : String := ""
  /-- Whether tabs respond to keys without focus (legacy behavior). -/
  globalKeys : Bool := false
  deriving Repr, Inhabited

/-! ## Tabs Result -/

/-- Result returned by tabs' containing reactive values and events. -/
structure TabsResult where
  /-- Currently active tab index. -/
  activeTab : Reactive.Dynamic Spider Nat
  /-- Event fired when tab changes. -/
  onTabChange : Reactive.Event Spider Nat

/-! ## Tabs Widget -/

/-- Create a tab bar widget.

    The widget handles:
    - Left/Right arrow key navigation
    - Number keys 1-9 for direct selection (if showNumbers or using numberedTabs')
    - Wrapping at boundaries (configurable)

    Example:
    ```
    let tabs ← tabs' #["Home", "Settings", "Help"] 0 {}
    -- Use tabs.activeTab to get current selection
    -- Use tabs.onTabChange to handle tab switches
    ```

    This implementation uses FRP-idiomatic state management via foldDyn,
    avoiding imperative subscribe patterns with IO.mkRef.
-/
def tabs' (labels : Array String) (initial : Nat := 0)
    (config : TabsConfig := {}) : WidgetM TabsResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "tabs" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the tab bar's focus name before calling useFocusedKeyEventsW
  let tabsName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW tabsName config.globalKeys

  -- Initial state
  let initialTab := if initial < labels.size then initial else 0
  let labelCount := labels.size
  let maxIndex := if labelCount > 0 then labelCount - 1 else 0

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun kd =>
    if labels.isEmpty then none
    else match kd.event.code with
    | .left => some fun (idx : Nat) =>
        if idx == 0 then
          if config.wrapAround then maxIndex else 0
        else idx - 1
    | .right => some fun (idx : Nat) =>
        if idx >= maxIndex then
          if config.wrapAround then 0 else maxIndex
        else idx + 1
    | .home => some fun (_ : Nat) => 0
    | .end => some fun (_ : Nat) => maxIndex
    | .char c =>
        -- Number key shortcuts (1-9)
        if config.showNumbers && c >= '1' && c <= '9' then
          let targetIdx := c.toNat - '1'.toNat
          if targetIdx < labelCount then some fun (_ : Nat) => targetIdx
          else none
        else none
    | _ => none) keyEvents

  -- Fold state operations - no subscribe needed!
  let activeTabDyn ← foldDyn (fun op state => op state) initialTab stateOps

  -- Derive change event from state updates
  let tabChangeEvent := activeTabDyn.updated

  let node ← activeTabDyn.map' fun currentTab =>
    Id.run do
      if labels.isEmpty then
        return RNode.text "(no tabs)" config.style
      else
        let tab := if currentTab >= labels.size then
          if labels.isEmpty then 0 else labels.size - 1
        else
          currentTab

        let mut nodes : Array RNode := #[]

        for h : i in [:labels.size] do
          let label := labels[i]
          let isActive := i == tab

          if i > 0 then
            nodes := nodes.push (RNode.text config.separator config.separatorStyle)

          let highlightPfx := if isActive then config.highlightSymbol else ""
          let numPfx := if config.showNumbers then s!"{i + 1}. " else ""
          let tabText := highlightPfx ++ numPfx ++ label
          let tabStyle := if isActive then config.activeStyle else config.style

          nodes := nodes.push (RNode.text tabText tabStyle)

        return RNode.row 0 {} nodes
  emit node

  pure {
    activeTab := activeTabDyn
    onTabChange := tabChangeEvent
  }

/-- Create a numbered tab bar with 1-9 shortcuts.
    Equivalent to tabs' with showNumbers := true. -/
def numberedTabs' (labels : Array String) (initial : Nat := 0)
    (config : TabsConfig := {}) : WidgetM TabsResult :=
  tabs' labels initial { config with showNumbers := true }

/-- Create tabs with a dynamic label array.
    The tabs update when labels change, preserving selection where possible.

    This implementation uses FRP-idiomatic state management via foldDyn,
    avoiding imperative subscribe patterns with IO.mkRef. Key events and
    label changes are merged into a single stream of state operations. -/
def dynTabs' (labels : Reactive.Dynamic Spider (Array String)) (initial : Nat := 0)
    (config : TabsConfig := {}) : WidgetM TabsResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "dynTabs" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the tab bar's focus name before calling useFocusedKeyEventsW
  let tabsName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW tabsName config.globalKeys

  -- Initial tab index (will be clamped at render time if out of bounds)
  -- We use the provided initial value; clamping happens in the render function
  -- and in the label clamp operations when labels change.
  let initialTab := initial

  -- Attach labels to key events so we can compute state ops with current label count
  let keyEventsWithLabels ← Event.attachWithM (fun (currentLabels : Array String) (kd : KeyData) =>
    (currentLabels, kd)) labels.current keyEvents

  -- Map key events (with labels) to state transformation functions
  let keyStateOps ← Event.mapMaybeM (fun (currentLabels, kd) =>
    if currentLabels.isEmpty then none
    else
      let labelCount := currentLabels.size
      let maxIndex := labelCount - 1
      match kd.event.code with
      | .left => some fun (idx : Nat) =>
          if idx == 0 then
            if config.wrapAround then maxIndex else 0
          else idx - 1
      | .right => some fun (idx : Nat) =>
          if idx >= maxIndex then
            if config.wrapAround then 0 else maxIndex
          else idx + 1
      | .home => some fun (_ : Nat) => 0
      | .end => some fun (_ : Nat) => maxIndex
      | .char c =>
          if config.showNumbers && c >= '1' && c <= '9' then
            let targetIdx := c.toNat - '1'.toNat
            if targetIdx < labelCount then some fun (_ : Nat) => targetIdx
            else none
          else none
      | _ => none) keyEventsWithLabels

  -- Map label change events to state clamping operations
  -- When labels change, clamp the current index to be within bounds
  let labelClampOps ← Event.mapMaybeM (fun (newLabels : Array String) =>
    -- Return a function that clamps the index if needed
    some fun (currentIdx : Nat) =>
      if newLabels.isEmpty then 0
      else if currentIdx >= newLabels.size then newLabels.size - 1
      else currentIdx) labels.updated

  -- Merge key operations and label clamp operations into single stream
  let allStateOps ← Event.mergeM keyStateOps labelClampOps

  -- Fold all state operations - no subscribe needed!
  let activeTabDyn ← foldDyn (fun op state => op state) initialTab allStateOps

  -- Derive change event from state updates
  let tabChangeEvent := activeTabDyn.updated

  -- Render the tabs
  let node ← activeTabDyn.zipWith' (fun currentTab currentLabels =>
    Id.run do
      if currentLabels.isEmpty then
        return RNode.text "(no tabs)" config.style
      else
        let tab := if currentTab >= currentLabels.size then
          if currentLabels.isEmpty then 0 else currentLabels.size - 1
        else
          currentTab

        let mut nodes : Array RNode := #[]

        for h : i in [:currentLabels.size] do
          let label := currentLabels[i]
          let isActive := i == tab

          if i > 0 then
            nodes := nodes.push (RNode.text config.separator config.separatorStyle)

          let highlightPfx := if isActive then config.highlightSymbol else ""
          let numPfx := if config.showNumbers then s!"{i + 1}. " else ""
          let tabText := highlightPfx ++ numPfx ++ label
          let tabStyle := if isActive then config.activeStyle else config.style

          nodes := nodes.push (RNode.text tabText tabStyle)

        return RNode.row 0 {} nodes
  ) labels
  emit node

  pure {
    activeTab := activeTabDyn
    onTabChange := tabChangeEvent
  }

end Terminus.Reactive
