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

  -- Create trigger events
  let (tabChangeEvent, fireTabChange) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Nat)

  -- Track internal state
  let initialTab := if initial < labels.size then initial else 0
  let tabRef ← SpiderM.liftIO (IO.mkRef initialTab)

  -- Create dynamics
  let activeTabDyn ← holdDyn initialTab indexEvent

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    if labels.isEmpty then pure ()
    else
      let currentTab ← tabRef.get
      let ke := kd.event

      -- Handle navigation keys
      let newTab ← match ke.code with
        | .left =>
          if currentTab == 0 then
            if config.wrapAround then pure (labels.size - 1) else pure 0
          else
            pure (currentTab - 1)
        | .right =>
          if currentTab >= labels.size - 1 then
            if config.wrapAround then pure 0 else pure (labels.size - 1)
          else
            pure (currentTab + 1)
        | .home => pure 0
        | .end => pure (labels.size - 1)
        | .char c =>
          -- Number key shortcuts (1-9)
          if config.showNumbers && c >= '1' && c <= '9' then
            let idx := c.toNat - '1'.toNat
            if idx < labels.size then pure idx else pure currentTab
          else
            pure currentTab
        | _ => pure currentTab

      -- Update state and fire events if changed
      if newTab != currentTab then
        tabRef.set newTab
        fireIndex newTab
        fireTabChange newTab

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
    The tabs update when labels change, preserving selection where possible. -/
def dynTabs' (labels : Reactive.Dynamic Spider (Array String)) (initial : Nat := 0)
    (config : TabsConfig := {}) : WidgetM TabsResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "dynTabs" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the tab bar's focus name before calling useFocusedKeyEventsW
  let tabsName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW tabsName config.globalKeys

  -- Create trigger events
  let (tabChangeEvent, fireTabChange) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Nat)

  -- Track internal state
  let initialLabels ← SpiderM.liftIO labels.sample
  let initialTab := if initial < initialLabels.size then initial else 0
  let tabRef ← SpiderM.liftIO (IO.mkRef initialTab)

  -- Create dynamics
  let activeTabDyn ← holdDyn initialTab indexEvent

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    let currentLabels ← labels.sample

    if currentLabels.isEmpty then pure ()
    else
      let currentTab ← tabRef.get
      let ke := kd.event

      let newTab ← match ke.code with
        | .left =>
          if currentTab == 0 then
            if config.wrapAround then pure (currentLabels.size - 1) else pure 0
          else
            pure (currentTab - 1)
        | .right =>
          if currentTab >= currentLabels.size - 1 then
            if config.wrapAround then pure 0 else pure (currentLabels.size - 1)
          else
            pure (currentTab + 1)
        | .home => pure 0
        | .end => pure (currentLabels.size - 1)
        | .char c =>
          if config.showNumbers && c >= '1' && c <= '9' then
            let idx := c.toNat - '1'.toNat
            if idx < currentLabels.size then pure idx else pure currentTab
          else
            pure currentTab
        | _ => pure currentTab

      if newTab != currentTab then
        tabRef.set newTab
        fireIndex newTab
        fireTabChange newTab

  -- Subscribe to label changes to adjust selection
  let _unsub2 ← SpiderM.liftIO <| labels.updated.subscribe fun newLabels => do
    let currentTab ← tabRef.get
    if currentTab >= newLabels.size then
      let newTab := if newLabels.isEmpty then 0 else newLabels.size - 1
      tabRef.set newTab
      fireIndex newTab

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
