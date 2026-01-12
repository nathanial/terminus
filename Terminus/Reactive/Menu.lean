/-
  Terminus Reactive - Menu Widget
  Dropdown/popup menu with keyboard navigation and submenus.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Menu Item -/

/-- Menu item definition. -/
structure MenuItem' where
  /-- Display label. -/
  label : String
  /-- Optional keyboard shortcut to display. -/
  shortcut : Option String := none
  /-- Whether the item is enabled. -/
  enabled : Bool := true
  /-- Custom style for this item. -/
  style : Style := {}
  /-- Nested submenu items. -/
  submenu : Array MenuItem' := #[]
  /-- Whether this is a separator line. -/
  isSeparator : Bool := false
  deriving Repr, Inhabited

namespace MenuItem'

/-- Create a simple menu item. -/
def new (label : String) : MenuItem' := { label }

/-- Create a separator. -/
def separator : MenuItem' := { label := "", isSeparator := true }

/-- Create a disabled menu item. -/
def disabled (label : String) : MenuItem' := { label, enabled := false }

/-- Add a keyboard shortcut. -/
def withShortcut (item : MenuItem') (s : String) : MenuItem' :=
  { item with shortcut := some s }

/-- Add a submenu. -/
def withSubmenu (item : MenuItem') (items : Array MenuItem') : MenuItem' :=
  { item with submenu := items }

/-- Check if item has a submenu. -/
def hasSubmenu (item : MenuItem') : Bool := !item.submenu.isEmpty

end MenuItem'

/-! ## Menu Configuration -/

/-- Configuration for menu appearance and behavior. -/
structure MenuConfig where
  /-- Style for highlighted (selected) items. -/
  highlightStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Style for normal items. -/
  normalStyle : Style := {}
  /-- Style for disabled items. -/
  disabledStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for keyboard shortcuts. -/
  shortcutStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for separators. -/
  separatorStyle : Style := { fg := .ansi .brightBlack }
  /-- Character used for separators. -/
  separatorChar : String := "─"
  /-- Indicator for submenus. -/
  submenuIndicator : String := "▶"
  /-- Padding on each side of menu items. -/
  padding : Nat := 1
  /-- Focus name for keyboard input. -/
  focusName : String := "menu"
  /-- Whether to use global key handling (ignores focus). -/
  globalKeys : Bool := false
  deriving Repr, Inhabited

/-! ## Menu State -/

/-- Internal menu state. -/
structure MenuState where
  /-- Selected index in current menu level. -/
  selectedIndex : Nat := 0
  /-- Stack of open submenu indices. -/
  openPath : Array Nat := #[]
  deriving Repr, Inhabited

namespace MenuState

/-- Get the currently selected item index. -/
def currentIndex (state : MenuState) : Nat := state.selectedIndex

/-- Navigate to next enabled item. -/
def navigateNext (state : MenuState) (items : Array MenuItem') (wrap : Bool := true) : MenuState :=
  if items.isEmpty then state
  else
    let count := items.size
    let start := state.selectedIndex
    let rec findNext (idx : Nat) (steps : Nat) : Nat :=
      if steps >= count then start
      else
        let nextIdx := if wrap then (idx + 1) % count
                       else min (idx + 1) (count - 1)
        match items[nextIdx]? with
        | some item =>
          if item.enabled && !item.isSeparator then nextIdx
          else if nextIdx == start then start
          else findNext nextIdx (steps + 1)
        | none => start
    { state with selectedIndex := findNext start 0 }

/-- Navigate to previous enabled item. -/
def navigatePrev (state : MenuState) (items : Array MenuItem') (wrap : Bool := true) : MenuState :=
  if items.isEmpty then state
  else
    let count := items.size
    let start := state.selectedIndex
    let rec findPrev (idx : Nat) (steps : Nat) : Nat :=
      if steps >= count then start
      else
        let prevIdx := if wrap then (idx + count - 1) % count
                       else if idx == 0 then 0 else idx - 1
        match items[prevIdx]? with
        | some item =>
          if item.enabled && !item.isSeparator then prevIdx
          else if prevIdx == start then start
          else findPrev prevIdx (steps + 1)
        | none => start
    { state with selectedIndex := findPrev start 0 }

/-- Open submenu at current selection. -/
def openSubmenu (state : MenuState) : MenuState :=
  { state with
    openPath := state.openPath.push state.selectedIndex
    selectedIndex := 0 }

/-- Close current submenu. -/
def closeSubmenu (state : MenuState) : MenuState :=
  match state.openPath.back? with
  | some idx => { state with
      openPath := state.openPath.pop
      selectedIndex := idx }
  | none => state

/-- Check if a submenu is open. -/
def hasOpenSubmenu (state : MenuState) : Bool := !state.openPath.isEmpty

end MenuState

/-! ## Menu Result -/

/-- Result returned by menu widget. -/
structure MenuResult where
  /-- Currently selected item path as indices. -/
  selectedPath : Reactive.Dynamic Spider (Array Nat)
  /-- Currently selected item label. -/
  selectedLabel : Reactive.Dynamic Spider (Option String)
  /-- Event fired when an item is selected (confirmed). -/
  onSelect : Reactive.Event Spider (Array Nat × String)
  /-- Event fired when menu is cancelled. -/
  onCancel : Reactive.Event Spider Unit

/-! ## Menu Widget -/

/-- Get the item at a path through nested menus. -/
private def getItemAtPath (items : Array MenuItem') (path : Array Nat) : Option MenuItem' := Id.run do
  if path.isEmpty then return none
  let mut current := items
  let mut result : Option MenuItem' := none
  for i in [:path.size] do
    match path[i]? with
    | some idx =>
      match current[idx]? with
      | some item =>
        result := some item
        current := item.submenu
      | none => return none
    | none => return none
  return result

/-- Get current menu items based on open path. -/
private def getCurrentItems (items : Array MenuItem') (openPath : Array Nat) : Array MenuItem' := Id.run do
  if openPath.isEmpty then return items
  let mut current := items
  for idx in openPath do
    match current[idx]? with
    | some item => current := item.submenu
    | none => return #[]
  return current

/-- Render a menu item. -/
private def renderMenuItem (item : MenuItem') (isSelected : Bool) (config : MenuConfig) (width : Nat) : RNode := Id.run do
  if item.isSeparator then
    let sep := String.ofList (List.replicate width (config.separatorChar.toList.headD '─'))
    return RNode.text sep config.separatorStyle

  let baseStyle := if item.enabled then config.normalStyle else config.disabledStyle
  let rowStyle := if isSelected then Style.merge baseStyle config.highlightStyle else baseStyle

  let mut parts : Array RNode := #[]

  -- Left padding
  parts := parts.push (RNode.text (String.ofList (List.replicate config.padding ' ')) rowStyle)

  -- Label
  parts := parts.push (RNode.text item.label rowStyle)

  -- Calculate right side content
  let shortcutLen := match item.shortcut with | some s => s.length + 2 | none => 0
  let indicatorLen := if item.hasSubmenu then config.submenuIndicator.length + 1 else 0
  let labelLen := item.label.length
  let usedLen := config.padding * 2 + labelLen + shortcutLen + indicatorLen

  -- Gap between label and right side
  let gap := if width > usedLen then width - usedLen else 0
  if gap > 0 then
    parts := parts.push (RNode.text (String.ofList (List.replicate gap ' ')) rowStyle)

  -- Shortcut
  match item.shortcut with
  | some s =>
    let scStyle := if isSelected then rowStyle else Style.merge rowStyle config.shortcutStyle
    parts := parts.push (RNode.text s!" {s}" scStyle)
  | none => pure ()

  -- Submenu indicator
  if item.hasSubmenu then
    parts := parts.push (RNode.text s!" {config.submenuIndicator}" rowStyle)

  -- Right padding
  parts := parts.push (RNode.text (String.ofList (List.replicate config.padding ' ')) rowStyle)

  return RNode.row 0 {} parts

/-- Create a menu widget.

    The menu supports keyboard navigation:
    - Up/Down arrows: Navigate items
    - Right arrow/Enter: Open submenu or select
    - Left arrow/Escape: Close submenu or cancel

    Example:
    ```
    let items := #[
      MenuItem'.new "File" |>.withSubmenu #[
        MenuItem'.new "New" |>.withShortcut "Ctrl+N",
        MenuItem'.new "Open" |>.withShortcut "Ctrl+O",
        MenuItem'.separator,
        MenuItem'.new "Exit"
      ],
      MenuItem'.new "Edit",
      MenuItem'.disabled "View"
    ]
    let menu ← menu' "main-menu" items {}
    ```
-/
def menu' (name : String) (items : Array MenuItem') (config : MenuConfig := {})
    : WidgetM MenuResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW name (isInput := true) (nameOverride := name)

  -- Compute input name for focus handling
  let inputName := if name.isEmpty then widgetName else name

  -- State
  let initialState := MenuState.mk 0 #[]
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := MenuState)
  let stateDyn ← holdDyn initialState stateEvent

  -- Events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Array Nat × String)
  let (cancelEvent, fireCancel) ← newTriggerEvent (t := Spider) (a := Unit)
  let (pathEvent, firePath) ← newTriggerEvent (t := Spider) (a := Array Nat)
  let (labelEvent, fireLabel) ← newTriggerEvent (t := Spider) (a := Option String)

  -- Initial dynamics
  let pathDyn ← holdDyn #[0] pathEvent
  let labelDyn ← holdDyn (items[0]?.map (·.label)) labelEvent

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName config.globalKeys

  let updateState : MenuState → IO Unit := fun newState => do
    stateRef.set newState
    fireState newState

  -- Subscribe to key events (already filtered by focus)
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    let state ← stateRef.get
    let currentItems := getCurrentItems items state.openPath

    match kd.event.code with
    | .down | .char 'j' =>
      let newState := state.navigateNext currentItems
      updateState newState
      let path := newState.openPath.push newState.selectedIndex
      firePath path
      let item := getItemAtPath items path
      fireLabel (item.map (·.label))

    | .up | .char 'k' =>
      let newState := state.navigatePrev currentItems
      updateState newState
      let path := newState.openPath.push newState.selectedIndex
      firePath path
      let item := getItemAtPath items path
      fireLabel (item.map (·.label))

    | .right | .enter =>
      match currentItems[state.selectedIndex]? with
      | some item =>
        if item.enabled && !item.isSeparator then
          if item.hasSubmenu then
            let newState := state.openSubmenu
            updateState newState
            let path := newState.openPath.push newState.selectedIndex
            firePath path
            let subItem := getItemAtPath items path
            fireLabel (subItem.map (·.label))
          else
            let path := state.openPath.push state.selectedIndex
            fireSelect (path, item.label)
      | none => pure ()

    | .left =>
      if state.hasOpenSubmenu then
        let newState := state.closeSubmenu
        updateState newState
        let path := newState.openPath.push newState.selectedIndex
        firePath path
        let item := getItemAtPath items path
        fireLabel (item.map (·.label))

    | .escape =>
      if state.hasOpenSubmenu then
        let newState := state.closeSubmenu
        updateState newState
        let path := newState.openPath.push newState.selectedIndex
        firePath path
        let item := getItemAtPath items path
        fireLabel (item.map (·.label))
      else
        fireCancel ()

    | _ => pure ()

  -- Render
  let node ← stateDyn.map' (fun state =>
    let currentItems := getCurrentItems items state.openPath

    if currentItems.isEmpty then
      RNode.empty
    else
      -- Calculate menu width
      let maxWidth := currentItems.foldl (fun acc item =>
        let labelLen := item.label.length
        let shortcutLen := match item.shortcut with | some s => s.length + 2 | none => 0
        let indicatorLen := if item.hasSubmenu then config.submenuIndicator.length + 1 else 0
        max acc (config.padding * 2 + labelLen + shortcutLen + indicatorLen + 2)
      ) 10

      let nodes := currentItems.mapIdx fun i item =>
        let isSelected := i == state.selectedIndex
        renderMenuItem item isSelected config maxWidth

      RNode.column 0 {} nodes
  )
  emit node

  pure {
    selectedPath := pathDyn
    selectedLabel := labelDyn
    onSelect := selectEvent
    onCancel := cancelEvent
  }

/-! ## Convenience Functions -/

/-- Create a simple dropdown menu. -/
def dropdown' (name : String) (options : Array String) (config : MenuConfig := {})
    : WidgetM MenuResult := do
  let items := options.map MenuItem'.new
  menu' name items config

/-- Create a context menu (usually shown on right-click). -/
def contextMenu' (name : String) (items : Array MenuItem') (config : MenuConfig := {})
    : WidgetM MenuResult :=
  menu' name items { config with globalKeys := true }

end Terminus.Reactive
