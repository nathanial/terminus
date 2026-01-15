/-
  Terminus Reactive - MenuBar Widget
  Horizontal menu bar with dropdown menus for traditional app-style navigation.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## MenuBar Item Types -/

/-- A submenu item within a top-level menu. -/
inductive MenuBarSubItem where
  /-- A selectable menu item with label and optional shortcut. -/
  | item (label : String) (shortcut : Option String := none)
  /-- A horizontal separator line. -/
  | separator
  deriving Repr, Inhabited

namespace MenuBarSubItem

/-- Create a simple menu item. -/
def new (label : String) : MenuBarSubItem := .item label none

/-- Create a menu item with a keyboard shortcut. -/
def withShortcut (label : String) (shortcut : String) : MenuBarSubItem :=
  .item label (some shortcut)

/-- Check if this is a separator. -/
def isSeparator : MenuBarSubItem -> Bool
  | .separator => true
  | .item .. => false

/-- Get the label of an item (empty for separators). -/
def label : MenuBarSubItem -> String
  | .item l _ => l
  | .separator => ""

/-- Get the shortcut of an item (none for separators). -/
def shortcut : MenuBarSubItem -> Option String
  | .item _ s => s
  | .separator => none

end MenuBarSubItem

/-- A top-level menu in the menu bar. -/
structure MenuBarItem where
  /-- Display label for the menu. -/
  label : String
  /-- Submenu items. -/
  items : Array MenuBarSubItem
  deriving Repr, Inhabited

namespace MenuBarItem

/-- Create a menu with the given label and items. -/
def new (label : String) (items : Array MenuBarSubItem) : MenuBarItem :=
  { label, items }

end MenuBarItem

/-! ## MenuBar Configuration -/

/-- Configuration for menu bar appearance. -/
structure MenuBarConfig where
  /-- Style for inactive top-level menu labels. -/
  style : Style := {}
  /-- Style for the focused/highlighted top-level menu label. -/
  activeStyle : Style := { fg := .ansi .black, bg := .ansi .white }
  /-- Style for the dropdown menu items. -/
  menuStyle : Style := {}
  /-- Style for the selected item in the dropdown. -/
  selectedStyle : Style := { fg := .ansi .black, bg := .ansi .cyan }
  /-- Character/string used for separators in the dropdown. -/
  separatorChar : String := "--------"
  /-- Style for keyboard shortcuts. -/
  shortcutStyle : Style := { fg := .ansi .brightBlack }
  /-- Gap between top-level menu labels. -/
  gap : Nat := 2
  /-- Border style for dropdown menus. -/
  borderStyle : Style := {}
  /-- Focus name for keyboard input. -/
  focusName : String := ""
  /-- Whether to use global key handling (ignores focus). -/
  globalKeys : Bool := false
  deriving Inhabited

/-! ## MenuBar Result -/

/-- Result returned by menuBar' containing reactive values and events. -/
structure MenuBarResult where
  /-- Which top-level menu is currently open (none if all closed). -/
  activeMenu : Reactive.Dynamic Spider (Option Nat)
  /-- Event fired when an item is selected: (menuIndex, itemIndex). -/
  onSelect : Reactive.Event Spider (Nat × Nat)
  /-- Event fired when an item is selected, containing the label. -/
  onSelectLabel : Reactive.Event Spider String

/-! ## MenuBar State -/

/-- Internal state for the menu bar. -/
private structure MenuBarState where
  /-- Currently focused top-level menu index. -/
  focusedMenu : Nat := 0
  /-- Currently open menu (none if closed). -/
  openMenu : Option Nat := none
  /-- Selected item index within the open menu. -/
  selectedItem : Nat := 0
  deriving Repr, Inhabited, BEq

namespace MenuBarState

/-- Move focus to the next top-level menu. -/
def focusNext (s : MenuBarState) (menuCount : Nat) (wrap : Bool := true) : MenuBarState :=
  if menuCount == 0 then s
  else if s.focusedMenu >= menuCount - 1 then
    if wrap then { s with focusedMenu := 0 } else s
  else
    { s with focusedMenu := s.focusedMenu + 1 }

/-- Move focus to the previous top-level menu. -/
def focusPrev (s : MenuBarState) (menuCount : Nat) (wrap : Bool := true) : MenuBarState :=
  if menuCount == 0 then s
  else if s.focusedMenu == 0 then
    if wrap then { s with focusedMenu := menuCount - 1 } else s
  else
    { s with focusedMenu := s.focusedMenu - 1 }

/-- Open the currently focused menu. -/
def openCurrentMenu (s : MenuBarState) : MenuBarState :=
  { s with openMenu := some s.focusedMenu, selectedItem := 0 }

/-- Close the open menu. -/
def closeMenu (s : MenuBarState) : MenuBarState :=
  { s with openMenu := none }

/-- Check if a menu is open. -/
def isOpen (s : MenuBarState) : Bool := s.openMenu.isSome

/-- Find the next selectable item index (skipping separators). -/
def findNextItem (items : Array MenuBarSubItem) (current : Nat) (wrap : Bool := true) : Nat :=
  if items.isEmpty then 0
  else
    let count := items.size
    let rec go (idx : Nat) (steps : Nat) : Nat :=
      if steps >= count then current
      else
        let nextIdx := if wrap then (idx + 1) % count
                       else min (idx + 1) (count - 1)
        match items[nextIdx]? with
        | some item =>
          if !item.isSeparator then nextIdx
          else if nextIdx == current then current
          else go nextIdx (steps + 1)
        | none => current
    go current 0

/-- Find the previous selectable item index (skipping separators). -/
def findPrevItem (items : Array MenuBarSubItem) (current : Nat) (wrap : Bool := true) : Nat :=
  if items.isEmpty then 0
  else
    let count := items.size
    let rec go (idx : Nat) (steps : Nat) : Nat :=
      if steps >= count then current
      else
        let prevIdx := if wrap then (idx + count - 1) % count
                       else if idx == 0 then 0 else idx - 1
        match items[prevIdx]? with
        | some item =>
          if !item.isSeparator then prevIdx
          else if prevIdx == current then current
          else go prevIdx (steps + 1)
        | none => current
    go current 0

/-- Find the first selectable item in a menu. -/
def findFirstSelectableItem (items : Array MenuBarSubItem) : Nat :=
  match items.findIdx? (!·.isSeparator) with
  | some idx => idx
  | none => 0

/-- Move selection down in the open menu. -/
def selectNext (s : MenuBarState) (items : Array MenuBarSubItem) : MenuBarState :=
  { s with selectedItem := findNextItem items s.selectedItem }

/-- Move selection up in the open menu. -/
def selectPrev (s : MenuBarState) (items : Array MenuBarSubItem) : MenuBarState :=
  { s with selectedItem := findPrevItem items s.selectedItem }

/-- Switch to the next menu (when dropdown is open). -/
def switchToNextMenu (s : MenuBarState) (menus : Array MenuBarItem) : MenuBarState :=
  if menus.isEmpty then s
  else
    let nextMenu := if s.focusedMenu >= menus.size - 1 then 0 else s.focusedMenu + 1
    let firstItem := match menus[nextMenu]? with
      | some m => findFirstSelectableItem m.items
      | none => 0
    { s with
      focusedMenu := nextMenu
      openMenu := some nextMenu
      selectedItem := firstItem }

/-- Switch to the previous menu (when dropdown is open). -/
def switchToPrevMenu (s : MenuBarState) (menus : Array MenuBarItem) : MenuBarState :=
  if menus.isEmpty then s
  else
    let prevMenu := if s.focusedMenu == 0 then menus.size - 1 else s.focusedMenu - 1
    let firstItem := match menus[prevMenu]? with
      | some m => findFirstSelectableItem m.items
      | none => 0
    { s with
      focusedMenu := prevMenu
      openMenu := some prevMenu
      selectedItem := firstItem }

end MenuBarState

/-! ## MenuBar Rendering Helpers -/

/-- Calculate the width needed for a dropdown menu. -/
private def calcMenuWidth (items : Array MenuBarSubItem) (separatorLen : Nat) : Nat :=
  items.foldl (fun acc item =>
    match item with
    | .separator => max acc separatorLen
    | .item label shortcut =>
      let labelLen := label.length
      let shortcutLen := match shortcut with
        | some s => s.length + 2  -- space + shortcut
        | none => 0
      max acc (labelLen + shortcutLen + 4)  -- padding on both sides
  ) 10

/-- Render a single dropdown menu item. -/
private def renderDropdownItem (item : MenuBarSubItem) (isSelected : Bool)
    (config : MenuBarConfig) (width : Nat) : RNode :=
  match item with
  | .separator =>
    -- Render separator line
    let sepLine := config.separatorChar
    let padding := if width > sepLine.length then (width - sepLine.length) / 2 else 0
    let paddedSep := String.ofList (List.replicate padding ' ') ++ sepLine
    RNode.text paddedSep config.shortcutStyle
  | .item label shortcut =>
    let baseStyle := if isSelected then config.selectedStyle else config.menuStyle
    -- Build the item content
    let shortcutStr := match shortcut with
      | some s => s!" {s}"
      | none => ""
    let contentLen := label.length + shortcutStr.length
    let gap := if width > contentLen + 2 then width - contentLen - 2 else 1
    let gapStr := String.ofList (List.replicate gap ' ')
    let text := s!" {label}{gapStr}{shortcutStr} "
    RNode.text text baseStyle

/-- Render the dropdown menu box for a given menu. -/
private def renderDropdown (menu : MenuBarItem) (selectedItem : Nat)
    (config : MenuBarConfig) : RNode :=
  if menu.items.isEmpty then
    RNode.empty
  else
    let width := calcMenuWidth menu.items config.separatorChar.length
    -- Top border
    let topBorder := "╭" ++ String.ofList (List.replicate width '─') ++ "╮"
    let bottomBorder := "╰" ++ String.ofList (List.replicate width '─') ++ "╯"

    -- Build item rows
    let itemRows := menu.items.mapIdx fun i item =>
      let isSelected := i == selectedItem
      let itemNode := renderDropdownItem item isSelected config width
      -- Wrap in side borders
      RNode.row 0 {} #[
        RNode.text "│" config.borderStyle,
        itemNode,
        RNode.text "│" config.borderStyle
      ]

    let rows := #[RNode.text topBorder config.borderStyle] ++ itemRows ++ #[RNode.text bottomBorder config.borderStyle]
    RNode.column 0 {} rows

/-! ## MenuBar Widget -/

/-- Create a menu bar widget with dropdown menus.

    The menu bar displays top-level menu labels horizontally. When focused:
    - Left/Right arrows navigate between top-level menus
    - Enter or Down opens the dropdown for the focused menu
    - When a dropdown is open:
      - Up/Down navigates items (skipping separators)
      - Left/Right switches to adjacent menus
      - Enter selects the current item
      - Escape closes the dropdown

    Visual example:
    ```
    File  Edit  View  Help
    ╭──────────╮
    │ New      │
    │ Open     │
    │ Save     │
    │ ──────── │
    │ Exit     │
    ╰──────────╯
    ```

    Example usage:
    ```
    let menus := #[
      MenuBarItem.new "File" #[
        MenuBarSubItem.withShortcut "New" "Ctrl+N",
        MenuBarSubItem.withShortcut "Open" "Ctrl+O",
        MenuBarSubItem.withShortcut "Save" "Ctrl+S",
        MenuBarSubItem.separator,
        MenuBarSubItem.new "Exit"
      ],
      MenuBarItem.new "Edit" #[
        MenuBarSubItem.withShortcut "Cut" "Ctrl+X",
        MenuBarSubItem.withShortcut "Copy" "Ctrl+C",
        MenuBarSubItem.withShortcut "Paste" "Ctrl+V"
      ]
    ]
    let menuBar ← menuBar' "main" menus {}
    -- menuBar.onSelectLabel fires with the label when an item is selected
    ```
-/
def menuBar' (name : String) (menus : Array MenuBarItem) (config : MenuBarConfig := {})
    : WidgetM MenuBarResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "menuBar" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- State management using idiomatic FRP patterns
  let initialState : MenuBarState := {}
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := MenuBarState)
  let stateDyn ← holdDyn initialState stateEvent

  -- Events for results
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Nat × Nat)
  let (selectLabelEvent, fireSelectLabel) ← newTriggerEvent (t := Spider) (a := String)
  let (activeMenuEvent, fireActiveMenu) ← newTriggerEvent (t := Spider) (a := Option Nat)

  -- Dynamics
  let activeMenuDyn ← holdDyn none activeMenuEvent

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName config.globalKeys

  -- Attach current state to key events using FRP pattern
  let keyEventsWithState ← Event.attachWithM
    (fun state kd => (state, kd)) stateDyn.current keyEvents

  let updateState : MenuBarState -> IO Unit := fun newState => do
    fireState newState
    fireActiveMenu newState.openMenu

  -- Subscribe to key events with state attached
  let _unsub ← SpiderM.liftIO <| keyEventsWithState.subscribe fun (state, kd) => do
    let ke := kd.event

    if state.isOpen then
      -- Dropdown is open - handle dropdown navigation
      match ke.code with
      | .down | .char 'j' =>
        match menus[state.focusedMenu]? with
        | some menu =>
          let newState := state.selectNext menu.items
          updateState newState
        | none => pure ()

      | .up | .char 'k' =>
        match menus[state.focusedMenu]? with
        | some menu =>
          let newState := state.selectPrev menu.items
          updateState newState
        | none => pure ()

      | .right =>
        let newState := state.switchToNextMenu menus
        updateState newState

      | .left =>
        let newState := state.switchToPrevMenu menus
        updateState newState

      | .enter =>
        -- Select the current item
        match menus[state.focusedMenu]? with
        | some menu =>
          match menu.items[state.selectedItem]? with
          | some item =>
            if !item.isSeparator then
              fireSelect (state.focusedMenu, state.selectedItem)
              fireSelectLabel item.label
              let newState := state.closeMenu
              updateState newState
          | none => pure ()
        | none => pure ()

      | .escape =>
        let newState := state.closeMenu
        updateState newState

      | _ => pure ()

    else
      -- Dropdown is closed - handle top-level navigation
      match ke.code with
      | .left | .char 'h' =>
        let newState := state.focusPrev menus.size
        updateState newState

      | .right | .char 'l' =>
        let newState := state.focusNext menus.size
        updateState newState

      | .enter | .down | .char 'j' =>
        -- Open the current menu
        if h : state.focusedMenu < menus.size then
          let menu := menus[state.focusedMenu]
          let firstItem := MenuBarState.findFirstSelectableItem menu.items
          let newState := { state with
            openMenu := some state.focusedMenu
            selectedItem := firstItem
          }
          updateState newState

      | _ => pure ()

  -- Get focus state for highlighting
  let focusedInput ← useFocusedInputW
  let isFocusedDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render the menu bar
  let node ← isFocusedDyn.zipWith' (fun isFocused state =>
    if menus.isEmpty then
      RNode.text "(no menus)" config.style
    else
      -- Render top-level menu labels
      let labelPairs := menus.mapIdx fun i menu =>
        let isHighlighted := isFocused && i == state.focusedMenu
        let labelStyle := if isHighlighted then config.activeStyle else config.style
        let labelNode := RNode.text menu.label labelStyle
        if i > 0 then
          let gapStr := String.ofList (List.replicate config.gap ' ')
          #[RNode.text gapStr {}, labelNode]
        else
          #[labelNode]

      let topLabels := labelPairs.foldl (· ++ ·) #[]
      let topRow := RNode.row 0 {} topLabels

      -- Render dropdown if open
      match state.openMenu with
      | none => topRow
      | some menuIdx =>
        match menus[menuIdx]? with
        | none => topRow
        | some menu =>
          -- Calculate offset for dropdown positioning
          -- The dropdown should appear below the corresponding menu label
          let offsetCols := (List.range menuIdx).foldl (fun acc j =>
            match menus[j]? with
            | some m => acc + m.label.length + config.gap
            | none => acc) 0

          let dropdown := renderDropdown menu state.selectedItem config

          -- Create spacing to position dropdown under the correct menu
          let spacer := if offsetCols > 0 then
            RNode.text (String.ofList (List.replicate offsetCols ' ')) {}
          else
            RNode.empty

          let dropdownRow := RNode.row 0 {} #[spacer, dropdown]
          RNode.column 0 {} #[topRow, dropdownRow]
  ) stateDyn
  emit node

  pure {
    activeMenu := activeMenuDyn
    onSelect := selectEvent
    onSelectLabel := selectLabelEvent
  }

end Terminus.Reactive
