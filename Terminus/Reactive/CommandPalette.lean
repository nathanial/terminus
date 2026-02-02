/-
  Terminus Reactive - Command Palette Widget
  A fuzzy search command picker similar to VS Code's Ctrl+P/Ctrl+Shift+P.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Command Types -/

/-- A command that can be executed from the palette. -/
structure Command where
  /-- Display name of the command. -/
  name : String
  /-- Optional description shown below the name. -/
  description : Option String := none
  /-- Optional keyboard shortcut display (e.g., "Ctrl+B"). -/
  shortcut : Option String := none
  deriving Repr, Inhabited, BEq

/-! ## Command Palette Configuration -/

/-- Configuration for command palette appearance and behavior. -/
structure CommandPaletteConfig where
  /-- Placeholder text shown in the search input. -/
  placeholder : String := "Type to search..."
  /-- Maximum number of visible results. -/
  maxVisible : Nat := 8
  /-- Width of the palette in characters. -/
  width : Nat := 40
  /-- Whether to show keyboard shortcuts. -/
  showShortcuts : Bool := true
  /-- Whether to show descriptions. -/
  showDescriptions : Bool := true
  /-- Use fuzzy matching instead of substring matching. -/
  fuzzyMatch : Bool := true
  /-- Style for the header/title. -/
  headerStyle : Style := { modifier := { bold := true } }
  /-- Style for the selected command row. -/
  selectedStyle : Style := { fg := .ansi .black, bg := .ansi .cyan }
  /-- Style for highlighting matched characters. -/
  matchStyle : Style := { fg := .ansi .yellow }
  /-- Style for the search input. -/
  inputStyle : Style := { fg := .ansi .white }
  /-- Style for command descriptions. -/
  descriptionStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for keyboard shortcuts. -/
  shortcutStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for the cursor in the search input. -/
  cursorStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Style for unselected command rows. -/
  normalStyle : Style := {}
  /-- Border type for the palette. -/
  borderType : BorderType := .rounded
  /-- Border style. -/
  borderStyle : Style := { fg := .ansi .white }
  deriving Inhabited

/-! ## Command Palette Result -/

/-- Result returned by commandPalette' containing reactive values and control functions. -/
structure CommandPaletteResult where
  /-- Whether the palette is currently open. -/
  isOpen : Dynamic Spider Bool
  /-- Currently highlighted command (if any). -/
  selectedCommand : Dynamic Spider (Option Command)
  /-- Event fired when a command is selected (Enter pressed). -/
  onSelect : Reactive.Event Spider Command
  /-- Event fired when the palette is closed (Escape pressed). -/
  onClose : Reactive.Event Spider Unit
  /-- Open the command palette. -/
  openPalette : IO Unit
  /-- Close the command palette. -/
  closePalette : IO Unit
  /-- Toggle the command palette open/closed. -/
  togglePalette : IO Unit

/-! ## Fuzzy Matching -/

/-- Check if query fuzzy-matches the target string.
    Returns true if all characters of query appear in order in target (case-insensitive).
    E.g., "bt" matches "Build Tests". -/
private def fuzzyMatch (query : String) (target : String) : Bool := Id.run do
  if query.isEmpty then return true
  let queryChars := query.toLower.toList
  let targetChars := target.toLower.toList
  let mut queryIdx := 0
  for c in targetChars do
    if queryIdx < queryChars.length then
      if queryChars[queryIdx]! == c then
        queryIdx := queryIdx + 1
  return decide (queryIdx >= queryChars.length)

/-- Check if query is a substring of target (case-insensitive).
    Uses a simple sliding window approach via List.foldl. -/
private def substringMatch (query : String) (target : String) : Bool :=
  if query.isEmpty then true
  else
    -- Check if query appears anywhere in target (case-insensitive)
    let q := query.toLower
    let t := target.toLower
    let qLen := q.length
    let tLen := t.length
    if qLen > tLen then false
    else
      -- Check each starting position using fold
      let positions := List.range (tLen - qLen + 1)
      positions.any fun i =>
        (t.drop i).take qLen == q

/-- Filter commands based on query and matching mode. -/
private def filterCommands (query : String) (commands : Array Command)
    (useFuzzy : Bool) (maxCount : Nat) : Array Command :=
  let matchFn := if useFuzzy then fuzzyMatch else substringMatch
  let filtered := commands.filter (fun cmd => matchFn query cmd.name)
  filtered.toList.take maxCount |>.toArray

/-! ## Fuzzy Match Highlighting -/

/-- Find indices of matched characters for fuzzy matching.
    Returns array of indices where query characters match in target. -/
private def findMatchIndices (query : String) (target : String) : Array Nat := Id.run do
  if query.isEmpty then return #[]
  let queryChars := query.toLower.toList
  let targetChars := target.toLower.toList
  let mut indices : Array Nat := #[]
  let mut queryIdx := 0
  for i in [:targetChars.length] do
    if queryIdx < queryChars.length then
      if queryChars[queryIdx]! == targetChars[i]! then
        indices := indices.push i
        queryIdx := queryIdx + 1
  return indices

/-! ## Command Palette State -/

/-- Internal state for the command palette. -/
structure CommandPaletteState where
  /-- Current search query text. -/
  query : String := ""
  /-- Cursor position in the query. -/
  cursor : Nat := 0
  /-- Currently selected index in filtered results. -/
  selectedIndex : Nat := 0
  /-- Whether the palette is open. -/
  isOpen : Bool := false
  deriving Repr, Inhabited, BEq

namespace CommandPaletteState

/-- Insert a character at cursor position. -/
def insertChar (s : CommandPaletteState) (c : Char) : CommandPaletteState :=
  let before := s.query.take s.cursor
  let after := s.query.drop s.cursor
  { s with
    query := before ++ c.toString ++ after
    cursor := s.cursor + 1
    selectedIndex := 0 }

/-- Delete character before cursor (backspace). -/
def backspace (s : CommandPaletteState) : CommandPaletteState :=
  if s.cursor == 0 then s
  else
    let before := s.query.take (s.cursor - 1)
    let after := s.query.drop s.cursor
    { s with
      query := before ++ after
      cursor := s.cursor - 1
      selectedIndex := 0 }

/-- Delete character at cursor (delete key). -/
def delete (s : CommandPaletteState) : CommandPaletteState :=
  if s.cursor >= s.query.length then s
  else
    let before := s.query.take s.cursor
    let after := s.query.drop (s.cursor + 1)
    { s with
      query := before ++ after
      selectedIndex := 0 }

/-- Move cursor left. -/
def moveLeft (s : CommandPaletteState) : CommandPaletteState :=
  if s.cursor == 0 then s
  else { s with cursor := s.cursor - 1 }

/-- Move cursor right. -/
def moveRight (s : CommandPaletteState) : CommandPaletteState :=
  if s.cursor >= s.query.length then s
  else { s with cursor := s.cursor + 1 }

/-- Move cursor to start. -/
def moveHome (s : CommandPaletteState) : CommandPaletteState :=
  { s with cursor := 0 }

/-- Move cursor to end. -/
def moveEnd (s : CommandPaletteState) : CommandPaletteState :=
  { s with cursor := s.query.length }

/-- Move selection up in results. -/
def moveUp (s : CommandPaletteState) (resultCount : Nat) : CommandPaletteState :=
  if resultCount == 0 then s
  else if s.selectedIndex == 0 then
    { s with selectedIndex := resultCount - 1 }
  else
    { s with selectedIndex := s.selectedIndex - 1 }

/-- Move selection down in results. -/
def moveDown (s : CommandPaletteState) (resultCount : Nat) : CommandPaletteState :=
  if resultCount == 0 then s
  else if s.selectedIndex >= resultCount - 1 then
    { s with selectedIndex := 0 }
  else
    { s with selectedIndex := s.selectedIndex + 1 }

/-- Open the palette. -/
def setOpen (s : CommandPaletteState) : CommandPaletteState :=
  { s with isOpen := true, query := "", cursor := 0, selectedIndex := 0 }

/-- Close the palette. -/
def setClosed (s : CommandPaletteState) : CommandPaletteState :=
  { s with isOpen := false }

/-- Toggle the palette open/closed. -/
def toggle (s : CommandPaletteState) : CommandPaletteState :=
  if s.isOpen then s.setClosed else s.setOpen

end CommandPaletteState

/-! ## Control Operations -/

/-- Control operations that can be triggered externally. -/
inductive ControlOp
  | open
  | close
  | toggle
  deriving Repr, Inhabited, BEq

/-! ## Render Helpers -/

/-- Build the search input line with cursor. -/
private def buildInputNode (state : CommandPaletteState) (config : CommandPaletteConfig)
    : RNode := Id.run do
  let prompt := "> "
  let promptNode := RNode.text prompt config.inputStyle

  if state.query.isEmpty then
    -- Show placeholder with cursor at start
    let cursorNode := RNode.text "|" config.cursorStyle
    let placeholderNode := RNode.text config.placeholder config.descriptionStyle
    return RNode.row 0 {} #[promptNode, cursorNode, placeholderNode]

  -- Build query with cursor
  let before := state.query.take state.cursor
  let after := state.query.drop state.cursor

  let beforeNode := if before.isEmpty then RNode.empty else RNode.text before config.inputStyle
  let cursorNode := RNode.text "|" config.cursorStyle
  let afterNode := if after.isEmpty then RNode.empty else RNode.text after config.inputStyle

  -- Calculate padding to fill width
  let usedWidth := prompt.length + state.query.length + 1  -- +1 for cursor
  let padding := if usedWidth < config.width - 2 then config.width - 2 - usedWidth else 0
  let padNode := if padding > 0 then
    RNode.text (String.ofList (List.replicate padding ' ')) config.inputStyle
  else
    RNode.empty

  let mut nodes : Array RNode := #[promptNode]
  if !before.isEmpty then nodes := nodes.push beforeNode
  nodes := nodes.push cursorNode
  if !after.isEmpty then nodes := nodes.push afterNode
  if padding > 0 then nodes := nodes.push padNode

  return RNode.row 0 {} nodes

/-- Build a command name node with fuzzy match highlighting. -/
private def buildHighlightedNameNode (name : String) (query : String)
    (isSelected : Bool) (config : CommandPaletteConfig) : RNode := Id.run do
  let baseStyle := if isSelected then config.selectedStyle else config.normalStyle
  let matchIndices := findMatchIndices query name
  let chars := name.toList
  let mut nodes : Array RNode := #[]
  for i in [:chars.length] do
    let c := chars[i]!
    let style := if matchIndices.contains i then
      if isSelected then
        { config.matchStyle with bg := config.selectedStyle.bg }
      else
        config.matchStyle
    else
      baseStyle
    nodes := nodes.push (RNode.text c.toString style)
  return RNode.row 0 {} nodes

/-- Build a command row with optional match highlighting. -/
private def buildCommandNode (cmd : Command) (isSelected : Bool) (query : String)
    (config : CommandPaletteConfig) : RNode := Id.run do
  let baseStyle := if isSelected then config.selectedStyle else config.normalStyle
  let itemPrefix := if isSelected then "> " else "  "
  let prefixNode := RNode.text itemPrefix baseStyle

  -- Build the command name with match highlighting
  let nameNode := if config.fuzzyMatch && !query.isEmpty then
    buildHighlightedNameNode cmd.name query isSelected config
  else
    RNode.text cmd.name baseStyle

  -- Calculate used width for name
  let nameWidth := itemPrefix.length + cmd.name.length

  -- Add description if enabled and present
  let descNode := if config.showDescriptions then
    match cmd.description with
    | some desc =>
      let descStyle := if isSelected then
        { config.descriptionStyle with bg := config.selectedStyle.bg }
      else
        config.descriptionStyle
      RNode.text s!" - {desc}" descStyle
    | none => RNode.empty
  else
    RNode.empty

  -- Add shortcut if enabled and present
  let shortcutNode := if config.showShortcuts then
    match cmd.shortcut with
    | some shortcut =>
      let shortcutStyle := if isSelected then
        { config.shortcutStyle with bg := config.selectedStyle.bg }
      else
        config.shortcutStyle
      -- Right-align shortcut
      let descWidth := match cmd.description with
        | some d => if config.showDescriptions then d.length + 3 else 0
        | none => 0
      let usedWidth := nameWidth + descWidth
      let shortcutWithParens := s!"({shortcut})"
      let targetPos := config.width - 2 - shortcutWithParens.length
      let spacesNeeded := if targetPos > usedWidth then targetPos - usedWidth else 1
      let spacer := RNode.text (String.ofList (List.replicate spacesNeeded ' ')) baseStyle
      RNode.row 0 {} #[spacer, RNode.text shortcutWithParens shortcutStyle]
    | none => RNode.empty
  else
    RNode.empty

  -- Combine all parts
  RNode.row 0 {} #[prefixNode, nameNode, descNode, shortcutNode]

/-- Build the separator line. -/
private def buildSeparatorNode (config : CommandPaletteConfig) : RNode :=
  let line := String.ofList (List.replicate (config.width - 2) '-')
  RNode.text line config.descriptionStyle

/-- Build the complete command palette render tree. -/
private def buildPaletteNode (state : CommandPaletteState) (filteredCommands : Array Command)
    (config : CommandPaletteConfig) : RNode := Id.run do
  let mut rows : Array RNode := #[]

  -- Search input
  rows := rows.push (buildInputNode state config)

  -- Separator
  rows := rows.push (buildSeparatorNode config)

  -- Command list
  for i in [:filteredCommands.size] do
    if h : i < filteredCommands.size then
      let cmd := filteredCommands[i]
      let isSelected := i == state.selectedIndex
      rows := rows.push (buildCommandNode cmd isSelected state.query config)

  -- Show "No commands found" if empty
  if filteredCommands.isEmpty && !state.query.isEmpty then
    rows := rows.push (RNode.text "  No commands found" config.descriptionStyle)

  let inner := RNode.column 0 {} rows
  let fillStyle : Style := { bg := .ansi .black }
  RNode.block (some "Commands") config.borderType config.borderStyle (some fillStyle) inner

/-! ## Command Palette Widget -/

/-- Create a command palette widget.

    The palette shows a searchable list of commands with fuzzy matching.
    When opened, focus goes to the search input. Type to filter commands,
    use Up/Down to navigate, Enter to select, Escape to close.

    Example:
    ```
    let commands := #[
      { name := "Build Project", shortcut := some "Ctrl+B" },
      { name := "Run Tests", shortcut := some "Ctrl+T" },
      { name := "Deploy", description := some "Deploy to production" }
    ]
    let palette ← commandPalette' "cmd" commands {}

    -- Open with a keyboard shortcut
    let ctrlP ← useKey (.char 'p')  -- Or however you detect Ctrl+P
    performEvent_ ctrlP fun _ => palette.openPalette

    -- Handle selection
    performEvent_ palette.onSelect fun cmd =>
      -- Execute the selected command
      ...
    ```
-/
def commandPalette' (name : String) (commands : Array Command)
    (config : CommandPaletteConfig := {}) : WidgetM CommandPaletteResult := do
  -- Register as focusable input component
  let widgetName ← registerComponentW "command-palette" (isInput := true) (nameOverride := name)

  -- Get events context for focus management
  let events ← getEventsW

  -- Compute input name for focus handling
  let inputName := if name.isEmpty then widgetName else name

  -- Create trigger events for external control operations
  let (controlEvent, fireControl) ← newTriggerEvent (t := Spider) (a := ControlOp)

  -- Initial state
  let initialState : CommandPaletteState := {}

  -- Create a preliminary isOpen dynamic for gating key events
  -- We'll use a ref to track open state for visibility gating
  let isOpenRef ← SpiderM.liftIO <| IO.mkRef false

  -- Create visibility behavior from ref
  let isOpenBehavior := Behavior.fromSample isOpenRef.get

  -- Get visibility-gated key events (only respond when open)
  let keyEvents ← Event.gateM isOpenBehavior events.keyEvent

  -- Map key events to state transformation functions
  -- Note: For up/down we need result count, so we use a closure over commands/config
  let keyStateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    match kd.event.code with
    | .char c =>
      if c.val >= 32 then
        some fun (state : CommandPaletteState) => state.insertChar c
      else
        none
    | .backspace => some fun (state : CommandPaletteState) => state.backspace
    | .delete => some fun (state : CommandPaletteState) => state.delete
    | .left => some fun (state : CommandPaletteState) => state.moveLeft
    | .right => some fun (state : CommandPaletteState) => state.moveRight
    | .home => some fun (state : CommandPaletteState) => state.moveHome
    | .end => some fun (state : CommandPaletteState) => state.moveEnd
    | .up => some fun (state : CommandPaletteState) =>
      let filtered := filterCommands state.query commands config.fuzzyMatch config.maxVisible
      state.moveUp filtered.size
    | .down => some fun (state : CommandPaletteState) =>
      let filtered := filterCommands state.query commands config.fuzzyMatch config.maxVisible
      state.moveDown filtered.size
    | .escape => some fun (state : CommandPaletteState) => state.setClosed
    | .enter => some fun (state : CommandPaletteState) => state.setClosed
    | _ => none) keyEvents

  -- Map control events to state transformation functions
  let controlStateOps ← Event.mapM (fun (op : ControlOp) =>
    match op with
    | .open => fun (state : CommandPaletteState) => state.setOpen
    | .close => fun (state : CommandPaletteState) => state.setClosed
    | .toggle => fun (state : CommandPaletteState) => state.toggle) controlEvent

  -- Merge all state operations
  let allStateOps ← Event.mergeM keyStateOps controlStateOps

  -- Fold state operations over initial state
  let stateDyn ← foldDyn id initialState allStateOps

  -- Update the isOpen ref whenever state changes (for visibility gating)
  let _unsub ← SpiderM.liftIO <| stateDyn.updated.subscribe fun state => do
    isOpenRef.set state.isOpen

  -- Create dynamics for isOpen and selectedCommand
  let isOpenDyn ← stateDyn.map' (·.isOpen)
  let selectedCommandDyn ← stateDyn.map' fun state =>
    let filtered := filterCommands state.query commands config.fuzzyMatch config.maxVisible
    if h : state.selectedIndex < filtered.size then
      some filtered[state.selectedIndex]
    else
      none

  -- Filter for enter key events (for selection)
  let enterEvents ← Event.filterM (fun kd => kd.event.code == .enter) keyEvents

  -- Attach current state to enter events to get the selected command
  let selectEvent ← Event.mapMaybeM (fun (state : CommandPaletteState) =>
    let filtered := filterCommands state.query commands config.fuzzyMatch config.maxVisible
    if h : state.selectedIndex < filtered.size then
      some filtered[state.selectedIndex]
    else
      none) (← Event.attachWithM (fun (state : CommandPaletteState) _ => state) stateDyn.current enterEvents)

  -- Filter for escape and enter events (for close)
  let escapeEvents ← Event.filterM (fun kd => kd.event.code == .escape) keyEvents
  let closeFromEscape ← Event.mapM (fun _ => ()) escapeEvents
  let closeFromEnter ← Event.mapM (fun _ => ()) enterEvents
  let closeEvent ← Event.mergeM closeFromEscape closeFromEnter

  -- Handle focus changes when opening/closing
  let _unsub2 ← SpiderM.liftIO <| stateDyn.updated.subscribe fun state => do
    if state.isOpen then
      events.registry.fireFocus (some inputName)
    else
      events.registry.fireFocus none

  -- Control functions
  let doOpenPalette : IO Unit := fireControl .open
  let doClosePalette : IO Unit := fireControl .close
  let doTogglePalette : IO Unit := fireControl .toggle

  -- Build render node
  let node ← stateDyn.map' fun state =>
    if state.isOpen then
      let filteredCommands := filterCommands state.query commands config.fuzzyMatch config.maxVisible
      let paletteNode := buildPaletteNode state filteredCommands config
      -- Wrap in overlay
      RNode.overlay RNode.empty paletteNode (some { modifier := { dim := true } })
    else
      RNode.empty

  emit node

  pure {
    isOpen := isOpenDyn
    selectedCommand := selectedCommandDyn
    onSelect := selectEvent
    onClose := closeEvent
    openPalette := doOpenPalette
    closePalette := doClosePalette
    togglePalette := doTogglePalette
  }

/-- Create a command palette with dynamic commands.
    The commands array can change over time. -/
def dynCommandPalette' (name : String) (commands : Dynamic Spider (Array Command))
    (config : CommandPaletteConfig := {}) : WidgetM CommandPaletteResult := do
  -- Register as focusable input component
  let widgetName ← registerComponentW "command-palette" (isInput := true) (nameOverride := name)

  -- Get events context for focus management
  let events ← getEventsW

  -- Compute input name for focus handling
  let inputName := if name.isEmpty then widgetName else name

  -- Create trigger events for external control operations
  let (controlEvent, fireControl) ← newTriggerEvent (t := Spider) (a := ControlOp)

  -- Initial state
  let initialState : CommandPaletteState := {}

  -- Create a ref to track open state for visibility gating
  let isOpenRef ← SpiderM.liftIO <| IO.mkRef false

  -- Create visibility behavior from ref
  let isOpenBehavior := Behavior.fromSample isOpenRef.get

  -- Get visibility-gated key events (only respond when open)
  let keyEvents ← Event.gateM isOpenBehavior events.keyEvent

  -- Attach commands to key events for state transformations
  let keyEventsWithCommands ← Event.attachWithM
    (fun (cmds : Array Command) (kd : KeyData) => (cmds, kd))
    commands.current keyEvents

  -- Map key events to state transformation functions
  let keyStateOps ← Event.mapMaybeM (fun ((cmds, kd) : Array Command × KeyData) =>
    match kd.event.code with
    | .char c =>
      if c.val >= 32 then
        some fun (state : CommandPaletteState) => state.insertChar c
      else
        none
    | .backspace => some fun (state : CommandPaletteState) => state.backspace
    | .delete => some fun (state : CommandPaletteState) => state.delete
    | .left => some fun (state : CommandPaletteState) => state.moveLeft
    | .right => some fun (state : CommandPaletteState) => state.moveRight
    | .home => some fun (state : CommandPaletteState) => state.moveHome
    | .end => some fun (state : CommandPaletteState) => state.moveEnd
    | .up => some fun (state : CommandPaletteState) =>
      let filtered := filterCommands state.query cmds config.fuzzyMatch config.maxVisible
      state.moveUp filtered.size
    | .down => some fun (state : CommandPaletteState) =>
      let filtered := filterCommands state.query cmds config.fuzzyMatch config.maxVisible
      state.moveDown filtered.size
    | .escape => some fun (state : CommandPaletteState) => state.setClosed
    | .enter => some fun (state : CommandPaletteState) => state.setClosed
    | _ => none) keyEventsWithCommands

  -- Map control events to state transformation functions
  let controlStateOps ← Event.mapM (fun (op : ControlOp) =>
    match op with
    | .open => fun (state : CommandPaletteState) => state.setOpen
    | .close => fun (state : CommandPaletteState) => state.setClosed
    | .toggle => fun (state : CommandPaletteState) => state.toggle) controlEvent

  -- Merge all state operations
  let allStateOps ← Event.mergeM keyStateOps controlStateOps

  -- Fold state operations over initial state
  let stateDyn ← foldDyn id initialState allStateOps

  -- Update the isOpen ref whenever state changes (for visibility gating)
  let _unsub ← SpiderM.liftIO <| stateDyn.updated.subscribe fun state => do
    isOpenRef.set state.isOpen

  -- Create dynamics for isOpen and selectedCommand
  let isOpenDyn ← stateDyn.map' (·.isOpen)
  let selectedCommandDyn ← stateDyn.zipWith' (fun state cmds =>
    let filtered := filterCommands state.query cmds config.fuzzyMatch config.maxVisible
    if h : state.selectedIndex < filtered.size then
      some filtered[state.selectedIndex]
    else
      none
  ) commands

  -- Filter for enter key events (for selection)
  let enterEvents ← Event.filterM (fun (cmds, kd) => kd.event.code == .enter) keyEventsWithCommands

  -- Attach current state to enter events to get the selected command
  let selectEvent ← Event.mapMaybeM (fun ((state, cmds) : CommandPaletteState × Array Command) =>
    let filtered := filterCommands state.query cmds config.fuzzyMatch config.maxVisible
    if h : state.selectedIndex < filtered.size then
      some filtered[state.selectedIndex]
    else
      none) (← Event.attachWithM (fun (state : CommandPaletteState) (cmds, _) => (state, cmds)) stateDyn.current enterEvents)

  -- Filter for escape and enter events (for close)
  let escapeEvents ← Event.filterM (fun (_, kd) => kd.event.code == .escape) keyEventsWithCommands
  let closeFromEscape ← Event.mapM (fun _ => ()) escapeEvents
  let closeFromEnter ← Event.mapM (fun _ => ()) enterEvents
  let closeEvent ← Event.mergeM closeFromEscape closeFromEnter

  -- Handle focus changes when opening/closing
  let _unsub2 ← SpiderM.liftIO <| stateDyn.updated.subscribe fun state => do
    if state.isOpen then
      events.registry.fireFocus (some inputName)
    else
      events.registry.fireFocus none

  -- Control functions
  let doOpenPalette : IO Unit := fireControl .open
  let doClosePalette : IO Unit := fireControl .close
  let doTogglePalette : IO Unit := fireControl .toggle

  -- Build render node
  let node ← stateDyn.zipWith' (fun state cmds =>
    if state.isOpen then
      let filteredCommands := filterCommands state.query cmds config.fuzzyMatch config.maxVisible
      let paletteNode := buildPaletteNode state filteredCommands config
      -- Wrap in overlay
      RNode.overlay RNode.empty paletteNode (some { modifier := { dim := true } })
    else
      RNode.empty
  ) commands

  emit node

  pure {
    isOpen := isOpenDyn
    selectedCommand := selectedCommandDyn
    onSelect := selectEvent
    onClose := closeEvent
    openPalette := doOpenPalette
    closePalette := doClosePalette
    togglePalette := doTogglePalette
  }

/-! ## Convenience Functions -/

/-- Create a simple command palette with default styling. -/
def simpleCommandPalette' (name : String) (commands : Array Command)
    : WidgetM CommandPaletteResult :=
  commandPalette' name commands {}

/-- Create a command palette without shortcuts display. -/
def commandPaletteNoShortcuts' (name : String) (commands : Array Command)
    (config : CommandPaletteConfig := {}) : WidgetM CommandPaletteResult :=
  commandPalette' name commands { config with showShortcuts := false }

/-- Create a command palette using substring matching instead of fuzzy. -/
def commandPaletteExact' (name : String) (commands : Array Command)
    (config : CommandPaletteConfig := {}) : WidgetM CommandPaletteResult :=
  commandPalette' name commands { config with fuzzyMatch := false }

end Terminus.Reactive
