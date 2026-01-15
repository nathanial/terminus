/-
  Terminus Reactive - Tree Component
  Hierarchical tree view with expand/collapse and keyboard navigation.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Tree Node Structure -/

/-- A node in the tree hierarchy. -/
structure TreeNode (α : Type) where
  /-- Value stored in this node. -/
  value : α
  /-- Child nodes. -/
  children : Array (TreeNode α) := #[]
  /-- Whether this branch is expanded (ignored for leaves). -/
  expanded : Bool := true
  deriving Repr, Inhabited

namespace TreeNode

/-- Create a leaf node (no children). -/
def leaf (value : α) : TreeNode α := { value, children := #[], expanded := true }

/-- Create a branch node with children. -/
def branch (value : α) (children : Array (TreeNode α)) : TreeNode α :=
  { value, children, expanded := true }

/-- Create a collapsed branch. -/
def collapsedBranch (value : α) (children : Array (TreeNode α)) : TreeNode α :=
  { value, children, expanded := false }

/-- Check if this is a leaf (no children). -/
def isLeaf (node : TreeNode α) : Bool := node.children.isEmpty

/-- Check if this is a branch (has children). -/
def isBranch (node : TreeNode α) : Bool := !node.children.isEmpty

/-- Toggle expand/collapse state. -/
def toggle (node : TreeNode α) : TreeNode α :=
  if node.isLeaf then node else { node with expanded := !node.expanded }

/-- Expand a branch. -/
def expand (node : TreeNode α) : TreeNode α :=
  if node.isLeaf then node else { node with expanded := true }

/-- Collapse a branch. -/
def collapse (node : TreeNode α) : TreeNode α :=
  if node.isLeaf then node else { node with expanded := false }

end TreeNode

/-! ## Flattened Tree Line -/

/-- A visible line in the flattened tree view. -/
structure FlatLine (α : Type) where
  /-- The node value. -/
  value : α
  /-- Depth in tree (0 = root). -/
  depth : Nat
  /-- Path of indices to this node. -/
  path : Array Nat
  /-- Whether this is a leaf node. -/
  isLeaf : Bool
  /-- Whether this branch is expanded. -/
  isExpanded : Bool
  /-- Whether this is the last child at its level. -/
  isLast : Bool
  deriving Repr, Inhabited

/-! ## Tree Configuration -/

/-- Configuration for tree appearance and behavior. -/
structure TreeConfig where
  /-- Style for unselected items. -/
  style : Style := {}
  /-- Style for selected item. -/
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Style for branch nodes. -/
  branchStyle : Style := { modifier := { bold := true } }
  /-- Icon for expanded branches. -/
  expandedIcon : String := "▼ "
  /-- Icon for collapsed branches. -/
  collapsedIcon : String := "▶ "
  /-- Icon for leaf nodes. -/
  leafIcon : String := "• "
  /-- Characters per indent level. -/
  indent : Nat := 2
  /-- Maximum visible lines (none = no limit). -/
  maxVisible : Option Nat := none
  /-- Focus name for this tree. Empty = auto-generated. -/
  focusName : String := ""
  /-- Whether tree responds to keys without focus. -/
  globalKeys : Bool := false
  /-- Show tree connector lines (├── └── │). -/
  showConnectors : Bool := true
  /-- Style for connector lines. -/
  connectorStyle : Style := { fg := .ansi .brightBlack }
  deriving Repr, Inhabited

/-! ## Tree Result -/

/-- Result returned by tree' containing reactive values and events. -/
structure TreeResult (α : Type) where
  /-- Path to currently selected node. -/
  selectedPath : Reactive.Dynamic Spider (Array Nat)
  /-- Currently selected node value (none if tree is empty). -/
  selectedNode : Reactive.Dynamic Spider (Option α)
  /-- Event fired when Enter is pressed on a node. -/
  onSelect : Reactive.Event Spider α
  /-- Event fired when a branch is toggled. -/
  onToggle : Reactive.Event Spider (Array Nat)

/-! ## Tree State -/

/-- Internal state for tree navigation. -/
structure TreeState where
  /-- Index in flattened visible list. -/
  selectedIndex : Nat := 0
  /-- Scroll offset (first visible item). -/
  scrollOffset : Nat := 0
  deriving Repr, Inhabited, BEq

namespace TreeState

/-- Move selection up. -/
def moveUp (s : TreeState) (itemCount : Nat) : TreeState :=
  if itemCount == 0 || s.selectedIndex == 0 then s
  else { s with selectedIndex := s.selectedIndex - 1 }

/-- Move selection down. -/
def moveDown (s : TreeState) (itemCount : Nat) : TreeState :=
  if itemCount == 0 || s.selectedIndex >= itemCount - 1 then s
  else { s with selectedIndex := s.selectedIndex + 1 }

/-- Jump to first item. -/
def moveToFirst (s : TreeState) : TreeState :=
  { s with selectedIndex := 0, scrollOffset := 0 }

/-- Jump to last item. -/
def moveToLast (s : TreeState) (itemCount : Nat) : TreeState :=
  if itemCount == 0 then s
  else { s with selectedIndex := itemCount - 1 }

/-- Adjust scroll to keep selection visible. -/
def adjustScroll (s : TreeState) (maxVisible : Nat) : TreeState :=
  if maxVisible == 0 then s
  else
    let newOffset := if s.selectedIndex < s.scrollOffset then
      s.selectedIndex
    else if s.selectedIndex >= s.scrollOffset + maxVisible then
      s.selectedIndex - maxVisible + 1
    else
      s.scrollOffset
    { s with scrollOffset := newOffset }

end TreeState

/-- Combined state for tree widget: navigation state + tree structure. -/
structure FullTreeState (α : Type) where
  /-- Navigation state (selection, scroll). -/
  nav : TreeState := {}
  /-- Tree roots (structure with expand/collapse state). -/
  roots : Array (TreeNode α) := #[]
  deriving Inhabited

/-! ## Tree Helpers -/

/-- Flatten a tree into visible lines. -/
partial def flattenTree [Inhabited α] (node : TreeNode α) (depth : Nat) (path : Array Nat)
    (isLast : Bool) : Array (FlatLine α) := Id.run do
  let mut result : Array (FlatLine α) := #[]

  -- Add this node
  result := result.push {
    value := node.value
    depth := depth
    path := path
    isLeaf := node.isLeaf
    isExpanded := node.expanded
    isLast := isLast
  }

  -- Add children if expanded
  if node.expanded && !node.children.isEmpty then
    for h : i in [:node.children.size] do
      let child := node.children[i]
      let childPath := path.push i
      let childIsLast := i == node.children.size - 1
      result := result ++ flattenTree child (depth + 1) childPath childIsLast

  result

/-- Flatten a forest (array of root nodes). -/
def flattenForest [Inhabited α] (roots : Array (TreeNode α)) : Array (FlatLine α) := Id.run do
  let mut result : Array (FlatLine α) := #[]
  for h : i in [:roots.size] do
    let root := roots[i]
    let isLast := i == roots.size - 1
    result := result ++ flattenTree root 0 #[i] isLast
  result

-- Simple BEq for change detection (compares value and expanded, not children structure)
instance [BEq α] : BEq (TreeNode α) where
  beq a b := a.value == b.value && a.expanded == b.expanded && a.children.size == b.children.size

instance [BEq α] : BEq (FullTreeState α) where
  beq a b := a.nav == b.nav && a.roots == b.roots

namespace FullTreeState

/-- Get the flattened view of the tree. -/
def flatten [Inhabited α] (s : FullTreeState α) : Array (FlatLine α) :=
  flattenForest s.roots

/-- Get the currently selected line (if any). -/
def currentLine [Inhabited α] (s : FullTreeState α) : Option (FlatLine α) :=
  let flat := s.flatten
  flat[s.nav.selectedIndex]?

/-- Get the path to the currently selected node. -/
def selectedPath [Inhabited α] (s : FullTreeState α) : Array Nat :=
  match s.currentLine with
  | some line => line.path
  | none => #[]

/-- Get the currently selected node value. -/
def selectedNode [Inhabited α] (s : FullTreeState α) : Option α :=
  match s.currentLine with
  | some line => some line.value
  | none => none

end FullTreeState

private def renderTreeView [Inhabited α] [ToString α]
    (roots : Array (TreeNode α)) (state : TreeState) (config : TreeConfig) : RNode :=
  Id.run do
    let flat := flattenForest roots

    if flat.isEmpty then
      return RNode.text "(empty)" config.style
    else
      let maxVis := config.maxVisible.getD flat.size

      -- Calculate visible range
      let startIdx := state.scrollOffset
      let endIdx := min (startIdx + maxVis) flat.size

      -- Build visible lines
      let mut rows : Array RNode := #[]

      for i in [startIdx:endIdx] do
        if h : i < flat.size then
          let line := flat[i]
          let isSelected := i == state.selectedIndex

          -- Build indent and connector
          let indentStr := String.ofList (List.replicate (line.depth * config.indent) ' ')

          -- Choose icon
          let icon := if line.isLeaf then config.leafIcon
            else if line.isExpanded then config.expandedIcon
            else config.collapsedIcon

          -- Build full text
          let text := indentStr ++ icon ++ toString line.value

          -- Choose style
          let baseStyle := if line.isLeaf then config.style else config.branchStyle
          let style := if isSelected then config.selectedStyle else baseStyle

          rows := rows.push (RNode.text text style)

      return RNode.column 0 {} rows

/-- Get node at path. -/
partial def getNodeAtPath [Inhabited α] (roots : Array (TreeNode α)) (path : Array Nat) : Option α :=
  match path.toList with
  | [] => none
  | [idx] => roots[idx]?.map (·.value)
  | idx :: rest =>
    match roots[idx]? with
    | none => none
    | some node => getNodeAtPathInNode node rest
where
  getNodeAtPathInNode (node : TreeNode α) : List Nat → Option α
    | [] => some node.value
    | [idx] => node.children[idx]?.map (·.value)
    | idx :: rest =>
      match node.children[idx]? with
      | none => none
      | some child => getNodeAtPathInNode child rest

/-- Toggle node at path in a forest. -/
partial def toggleAtPath [Inhabited α] (roots : Array (TreeNode α)) (path : Array Nat) : Array (TreeNode α) :=
  match path.toList with
  | [] => roots
  | [idx] =>
    roots.mapIdx fun i node => if i == idx then node.toggle else node
  | idx :: rest =>
    roots.mapIdx fun i node =>
      if i == idx then toggleInNode node rest else node
where
  toggleInNode (node : TreeNode α) : List Nat → TreeNode α
    | [] => node.toggle
    | [idx] =>
      { node with children := node.children.mapIdx fun i c => if i == idx then c.toggle else c }
    | idx :: rest =>
      { node with children := node.children.mapIdx fun i c =>
          if i == idx then toggleInNode c rest else c }

/-- Collapse node at path. -/
partial def collapseAtPath [Inhabited α] (roots : Array (TreeNode α)) (path : Array Nat) : Array (TreeNode α) :=
  match path.toList with
  | [] => roots
  | [idx] =>
    roots.mapIdx fun i node => if i == idx then node.collapse else node
  | idx :: rest =>
    roots.mapIdx fun i node =>
      if i == idx then collapseInNode node rest else node
where
  collapseInNode (node : TreeNode α) : List Nat → TreeNode α
    | [] => node.collapse
    | [idx] =>
      { node with children := node.children.mapIdx fun i c => if i == idx then c.collapse else c }
    | idx :: rest =>
      { node with children := node.children.mapIdx fun i c =>
          if i == idx then collapseInNode c rest else c }

/-- Expand node at path. -/
partial def expandAtPath [Inhabited α] (roots : Array (TreeNode α)) (path : Array Nat) : Array (TreeNode α) :=
  match path.toList with
  | [] => roots
  | [idx] =>
    roots.mapIdx fun i node => if i == idx then node.expand else node
  | idx :: rest =>
    roots.mapIdx fun i node =>
      if i == idx then expandInNode node rest else node
where
  expandInNode (node : TreeNode α) : List Nat → TreeNode α
    | [] => node.expand
    | [idx] =>
      { node with children := node.children.mapIdx fun i c => if i == idx then c.expand else c }
    | idx :: rest =>
      { node with children := node.children.mapIdx fun i c =>
          if i == idx then expandInNode c rest else c }

/-! ## Tree Widget -/

/-- Represents a key action that may result in state change and/or events. -/
inductive TreeAction (α : Type) where
  /-- Move selection up. -/
  | moveUp
  /-- Move selection down. -/
  | moveDown
  /-- Move to first item. -/
  | moveToFirst
  /-- Move to last item. -/
  | moveToLast
  /-- Handle left arrow (collapse or move to parent). -/
  | left
  /-- Handle right arrow (expand or move to child). -/
  | right
  /-- Handle enter/space (select leaf or toggle branch). -/
  | activate

/-- Result of applying an action to tree state. -/
structure TreeActionResult (α : Type) where
  /-- The new state. -/
  state : FullTreeState α
  /-- Whether a toggle occurred (for toggle event). -/
  toggled : Option (Array Nat) := none
  /-- Whether a leaf was selected (for select event). -/
  selected : Option α := none

/-- Apply a tree action to the state. -/
def applyTreeAction [Inhabited α] (action : TreeAction α) (s : FullTreeState α)
    (maxVis : Nat) : TreeActionResult α := Id.run do
  let flat := s.flatten
  if flat.isEmpty then return { state := s }

  let itemCount := flat.size
  let currentLine := flat[s.nav.selectedIndex]?

  match action with
  | .moveUp =>
    let newNav := s.nav.moveUp itemCount |>.adjustScroll maxVis
    return { state := { s with nav := newNav } }

  | .moveDown =>
    let newNav := s.nav.moveDown itemCount |>.adjustScroll maxVis
    return { state := { s with nav := newNav } }

  | .moveToFirst =>
    let newNav := s.nav.moveToFirst.adjustScroll maxVis
    return { state := { s with nav := newNav } }

  | .moveToLast =>
    let newNav := s.nav.moveToLast itemCount |>.adjustScroll maxVis
    return { state := { s with nav := newNav } }

  | .left =>
    match currentLine with
    | none => return { state := s }
    | some line =>
      if !line.isLeaf && line.isExpanded then
        -- Collapse the branch
        let newRoots := collapseAtPath s.roots line.path
        return { state := { s with roots := newRoots }, toggled := some line.path }
      else if line.path.size > 1 then
        -- Move to parent
        let parentPath := line.path.pop
        match flat.findIdx? (fun l => l.path == parentPath) with
        | some parentIdx =>
          let newNav := { s.nav with selectedIndex := parentIdx }.adjustScroll maxVis
          return { state := { s with nav := newNav } }
        | none => return { state := s }
      else
        return { state := s }

  | .right =>
    match currentLine with
    | none => return { state := s }
    | some line =>
      if !line.isLeaf then
        if !line.isExpanded then
          -- Expand the branch
          let newRoots := expandAtPath s.roots line.path
          return { state := { s with roots := newRoots }, toggled := some line.path }
        else
          -- Move to first child (next item in flat list)
          let newNav := s.nav.moveDown itemCount |>.adjustScroll maxVis
          return { state := { s with nav := newNav } }
      else
        return { state := s }

  | .activate =>
    match currentLine with
    | none => return { state := s }
    | some line =>
      if line.isLeaf then
        -- Select the leaf
        return { state := s, selected := some line.value }
      else
        -- Toggle the branch
        let newRoots := toggleAtPath s.roots line.path
        return { state := { s with roots := newRoots }, toggled := some line.path }

/-- Create a tree widget.

    The widget handles:
    - Up/Down (j/k) for navigation
    - Left to collapse or move to parent
    - Right to expand or move to first child
    - Enter/Space to select (fires onSelect) or toggle (fires onToggle)
    - Home/End to jump to first/last

    Example:
    ```
    let root := TreeNode.branch "Root" #[
      TreeNode.leaf "Child 1",
      TreeNode.branch "Child 2" #[
        TreeNode.leaf "Grandchild"
      ]
    ]
    let tree ← tree' root {}
    -- Use tree.selectedNode to get current selection
    -- Use tree.onSelect for Enter key
    -- Use tree.onToggle for expand/collapse
    ```
-/
def tree' [Inhabited α] [ToString α] [BEq α] (root : TreeNode α) (config : TreeConfig := {})
    : WidgetM (TreeResult α) := do
  -- Register as focusable component
  let widgetName ← registerComponentW "tree" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the tree's focus name before calling useFocusedKeyEventsW
  let treeName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW treeName config.globalKeys

  -- Initial state
  let initialState : FullTreeState α := { nav := { selectedIndex := 0, scrollOffset := 0 }, roots := #[root] }

  -- Map key events to tree actions
  let actionEvents ← Event.mapMaybeM (fun (kd : KeyData) =>
    match kd.event.code with
    | .up | .char 'k' => some (TreeAction.moveUp : TreeAction α)
    | .down | .char 'j' => some (TreeAction.moveDown : TreeAction α)
    | .home => some (TreeAction.moveToFirst : TreeAction α)
    | .end => some (TreeAction.moveToLast : TreeAction α)
    | .left => some (TreeAction.left : TreeAction α)
    | .right => some (TreeAction.right : TreeAction α)
    | .enter | .space => some (TreeAction.activate : TreeAction α)
    | _ => none) keyEvents

  -- Fold actions into state AND result (so we can extract events from the result)
  let stateWithResultDyn ← foldDyn (fun (action : TreeAction α) (pair : FullTreeState α × Option (TreeActionResult α)) =>
    let s := pair.1
    let maxVis := config.maxVisible.getD (s.flatten.size)
    let result := applyTreeAction action s maxVis
    (result.state, some result)
  ) (initialState, none) actionEvents

  -- Get just the state
  let stateDyn ← stateWithResultDyn.map' (·.1)

  -- Extract results from the update event (result is computed at fold time, not after)
  let actionResults ← Event.mapMaybeM (fun (pair : FullTreeState α × Option (TreeActionResult α)) =>
    pair.2) stateWithResultDyn.updated

  -- Extract toggle events
  let toggleEvent ← Event.mapMaybeM (fun (result : TreeActionResult α) =>
    result.toggled) actionResults

  -- Extract select events (leaf selection)
  let selectEvent ← Event.mapMaybeM (fun (result : TreeActionResult α) =>
    result.selected) actionResults

  -- Derive selected path from state
  let selectedPathDyn ← stateDyn.map' (fun (s : FullTreeState α) => s.selectedPath)

  -- Derive selected node from state
  let selectedNodeDyn ← stateDyn.map' (fun (s : FullTreeState α) => s.selectedNode)

  -- Emit render function
  let node ← stateDyn.map' fun (s : FullTreeState α) =>
    renderTreeView s.roots s.nav config
  emit node

  pure {
    selectedPath := selectedPathDyn
    selectedNode := selectedNodeDyn
    onSelect := selectEvent
    onToggle := toggleEvent
  }

/-- Create a tree from multiple root nodes. -/
def forest' [Inhabited α] [ToString α] [BEq α] (roots : Array (TreeNode α)) (config : TreeConfig := {})
    : WidgetM (TreeResult α) := do
  let widgetName ← registerComponentW "forest" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the tree's focus name before calling useFocusedKeyEventsW
  let treeName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW treeName config.globalKeys

  -- Initial state
  let initialState : FullTreeState α := { nav := { selectedIndex := 0, scrollOffset := 0 }, roots := roots }

  -- Map key events to tree actions
  let actionEvents ← Event.mapMaybeM (fun (kd : KeyData) =>
    match kd.event.code with
    | .up | .char 'k' => some (TreeAction.moveUp : TreeAction α)
    | .down | .char 'j' => some (TreeAction.moveDown : TreeAction α)
    | .home => some (TreeAction.moveToFirst : TreeAction α)
    | .end => some (TreeAction.moveToLast : TreeAction α)
    | .left => some (TreeAction.left : TreeAction α)
    | .right => some (TreeAction.right : TreeAction α)
    | .enter | .space => some (TreeAction.activate : TreeAction α)
    | _ => none) keyEvents

  -- Fold actions into state AND result (so we can extract events from the result)
  let stateWithResultDyn ← foldDyn (fun (action : TreeAction α) (pair : FullTreeState α × Option (TreeActionResult α)) =>
    let s := pair.1
    let maxVis := config.maxVisible.getD (s.flatten.size)
    let result := applyTreeAction action s maxVis
    (result.state, some result)
  ) (initialState, none) actionEvents

  -- Get just the state
  let stateDyn ← stateWithResultDyn.map' (·.1)

  -- Extract results from the update event (result is computed at fold time, not after)
  let actionResults ← Event.mapMaybeM (fun (pair : FullTreeState α × Option (TreeActionResult α)) =>
    pair.2) stateWithResultDyn.updated

  -- Extract toggle events
  let toggleEvent ← Event.mapMaybeM (fun (result : TreeActionResult α) =>
    result.toggled) actionResults

  -- Extract select events (leaf selection)
  let selectEvent ← Event.mapMaybeM (fun (result : TreeActionResult α) =>
    result.selected) actionResults

  -- Derive selected path from state
  let selectedPathDyn ← stateDyn.map' (fun (s : FullTreeState α) => s.selectedPath)

  -- Derive selected node from state
  let selectedNodeDyn ← stateDyn.map' (fun (s : FullTreeState α) => s.selectedNode)

  -- Emit render function
  let node ← stateDyn.map' fun (s : FullTreeState α) =>
    renderTreeView s.roots s.nav config
  emit node

  pure {
    selectedPath := selectedPathDyn
    selectedNode := selectedNodeDyn
    onSelect := selectEvent
    onToggle := toggleEvent
  }

end Terminus.Reactive
