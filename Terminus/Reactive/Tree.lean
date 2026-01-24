/-
  Terminus Reactive - Tree Component
  Hierarchical tree view with expand/collapse and keyboard navigation.

  **State Management**: Tree state (selection, expansion) persists across data updates.
  When the data Dynamic updates, the widget adjusts its internal state (clamps selection,
  prunes invalid expansion paths) without losing user interactions.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive
import Std.Data.HashMap

open Reactive Reactive.Host
open Std (HashMap)

namespace Terminus.Reactive

/-! ## Tree Data Structure -/

/-- Pure structural tree data without embedded expansion state.
    Use this with the dynamic tree widget for state that persists across data updates. -/
structure TreeData (α : Type) where
  /-- Value stored in this node. -/
  value : α
  /-- Child nodes. -/
  children : Array (TreeData α) := #[]
  deriving Repr, Inhabited

-- BEq instance for TreeData (compares value and children size only for efficiency)
instance [BEq α] : BEq (TreeData α) where
  beq a b := a.value == b.value && a.children.size == b.children.size

-- Nonempty instance for TreeData (needed for partial functions)
instance [Inhabited α] : Nonempty (TreeData α) := ⟨default⟩

namespace TreeData

/-- Create a leaf node (no children). -/
def leaf (value : α) : TreeData α := { value, children := #[] }

/-- Create a branch node with children. -/
def branch (value : α) (children : Array (TreeData α)) : TreeData α :=
  { value, children }

/-- Check if this is a leaf (no children). -/
def isLeaf (node : TreeData α) : Bool := node.children.isEmpty

/-- Check if this is a branch (has children). -/
def isBranch (node : TreeData α) : Bool := !node.children.isEmpty

/-- Count total nodes in tree. -/
partial def size (node : TreeData α) : Nat :=
  1 + node.children.foldl (fun acc child => acc + child.size) 0

end TreeData

/-! ## Legacy TreeNode (Deprecated) -/

/-- A node in the tree hierarchy.
    **Deprecated**: Use `TreeData` with dynamic tree widgets instead.
    This type embeds expansion state which doesn't work well with FRP. -/
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

/-- Convert TreeNode to TreeData (strips expansion state). -/
partial def toTreeData [Inhabited α] (node : TreeNode α) : TreeData α :=
  { value := node.value, children := node.children.map toTreeData }

end TreeNode

/-! ## Expansion State -/

/-- Expansion state stored by path. Paths not in the map use the default expansion. -/
abbrev ExpansionState := HashMap (Array Nat) Bool

namespace ExpansionState

/-- Create empty expansion state. -/
def empty : ExpansionState := {}

/-- Check if a path is expanded (defaults to true if not set). -/
def isExpanded (state : ExpansionState) (path : Array Nat) (default : Bool := true) : Bool :=
  state.get? path |>.getD default

/-- Toggle expansion at a path. -/
def toggle (state : ExpansionState) (path : Array Nat) (default : Bool := true) : ExpansionState :=
  let current := state.isExpanded path default
  state.insert path (!current)

/-- Set expansion at a path. -/
def set (state : ExpansionState) (path : Array Nat) (expanded : Bool) : ExpansionState :=
  state.insert path expanded

/-- Prune paths that are no longer valid for the given data.
    This is called when data updates to remove stale expansion state. -/
partial def prune [Inhabited α] (state : ExpansionState) (roots : Array (TreeData α)) : ExpansionState :=
  state.fold (init := {}) fun acc path expanded =>
    if isValidPath roots path then acc.insert path expanded else acc
where
  isValidPath (nodes : Array (TreeData α)) (path : Array Nat) : Bool :=
    match path.toList with
    | [] => true
    | [i] => i < nodes.size
    | i :: rest =>
      match nodes[i]? with
      | some node => isValidPath node.children rest.toArray
      | none => false

end ExpansionState

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
structure TreeConfig (α : Type) where
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
  /-- Initial selection: if provided, tree will start with this item selected. -/
  initialSelection : Option α := none
  /-- Whether branches start expanded by default. -/
  expandByDefault : Bool := true
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

/-! ## Tree Navigation State -/

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

/-- Clamp selection to valid range for item count. -/
def clamp (s : TreeState) (itemCount : Nat) : TreeState :=
  if itemCount == 0 then { s with selectedIndex := 0 }
  else { s with selectedIndex := min s.selectedIndex (itemCount - 1) }

end TreeState

/-! ## Internal Tree State -/

/-- Internal state for the dynamic tree widget.
    Combines navigation state with expansion state. -/
structure TreeInternalState where
  /-- Navigation state (selection, scroll). -/
  nav : TreeState := {}
  /-- Expansion state (which branches are expanded/collapsed). -/
  expansion : ExpansionState := ExpansionState.empty
  deriving Inhabited

instance : BEq TreeInternalState where
  beq a b := a.nav == b.nav && a.expansion.size == b.expansion.size

/-! ## Legacy FullTreeState (for flattenForest tests) -/

/-- Combined state for tree widget: navigation state + tree structure.
    **Deprecated**: Only used by flattenForest helper tests. -/
structure FullTreeState (α : Type) where
  /-- Navigation state (selection, scroll). -/
  nav : TreeState := {}
  /-- Tree roots (structure with expand/collapse state). -/
  roots : Array (TreeNode α) := #[]
  deriving Inhabited

/-! ## Tree Flattening Helpers -/

/-- Flatten a TreeData node into visible lines using external expansion state. -/
partial def flattenTreeData [Inhabited α] (node : TreeData α) (depth : Nat) (path : Array Nat)
    (isLast : Bool) (expansion : ExpansionState) (expandByDefault : Bool) : Array (FlatLine α) := Id.run do
  let mut result : Array (FlatLine α) := #[]

  let isExpanded := if node.isLeaf then true else expansion.isExpanded path expandByDefault

  -- Add this node
  result := result.push {
    value := node.value
    depth := depth
    path := path
    isLeaf := node.isLeaf
    isExpanded := isExpanded
    isLast := isLast
  }

  -- Add children if expanded
  if isExpanded && !node.children.isEmpty then
    for h : i in [:node.children.size] do
      let child := node.children[i]
      let childPath := path.push i
      let childIsLast := i == node.children.size - 1
      result := result ++ flattenTreeData child (depth + 1) childPath childIsLast expansion expandByDefault

  result

/-- Flatten a forest (array of TreeData nodes) using external expansion state. -/
def flattenForestData [Inhabited α] (roots : Array (TreeData α)) (expansion : ExpansionState)
    (expandByDefault : Bool := true) : Array (FlatLine α) := Id.run do
  let mut result : Array (FlatLine α) := #[]
  for h : i in [:roots.size] do
    let root := roots[i]
    let isLast := i == roots.size - 1
    result := result ++ flattenTreeData root 0 #[i] isLast expansion expandByDefault
  result

/-- Get node value at path in TreeData forest. -/
partial def getNodeAtPathData [Inhabited α] (roots : Array (TreeData α)) (path : Array Nat) : Option α :=
  match path.toList with
  | [] => none
  | [idx] => roots[idx]?.map (·.value)
  | idx :: rest =>
    match roots[idx]? with
    | none => none
    | some node => getNodeAtPathInNode node rest
where
  getNodeAtPathInNode (node : TreeData α) : List Nat → Option α
    | [] => some node.value
    | [idx] => node.children[idx]?.map (·.value)
    | idx :: rest =>
      match node.children[idx]? with
      | none => none
      | some child => getNodeAtPathInNode child rest

/-- Check if a path points to a leaf in TreeData forest. -/
partial def isLeafAtPath [Inhabited α] (roots : Array (TreeData α)) (path : Array Nat) : Bool :=
  match path.toList with
  | [] => false
  | [idx] => roots[idx]?.map (·.isLeaf) |>.getD true
  | idx :: rest =>
    match roots[idx]? with
    | none => true
    | some node => isLeafAtPathInNode node rest
where
  isLeafAtPathInNode (node : TreeData α) : List Nat → Bool
    | [] => node.isLeaf
    | [idx] => node.children[idx]?.map (·.isLeaf) |>.getD true
    | idx :: rest =>
      match node.children[idx]? with
      | none => true
      | some child => isLeafAtPathInNode child rest

/-! ## Legacy TreeNode Flattening (for backward compatibility) -/

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

/-! ## Rendering -/

private def renderTreeViewData [Inhabited α] [ToString α]
    (roots : Array (TreeData α)) (nav : TreeState) (expansion : ExpansionState)
    (config : TreeConfig α) : RNode :=
  Id.run do
    let flat := flattenForestData roots expansion config.expandByDefault

    if flat.isEmpty then
      return RNode.text "(empty)" config.style
    else
      let maxVis := config.maxVisible.getD flat.size

      -- Calculate visible range
      let startIdx := nav.scrollOffset
      let endIdx := min (startIdx + maxVis) flat.size

      -- Build visible lines
      let mut rows : Array RNode := #[]

      for i in [startIdx:endIdx] do
        if h : i < flat.size then
          let line := flat[i]
          let isSelected := i == nav.selectedIndex

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

/-! ## Legacy Path Manipulation (for backward compatibility) -/

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

/-! ## Tree Actions -/

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
  /-- Data has changed - adjust state. -/
  | dataChanged (newData : Array (TreeData α))

/-- Result of applying an action to tree internal state. -/
structure InternalActionResult (α : Type) where
  /-- The new state. -/
  state : TreeInternalState
  /-- Whether a toggle occurred (for toggle event). -/
  toggled : Option (Array Nat) := none
  /-- Whether a leaf was selected (for select event). -/
  selected : Option α := none

/-- Apply a tree action to the internal state. -/
def applyTreeActionInternal [Inhabited α] (action : TreeAction α) (s : TreeInternalState)
    (data : Array (TreeData α)) (config : TreeConfig α) : InternalActionResult α := Id.run do
  let flat := flattenForestData data s.expansion config.expandByDefault
  let maxVis := config.maxVisible.getD flat.size

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
        let newExpansion := s.expansion.set line.path false
        return { state := { s with expansion := newExpansion }, toggled := some line.path }
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
          let newExpansion := s.expansion.set line.path true
          return { state := { s with expansion := newExpansion }, toggled := some line.path }
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
        let newExpansion := s.expansion.toggle line.path config.expandByDefault
        return { state := { s with expansion := newExpansion }, toggled := some line.path }

  | .dataChanged newData =>
    -- Data has changed - prune invalid expansion paths and clamp selection
    let prunedExpansion := s.expansion.prune newData
    let newFlat := flattenForestData newData prunedExpansion config.expandByDefault
    let clampedNav := s.nav.clamp newFlat.size |>.adjustScroll maxVis
    return { state := { nav := clampedNav, expansion := prunedExpansion } }

/-! ## Dynamic Tree Widget -/

/-- Create a forest widget with a dynamic data source.

    **State persists across data updates**: Selection, expansion, and scroll position
    are maintained when the data changes. Invalid state is automatically adjusted
    (selection clamped, invalid expansion paths pruned).

    Example:
    ```
    let dataDyn ← Dynamic.pureM #[
      TreeData.branch "Project A" #[...],
      TreeData.branch "Project B" #[...]
    ]
    let forest ← forestDyn' dataDyn {}
    ```
-/
def forestDyn' [Inhabited α] [ToString α] [BEq α]
    (dataDyn : Dynamic Spider (Array (TreeData α))) (config : TreeConfig α := {})
    : WidgetM (TreeResult α) := do
  -- Register as focusable component
  let widgetName ← registerComponentW "forest" (isInput := true)
    (nameOverride := config.focusName)

  let treeName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW treeName config.globalKeys

  -- Map key events to tree actions
  let keyActionEvents ← Event.mapMaybeM (fun (kd : KeyData) =>
    match kd.event.code with
    | .up | .char 'k' => some (TreeAction.moveUp : TreeAction α)
    | .down | .char 'j' => some (TreeAction.moveDown : TreeAction α)
    | .home => some (TreeAction.moveToFirst : TreeAction α)
    | .end => some (TreeAction.moveToLast : TreeAction α)
    | .left => some (TreeAction.left : TreeAction α)
    | .right => some (TreeAction.right : TreeAction α)
    | .enter | .space => some (TreeAction.activate : TreeAction α)
    | _ => none) keyEvents

  -- Map data changes to dataChanged actions
  let dataChangeActions ← Event.mapM (fun data => TreeAction.dataChanged data) dataDyn.updated

  -- Merge action streams (key actions take priority over data changes)
  let allActions ← Event.leftmostM [keyActionEvents, dataChangeActions]

  -- Get initial data for initial state setup
  let initialData ← sample dataDyn.current
  let initialFlat := flattenForestData initialData ExpansionState.empty config.expandByDefault
  let initialIndex := match config.initialSelection with
    | some target => initialFlat.findIdx? (·.value == target) |>.getD 0
    | none => 0

  -- Initial internal state
  let initialState : TreeInternalState := {
    nav := { selectedIndex := initialIndex, scrollOffset := 0 }
    expansion := ExpansionState.empty
  }

  -- Use fixDynM for self-referential state:
  -- Actions need current data AND current state to compute results
  let stateWithResultDyn ← SpiderM.fixDynM fun stateBehavior => do
    -- Extract just the state (first element of tuple)
    let pureStateBehavior := stateBehavior.map (·.1)

    -- Attach current state AND current data to action events
    let actionsWithContext ← Event.attachWithM
      (fun (state, data) action => (action, state, data))
      (Behavior.zipWith (·, ·) pureStateBehavior dataDyn.current)
      allActions

    -- Map actions to state updates
    let updates ← Event.mapM (fun (action, state, data) =>
      let result := applyTreeActionInternal action state data config
      (result.state, some result)) actionsWithContext

    -- Fold updates into state
    holdDyn (initialState, none) updates

  -- Get just the state
  let stateDyn ← stateWithResultDyn.map' (·.1)

  -- Extract action results from update events
  let actionResults ← Event.mapMaybeM (fun (pair : TreeInternalState × Option (InternalActionResult α)) =>
    pair.2) stateWithResultDyn.updated

  -- Extract toggle events
  let toggleEvent ← Event.mapMaybeM (fun (result : InternalActionResult α) =>
    result.toggled) actionResults

  -- Extract select events (leaf selection)
  let selectEvent ← Event.mapMaybeM (fun (result : InternalActionResult α) =>
    result.selected) actionResults

  -- Combine data and state for derived dynamics
  let dataStateDyn ← dataDyn.zipWith' (fun data state => (data, state)) stateDyn

  -- Derive selected path from state
  let selectedPathDyn ← dataStateDyn.map' fun (data, state) =>
    let flat := flattenForestData data state.expansion config.expandByDefault
    flat[state.nav.selectedIndex]?.map (·.path) |>.getD #[]

  -- Derive selected node from state
  let selectedNodeDyn ← dataStateDyn.map' fun (data, state) =>
    let flat := flattenForestData data state.expansion config.expandByDefault
    flat[state.nav.selectedIndex]?.map (·.value)

  -- Emit render function
  let node ← dataStateDyn.map' fun (data, state) =>
    renderTreeViewData data state.nav state.expansion config
  emit node

  pure {
    selectedPath := selectedPathDyn
    selectedNode := selectedNodeDyn
    onSelect := selectEvent
    onToggle := toggleEvent
  }

/-- Create a tree widget with a dynamic data source.

    **State persists across data updates**: Selection, expansion, and scroll position
    are maintained when the data changes. Invalid state is automatically adjusted
    (selection clamped, invalid expansion paths pruned).

    The widget handles:
    - Up/Down (j/k) for navigation
    - Left to collapse or move to parent
    - Right to expand or move to first child
    - Enter/Space to select (fires onSelect) or toggle (fires onToggle)
    - Home/End to jump to first/last

    Example:
    ```
    let dataDyn ← Dynamic.pureM (TreeData.branch "Root" #[
      TreeData.leaf "Child 1",
      TreeData.branch "Child 2" #[
        TreeData.leaf "Grandchild"
      ]
    ])
    let tree ← treeDyn' dataDyn {}
    -- Use tree.selectedNode to get current selection
    -- Use tree.onSelect for Enter key
    -- Use tree.onToggle for expand/collapse
    ```
-/
def treeDyn' [Inhabited α] [ToString α] [BEq α]
    (dataDyn : Dynamic Spider (TreeData α)) (config : TreeConfig α := {})
    : WidgetM (TreeResult α) := do
  -- Convert single root to forest
  let forestDyn ← dataDyn.map' (fun root => #[root])
  forestDyn' forestDyn config

end Terminus.Reactive
