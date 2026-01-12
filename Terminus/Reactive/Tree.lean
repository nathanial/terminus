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
  deriving Repr, Inhabited

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
def tree' [Inhabited α] [ToString α] (root : TreeNode α) (config : TreeConfig := {})
    : WidgetM (TreeResult α) := do
  let events ← getEventsW

  -- Register as focusable component
  let widgetName ← registerComponentW "tree" (isInput := true)
    (nameOverride := config.focusName)
  let focusedInput ← useFocusedInputW

  -- Create trigger events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := α)
  let (toggleEvent, fireToggle) ← newTriggerEvent (t := Spider) (a := Array Nat)
  let (pathEvent, firePath) ← newTriggerEvent (t := Spider) (a := Array Nat)
  let (nodeEvent, fireNode) ← newTriggerEvent (t := Spider) (a := Option α)

  -- Track state
  let initialState := TreeState.mk 0 0
  let initialRoots := #[root]
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
  let rootsRef ← SpiderM.liftIO (IO.mkRef initialRoots)
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := TreeState)
  let stateDyn ← holdDyn initialState stateEvent
  let (rootsEvent, fireRoots) ← newTriggerEvent (t := Spider) (a := Array (TreeNode α))
  let rootsDyn ← holdDyn initialRoots rootsEvent

  -- Initial flattening
  let initialFlat := flattenTree root 0 #[0] true
  let initialPath := if initialFlat.isEmpty then #[] else initialFlat[0]!.path
  let initialNode := if initialFlat.isEmpty then none else some initialFlat[0]!.value

  -- Create dynamics
  let selectedPathDyn ← holdDyn initialPath pathEvent
  let selectedNodeDyn ← holdDyn initialNode nodeEvent

  -- Determine the tree's focus name
  let treeName := if config.focusName.isEmpty then widgetName else config.focusName

  let updateState : TreeState → IO Unit := fun newState => do
    stateRef.set newState
    fireState newState

  let updateRoots : Array (TreeNode α) → IO Unit := fun newRoots => do
    rootsRef.set newRoots
    fireRoots newRoots

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    let currentFocus ← focusedInput.sample
    let isFocused := config.globalKeys || currentFocus == some treeName

    let roots ← rootsRef.get
    let flat := flattenForest roots

    if flat.isEmpty || !isFocused then pure ()
    else
      let state ← stateRef.get
      let ke := kd.event
      let maxVis := config.maxVisible.getD flat.size
      let currentLine := flat[state.selectedIndex]?

      match ke.code with
      | .up | .char 'k' =>
        let newState := state.moveUp flat.size |>.adjustScroll maxVis
        if newState.selectedIndex != state.selectedIndex then
          updateState newState
          if h : newState.selectedIndex < flat.size then
            let line := flat[newState.selectedIndex]
            firePath line.path
            fireNode (some line.value)
      | .down | .char 'j' =>
        let newState := state.moveDown flat.size |>.adjustScroll maxVis
        if newState.selectedIndex != state.selectedIndex then
          updateState newState
          if h : newState.selectedIndex < flat.size then
            let line := flat[newState.selectedIndex]
            firePath line.path
            fireNode (some line.value)
      | .home =>
        let newState := state.moveToFirst.adjustScroll maxVis
        if newState.selectedIndex != state.selectedIndex then
          updateState newState
          if h : 0 < flat.size then
            let line := flat[0]
            firePath line.path
            fireNode (some line.value)
      | .end =>
        let newState := state.moveToLast flat.size |>.adjustScroll maxVis
        if newState.selectedIndex != state.selectedIndex then
          updateState newState
          if h : newState.selectedIndex < flat.size then
            let line := flat[newState.selectedIndex]
            firePath line.path
            fireNode (some line.value)
      | .left =>
        -- Collapse current branch or move to parent
        match currentLine with
        | none => pure ()
        | some line =>
          if !line.isLeaf && line.isExpanded then
            -- Collapse
            let newRoots := collapseAtPath roots line.path
            updateRoots newRoots
            fireToggle line.path
          else if line.path.size > 1 then
            -- Move to parent (find parent's index in flat list)
            let parentPath := line.path.pop
            match flat.findIdx? (fun l => l.path == parentPath) with
            | some parentIdx =>
              let newState := { state with selectedIndex := parentIdx }.adjustScroll maxVis
              updateState newState
              if h : parentIdx < flat.size then
                let parentLine := flat[parentIdx]
                firePath parentLine.path
                fireNode (some parentLine.value)
            | none => pure ()
      | .right =>
        -- Expand current branch or move to first child
        match currentLine with
        | none => pure ()
        | some line =>
          if !line.isLeaf then
            if !line.isExpanded then
              -- Expand
              let newRoots := expandAtPath roots line.path
              updateRoots newRoots
              fireToggle line.path
            else
              -- Move to first child (next item in flat list)
              let newState := state.moveDown flat.size |>.adjustScroll maxVis
              if newState.selectedIndex != state.selectedIndex then
                updateState newState
                if h : newState.selectedIndex < flat.size then
                  let childLine := flat[newState.selectedIndex]
                  firePath childLine.path
                  fireNode (some childLine.value)
      | .enter | .space =>
        match currentLine with
        | none => pure ()
        | some line =>
          if line.isLeaf then
            fireSelect line.value
          else
            -- Toggle branch
            let newRoots := toggleAtPath roots line.path
            updateRoots newRoots
            fireToggle line.path
      | _ => pure ()

  -- Emit render function
  let node ← stateDyn.zipWith' (fun state currentRoots =>
    renderTreeView currentRoots state config
  ) rootsDyn
  emit node

  pure {
    selectedPath := selectedPathDyn
    selectedNode := selectedNodeDyn
    onSelect := selectEvent
    onToggle := toggleEvent
  }

/-- Create a tree from multiple root nodes. -/
def forest' [Inhabited α] [ToString α] (roots : Array (TreeNode α)) (config : TreeConfig := {})
    : WidgetM (TreeResult α) := do
  let events ← getEventsW

  let widgetName ← registerComponentW "forest" (isInput := true)
    (nameOverride := config.focusName)
  let focusedInput ← useFocusedInputW

  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := α)
  let (toggleEvent, fireToggle) ← newTriggerEvent (t := Spider) (a := Array Nat)
  let (pathEvent, firePath) ← newTriggerEvent (t := Spider) (a := Array Nat)
  let (nodeEvent, fireNode) ← newTriggerEvent (t := Spider) (a := Option α)

  let initialState := TreeState.mk 0 0
  let initialRoots := roots
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
  let rootsRef ← SpiderM.liftIO (IO.mkRef initialRoots)
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := TreeState)
  let stateDyn ← holdDyn initialState stateEvent
  let (rootsEvent, fireRoots) ← newTriggerEvent (t := Spider) (a := Array (TreeNode α))
  let rootsDyn ← holdDyn initialRoots rootsEvent

  let initialFlat := flattenForest roots
  let initialPath := if initialFlat.isEmpty then #[] else initialFlat[0]!.path
  let initialNode := if initialFlat.isEmpty then none else some initialFlat[0]!.value

  let selectedPathDyn ← holdDyn initialPath pathEvent
  let selectedNodeDyn ← holdDyn initialNode nodeEvent

  let treeName := if config.focusName.isEmpty then widgetName else config.focusName

  let updateState : TreeState → IO Unit := fun newState => do
    stateRef.set newState
    fireState newState

  let updateRoots : Array (TreeNode α) → IO Unit := fun newRoots => do
    rootsRef.set newRoots
    fireRoots newRoots

  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    let currentFocus ← focusedInput.sample
    let isFocused := config.globalKeys || currentFocus == some treeName

    let currentRoots ← rootsRef.get
    let flat := flattenForest currentRoots

    if flat.isEmpty || !isFocused then pure ()
    else
      let state ← stateRef.get
      let ke := kd.event
      let maxVis := config.maxVisible.getD flat.size
      let currentLine := flat[state.selectedIndex]?

      match ke.code with
      | .up | .char 'k' =>
        let newState := state.moveUp flat.size |>.adjustScroll maxVis
        if newState.selectedIndex != state.selectedIndex then
          updateState newState
          if h : newState.selectedIndex < flat.size then
            let line := flat[newState.selectedIndex]
            firePath line.path
            fireNode (some line.value)
      | .down | .char 'j' =>
        let newState := state.moveDown flat.size |>.adjustScroll maxVis
        if newState.selectedIndex != state.selectedIndex then
          updateState newState
          if h : newState.selectedIndex < flat.size then
            let line := flat[newState.selectedIndex]
            firePath line.path
            fireNode (some line.value)
      | .home =>
        let newState := state.moveToFirst.adjustScroll maxVis
        updateState newState
        if h : 0 < flat.size then
          let line := flat[0]
          firePath line.path
          fireNode (some line.value)
      | .end =>
        let newState := state.moveToLast flat.size |>.adjustScroll maxVis
        updateState newState
        if h : newState.selectedIndex < flat.size then
          let line := flat[newState.selectedIndex]
          firePath line.path
          fireNode (some line.value)
      | .left =>
        match currentLine with
        | none => pure ()
        | some line =>
          if !line.isLeaf && line.isExpanded then
            let newRoots := collapseAtPath currentRoots line.path
            updateRoots newRoots
            fireToggle line.path
          else if line.path.size > 1 then
            let parentPath := line.path.pop
            match flat.findIdx? (fun l => l.path == parentPath) with
            | some parentIdx =>
              let newState := { state with selectedIndex := parentIdx }.adjustScroll maxVis
              updateState newState
              if h : parentIdx < flat.size then
                let parentLine := flat[parentIdx]
                firePath parentLine.path
                fireNode (some parentLine.value)
            | none => pure ()
      | .right =>
        match currentLine with
        | none => pure ()
        | some line =>
          if !line.isLeaf then
            if !line.isExpanded then
              let newRoots := expandAtPath currentRoots line.path
              updateRoots newRoots
              fireToggle line.path
            else
              let newState := state.moveDown flat.size |>.adjustScroll maxVis
              if newState.selectedIndex != state.selectedIndex then
                updateState newState
                if h : newState.selectedIndex < flat.size then
                  let childLine := flat[newState.selectedIndex]
                  firePath childLine.path
                  fireNode (some childLine.value)
      | .enter | .space =>
        match currentLine with
        | none => pure ()
        | some line =>
          if line.isLeaf then
            fireSelect line.value
          else
            let newRoots := toggleAtPath currentRoots line.path
            updateRoots newRoots
            fireToggle line.path
      | _ => pure ()

  let node ← stateDyn.zipWith' (fun state currentRoots =>
    renderTreeView currentRoots state config
  ) rootsDyn
  emit node

  pure {
    selectedPath := selectedPathDyn
    selectedNode := selectedNodeDyn
    onSelect := selectEvent
    onToggle := toggleEvent
  }

end Terminus.Reactive
