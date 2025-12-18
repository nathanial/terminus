-- Terminus.Widgets.Tree: Hierarchical tree view with expand/collapse

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- A node in the tree (leaf or branch) -/
inductive TreeNode where
  | leaf (label : String) (data : Option String := none)
  | branch (label : String) (children : List TreeNode) (expanded : Bool := true)
  deriving Repr, Inhabited

namespace TreeNode

def mkLeaf (label : String) : TreeNode := .leaf label none
def mkBranch (label : String) (children : List TreeNode) : TreeNode := .branch label children true
def mkCollapsed (label : String) (children : List TreeNode) : TreeNode := .branch label children false

def label : TreeNode → String
  | .leaf l _ => l
  | .branch l _ _ => l

def isLeaf : TreeNode → Bool
  | .leaf _ _ => true
  | .branch _ _ _ => false

def isBranch : TreeNode → Bool
  | .leaf _ _ => false
  | .branch _ _ _ => true

def isExpanded : TreeNode → Bool
  | .leaf _ _ => false
  | .branch _ _ e => e

def children : TreeNode → List TreeNode
  | .leaf _ _ => []
  | .branch _ cs _ => cs

def toggle : TreeNode → TreeNode
  | .leaf l d => .leaf l d
  | .branch l cs e => .branch l cs (!e)

def expand : TreeNode → TreeNode
  | .leaf l d => .leaf l d
  | .branch l cs _ => .branch l cs true

def collapse : TreeNode → TreeNode
  | .leaf l d => .leaf l d
  | .branch l cs _ => .branch l cs false

end TreeNode

/-- Represents a visible line in the flattened tree -/
structure TreeLine where
  label : String
  depth : Nat
  isLeaf : Bool
  isExpanded : Bool
  isLast : Bool  -- Is last child at this level
  path : List Nat  -- Path to this node (indices at each level)
  linePrefix : String  -- Tree drawing prefix (│ ├── └── etc)
  deriving Repr, Inhabited

/-- Tree widget -/
structure Tree where
  nodes : List TreeNode := []
  selected : Nat := 0  -- Index of selected line in visible list
  scrollOffset : Nat := 0
  indentSize : Nat := 2
  expandedChar : String := "▼"
  collapsedChar : String := "▶"
  leafChar : String := "•"
  selectedStyle : Style := Style.reversed
  normalStyle : Style := Style.default
  branchStyle : Style := Style.bold
  leafStyle : Style := Style.default
  prefixStyle : Style := Style.dim
  block : Option Block := none
  deriving Repr, Inhabited

namespace Tree

def new (nodes : List TreeNode) : Tree := { nodes }

def withSelected (t : Tree) (idx : Nat) : Tree := { t with selected := idx }
def withScrollOffset (t : Tree) (offset : Nat) : Tree := { t with scrollOffset := offset }
def withIndentSize (t : Tree) (size : Nat) : Tree := { t with indentSize := size }
def withExpandedChar (t : Tree) (c : String) : Tree := { t with expandedChar := c }
def withCollapsedChar (t : Tree) (c : String) : Tree := { t with collapsedChar := c }
def withLeafChar (t : Tree) (c : String) : Tree := { t with leafChar := c }
def withSelectedStyle (t : Tree) (s : Style) : Tree := { t with selectedStyle := s }
def withNormalStyle (t : Tree) (s : Style) : Tree := { t with normalStyle := s }
def withBranchStyle (t : Tree) (s : Style) : Tree := { t with branchStyle := s }
def withLeafStyle (t : Tree) (s : Style) : Tree := { t with leafStyle := s }
def withPrefixStyle (t : Tree) (s : Style) : Tree := { t with prefixStyle := s }
def withBlock (t : Tree) (b : Block) : Tree := { t with block := some b }

/-- Flatten the tree into visible lines -/
partial def flattenNodes (nodes : List TreeNode) (depth : Nat) (parentPrefix : String) (parentPath : List Nat) : List TreeLine := Id.run do
  let mut result : List TreeLine := []
  let numNodes := nodes.length

  for hi : i in [:numNodes] do
    match nodes[i]? with
    | some node =>
      let isLast := i == numNodes - 1
      let path := parentPath ++ [i]

      -- Build prefix for tree structure
      let connector := if isLast then "└── " else "├── "
      let linePrefix := parentPrefix ++ connector

      -- Build continuation prefix for children
      let childPrefix := parentPrefix ++ (if isLast then "    " else "│   ")

      match node with
      | .leaf label _ =>
        result := result ++ [{
          label := label
          depth := depth
          isLeaf := true
          isExpanded := false
          isLast := isLast
          path := path
          linePrefix := linePrefix
        }]
      | .branch label children expanded =>
        result := result ++ [{
          label := label
          depth := depth
          isLeaf := false
          isExpanded := expanded
          isLast := isLast
          path := path
          linePrefix := linePrefix
        }]
        if expanded then
          result := result ++ flattenNodes children (depth + 1) childPrefix path
    | none => pure ()

  result

/-- Get all visible lines -/
def visibleLines (t : Tree) : List TreeLine :=
  flattenNodes t.nodes 0 "" []

/-- Get the number of visible lines -/
def visibleCount (t : Tree) : Nat :=
  t.visibleLines.length

/-- Move selection up -/
def selectPrev (t : Tree) : Tree :=
  if t.selected > 0 then { t with selected := t.selected - 1 }
  else t

/-- Move selection down -/
def selectNext (t : Tree) : Tree :=
  let count := t.visibleCount
  if t.selected + 1 < count then { t with selected := t.selected + 1 }
  else t

/-- Toggle expand/collapse at a path -/
partial def toggleAtPath (nodes : List TreeNode) (path : List Nat) : List TreeNode := Id.run do
  match path with
  | [] => nodes
  | [idx] =>
    let mut result : List TreeNode := []
    for hi : i in [:nodes.length] do
      match nodes[i]? with
      | some node =>
        if i == idx then result := result ++ [node.toggle]
        else result := result ++ [node]
      | none => pure ()
    result
  | idx :: rest =>
    let mut result : List TreeNode := []
    for hi : i in [:nodes.length] do
      match nodes[i]? with
      | some node =>
        if i == idx then
          match node with
          | .leaf l d => result := result ++ [.leaf l d]
          | .branch l cs e => result := result ++ [.branch l (toggleAtPath cs rest) e]
        else result := result ++ [node]
      | none => pure ()
    result

/-- Toggle the currently selected node -/
def toggleSelected (t : Tree) : Tree :=
  let lines := t.visibleLines
  match lines[t.selected]? with
  | none => t
  | some line =>
    if line.isLeaf then t
    else { t with nodes := toggleAtPath t.nodes line.path }

/-- Adjust scroll to keep selection visible -/
def adjustScroll (t : Tree) (visibleHeight : Nat) : Tree :=
  if visibleHeight == 0 then t
  else if t.selected < t.scrollOffset then
    { t with scrollOffset := t.selected }
  else if t.selected >= t.scrollOffset + visibleHeight then
    { t with scrollOffset := t.selected - visibleHeight + 1 }
  else t

/-- Get the selected line (if any) -/
def getSelected (t : Tree) : Option TreeLine :=
  t.visibleLines[t.selected]?

end Tree

instance : Widget Tree where
  render t area buf := Id.run do
    -- Render block if present
    let mut result := match t.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get content area
    let contentArea := match t.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty then return result

    let lines := t.visibleLines
    let visibleHeight := contentArea.height

    -- Adjust scroll offset
    let scrollOffset := if t.selected < t.scrollOffset then t.selected
                        else if t.selected >= t.scrollOffset + visibleHeight then t.selected - visibleHeight + 1
                        else t.scrollOffset

    -- Render visible lines
    for row in [:visibleHeight] do
      let lineIdx := scrollOffset + row
      match lines[lineIdx]? with
      | none => pure ()
      | some line =>
        let y := contentArea.y + row
        let isSelected := lineIdx == t.selected

        -- Build display text
        let icon := if line.isLeaf then t.leafChar
                    else if line.isExpanded then t.expandedChar
                    else t.collapsedChar
        let displayText := line.linePrefix ++ icon ++ " " ++ line.label

        -- Choose style
        let baseStyle := if isSelected then t.selectedStyle
                         else if line.isLeaf then t.leafStyle
                         else t.branchStyle

        -- Render the line
        let mut x := contentArea.x
        let displayChars := displayText.toList
        for hc : c in displayChars do
          if x >= contentArea.x + contentArea.width then break
          result := result.setStyled x y c baseStyle
          x := x + 1

        -- Fill rest of line with spaces if selected (for highlight effect)
        if isSelected then
          while x < contentArea.x + contentArea.width do
            result := result.setStyled x y ' ' t.selectedStyle
            x := x + 1

    result

end Terminus
