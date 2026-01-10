/-
  Terminus Reactive - Render Pipeline
  Converts RNode trees to terminal Buffer output using Trellis layout.
-/
import Terminus.Reactive.Types
import Trellis

namespace Terminus.Reactive

open Trellis

/-! ## ID Generation

We need unique IDs for each RNode when converting to Trellis LayoutNode.
-/

/-- State for generating unique node IDs during tree traversal. -/
structure IdGen where
  nextId : Nat := 0

/-- Get next ID and increment counter. -/
def IdGen.fresh (g : IdGen) : Nat × IdGen :=
  (g.nextId, { nextId := g.nextId + 1 })

/-! ## RNode to LayoutNode Conversion -/

/-- Measure text content size in terminal cells.
    Each character is 1 cell wide (simplified - doesn't handle wide chars). -/
def measureText (content : String) : ContentSize :=
  { width := content.length.toFloat
  , height := 1.0
  , baseline := none }

/-- Convert RNode tree to Trellis LayoutNode tree.
    Returns the LayoutNode and mapping from Trellis node IDs to RNodes. -/
partial def buildLayoutTree (node : RNode) : StateM IdGen (LayoutNode × Array (Nat × RNode)) := do
  let (nodeId, gen) ← get >>= fun g => pure g.fresh
  set gen

  match node with
  | .text content _ =>
    let size := measureText content
    -- Terminal text must be at least 1 cell tall to prevent overlap
    let box : BoxConstraints := { minHeight := 1.0 }
    let layoutNode := LayoutNode.leaf nodeId size (box := box)
    pure (layoutNode, #[(nodeId, node)])

  | .block _ _ _ child =>
    -- Block adds 2 cells for border (1 each side)
    let (childLayout, childMap) ← buildLayoutTree child
    -- Wrap child in a container with padding for border
    let box : BoxConstraints := { padding := EdgeInsets.uniform 1 }
    let layoutNode := LayoutNode.column nodeId #[childLayout] (gap := 0) (box := box)
    pure (layoutNode, #[(nodeId, node)] ++ childMap)

  | .row gap style children =>
    let mut childLayouts : Array LayoutNode := #[]
    let mut allMaps : Array (Nat × RNode) := #[(nodeId, node)]
    for child in children do
      let (childLayout, childMap) ← buildLayoutTree child
      childLayouts := childLayouts.push childLayout
      allMaps := allMaps ++ childMap
    let layoutNode := LayoutNode.row nodeId childLayouts (gap := gap.toFloat) (box := style)
    pure (layoutNode, allMaps)

  | .column gap style children =>
    let mut childLayouts : Array LayoutNode := #[]
    let mut allMaps : Array (Nat × RNode) := #[(nodeId, node)]
    for child in children do
      let (childLayout, childMap) ← buildLayoutTree child
      childLayouts := childLayouts.push childLayout
      allMaps := allMaps ++ childMap
    let layoutNode := LayoutNode.column nodeId childLayouts (gap := gap.toFloat) (box := style)
    pure (layoutNode, allMaps)

  | .spacer width height =>
    let size : ContentSize := { width := width.toFloat, height := height.toFloat, baseline := none }
    -- Spacers should maintain their requested minimum size
    let box : BoxConstraints := { minWidth := width.toFloat, minHeight := height.toFloat }
    let layoutNode := LayoutNode.leaf nodeId size (box := box)
    pure (layoutNode, #[(nodeId, node)])

  | .empty =>
    let size : ContentSize := { width := 0, height := 0, baseline := none }
    let layoutNode := LayoutNode.leaf nodeId size
    pure (layoutNode, #[(nodeId, node)])

/-! ## Coordinate Conversion -/

/-- Convert Float to Nat for terminal coordinates (floor to nearest cell). -/
def floatToCell (f : Float) : Nat :=
  if f < 0 then 0 else f.toUInt32.toNat

/-- Convert Float to Nat using ceiling (for heights where fractional content should still render). -/
def floatToCellCeil (f : Float) : Nat :=
  if f < 0 then 0 else f.ceil.toUInt32.toNat

/-- Convert Trellis LayoutRect to Terminus Rect.
    Uses ceiling for height to ensure text with fractional layout still renders. -/
def toTerminalRect (rect : LayoutRect) : Terminus.Rect :=
  { x := floatToCell rect.x
  , y := floatToCell rect.y
  , width := floatToCell rect.width
  , height := floatToCellCeil rect.height }

/-! ## Buffer Rendering -/

/-- Render a text node to the buffer at the given rect. -/
def renderText (content : String) (style : Style) (rect : Terminus.Rect) (buf : Buffer) : Buffer :=
  if rect.width == 0 || rect.height == 0 then buf
  else
    -- Truncate text to fit width
    let displayText := content.take rect.width
    buf.writeString rect.x rect.y displayText style

/-- Render a block border around the given rect. -/
def renderBlockBorder (title : Option String) (borderType : BorderType) (borderStyle : Style)
    (rect : Terminus.Rect) (buf : Buffer) : Buffer := Id.run do
  if borderType == .none || rect.width < 2 || rect.height < 2 then
    return buf

  let chars := BorderChars.fromType borderType
  let mut result := buf

  -- Draw corners
  result := result.setStyled rect.x rect.y chars.topLeft borderStyle
  result := result.setStyled (rect.x + rect.width - 1) rect.y chars.topRight borderStyle
  result := result.setStyled rect.x (rect.y + rect.height - 1) chars.bottomLeft borderStyle
  result := result.setStyled (rect.x + rect.width - 1) (rect.y + rect.height - 1) chars.bottomRight borderStyle

  -- Draw horizontal borders
  for x in [rect.x + 1 : rect.x + rect.width - 1] do
    result := result.setStyled x rect.y chars.horizontal borderStyle
    result := result.setStyled x (rect.y + rect.height - 1) chars.horizontal borderStyle

  -- Draw vertical borders
  for y in [rect.y + 1 : rect.y + rect.height - 1] do
    result := result.setStyled rect.x y chars.vertical borderStyle
    result := result.setStyled (rect.x + rect.width - 1) y chars.vertical borderStyle

  -- Draw title if present
  match title with
  | some t =>
    if rect.width >= 4 then
      let maxLen := rect.width - 4
      let displayTitle := " " ++ t.take maxLen ++ " "
      result := result.writeString (rect.x + 1) rect.y displayTitle borderStyle
  | none => pure ()

  result

/-- Render an RNode to a Buffer using computed layout.
    - nodeMap: Mapping from Trellis node IDs to RNodes
    - layouts: Computed layout result from Trellis -/
def renderNode (nodeId : Nat) (node : RNode) (layouts : LayoutResult) (buf : Buffer) : Buffer :=
  match layouts.get nodeId with
  | none => buf  -- No layout for this node
  | some computed =>
    let rect := toTerminalRect computed.contentRect
    match node with
    | .text content style =>
      renderText content style rect buf

    | .block title borderType borderStyle _ =>
      -- Render border using the border rect (includes border area)
      let borderRect := toTerminalRect computed.borderRect
      renderBlockBorder title borderType borderStyle borderRect buf

    | .row _ _ _ | .column _ _ _ | .spacer _ _ | .empty =>
      -- Container nodes don't render directly - children handle themselves
      buf

/-- Render entire RNode tree to Buffer.
    Traverses the nodeMap and renders each node at its computed position. -/
def renderTree (nodeMap : Array (Nat × RNode)) (layouts : LayoutResult) (buf : Buffer) : Buffer :=
  nodeMap.foldl (fun b (id, node) => renderNode id node layouts b) buf

/-! ## Main Render Function -/

/-- Compute layout for an RNode tree.
    Returns the layout result and node mapping for rendering. -/
def computeLayout (root : RNode) (width height : Nat) : LayoutResult × Array (Nat × RNode) :=
  let ((layoutTree, nodeMap), _) := buildLayoutTree root |>.run { nextId := 0 }
  let layouts := Trellis.layoutNode layoutTree width.toFloat height.toFloat
  (layouts, nodeMap)

/-- Render an RNode tree to a Buffer.
    This is the main entry point for rendering reactive widgets. -/
def renderToBuffer (root : RNode) (width height : Nat) (buf : Buffer) : Buffer :=
  let (layouts, nodeMap) := computeLayout root width height
  renderTree nodeMap layouts buf

/-- Create a fresh buffer and render an RNode tree to it. -/
def render (root : RNode) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  renderToBuffer root width height buf

end Terminus.Reactive
