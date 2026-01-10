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

  | .clipped child =>
    -- Clipped node acts as a pass-through container
    -- The clip boundary is established during render based on computed bounds
    let (childLayout, childMap) ← buildLayoutTree child
    let layoutNode := LayoutNode.column nodeId #[childLayout] (gap := 0)
    pure (layoutNode, #[(nodeId, node)] ++ childMap)

  | .scrolled _ _ child =>
    -- Scrolled node passes through to child
    -- Offset is applied during render phase
    let (childLayout, childMap) ← buildLayoutTree child
    let layoutNode := LayoutNode.column nodeId #[childLayout] (gap := 0)
    pure (layoutNode, #[(nodeId, node)] ++ childMap)

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

/-! ## Clipping and Scroll Context -/

/-- Clip rectangle context for rendering. None means no clipping. -/
structure ClipContext where
  /-- Current clip rectangle (none = no clipping). -/
  clipRect : Option Terminus.Rect := none
  /-- Scroll offset to subtract from positions. -/
  scrollOffsetX : Nat := 0
  scrollOffsetY : Nat := 0
  deriving Repr, Inhabited

namespace ClipContext

/-- Check if a cell position is within the clip bounds. -/
def contains (ctx : ClipContext) (x y : Nat) : Bool :=
  match ctx.clipRect with
  | none => true
  | some r =>
    x >= r.x && x < r.x + r.width &&
    y >= r.y && y < r.y + r.height

/-- Intersect current clip with a new rect. -/
def intersect (ctx : ClipContext) (r : Terminus.Rect) : ClipContext :=
  match ctx.clipRect with
  | none => { ctx with clipRect := some r }
  | some existing =>
    -- Compute intersection
    let x1 := max existing.x r.x
    let y1 := max existing.y r.y
    let x2 := min (existing.x + existing.width) (r.x + r.width)
    let y2 := min (existing.y + existing.height) (r.y + r.height)
    let w := if x2 > x1 then x2 - x1 else 0
    let h := if y2 > y1 then y2 - y1 else 0
    { ctx with clipRect := some { x := x1, y := y1, width := w, height := h } }

/-- Add scroll offset to context. -/
def addOffset (ctx : ClipContext) (dx dy : Nat) : ClipContext :=
  { ctx with
    scrollOffsetX := ctx.scrollOffsetX + dx
    scrollOffsetY := ctx.scrollOffsetY + dy }

/-- Apply scroll offset to a position. Returns adjusted (x, y). -/
def applyOffset (ctx : ClipContext) (x y : Nat) : (Nat × Nat) :=
  (x - min x ctx.scrollOffsetX, y - min y ctx.scrollOffsetY)

end ClipContext

/-! ## Buffer Rendering -/

/-- Render a text node to the buffer at the given rect (no clipping). -/
def renderText (content : String) (style : Style) (rect : Terminus.Rect) (buf : Buffer) : Buffer :=
  if rect.width == 0 || rect.height == 0 then buf
  else
    -- Truncate text to fit width
    let displayText := content.take rect.width
    buf.writeString rect.x rect.y displayText style

/-- Write a string with clipping support. -/
def writeStringClipped (buf : Buffer) (x y : Nat) (s : String) (style : Style)
    (clip : ClipContext) : Buffer := Id.run do
  let mut result := buf
  let mut col := x
  for c in s.toList do
    let width := c.displayWidth
    if width == 0 then continue
    if clip.contains col y then
      result := result.setStyled col y c style
    if width == 2 && clip.contains (col + 1) y then
      result := result.set (col + 1) y Cell.placeholder
    col := col + width
  result

/-- Write a styled character with clipping. -/
def setStyledClipped (buf : Buffer) (x y : Nat) (c : Char) (style : Style)
    (clip : ClipContext) : Buffer :=
  if clip.contains x y then buf.setStyled x y c style else buf

/-- Render a text node with clipping support. -/
def renderTextClipped (content : String) (style : Style) (rect : Terminus.Rect)
    (buf : Buffer) (clip : ClipContext) : Buffer :=
  if rect.width == 0 || rect.height == 0 then buf
  else
    let displayText := content.take rect.width
    writeStringClipped buf rect.x rect.y displayText style clip

/-- Render a block border around the given rect (no clipping). -/
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

/-- Render a block border with clipping support. -/
def renderBlockBorderClipped (title : Option String) (borderType : BorderType) (borderStyle : Style)
    (rect : Terminus.Rect) (buf : Buffer) (clip : ClipContext) : Buffer := Id.run do
  if borderType == .none || rect.width < 2 || rect.height < 2 then
    return buf

  let chars := BorderChars.fromType borderType
  let mut result := buf

  -- Draw corners
  result := setStyledClipped result rect.x rect.y chars.topLeft borderStyle clip
  result := setStyledClipped result (rect.x + rect.width - 1) rect.y chars.topRight borderStyle clip
  result := setStyledClipped result rect.x (rect.y + rect.height - 1) chars.bottomLeft borderStyle clip
  result := setStyledClipped result (rect.x + rect.width - 1) (rect.y + rect.height - 1) chars.bottomRight borderStyle clip

  -- Draw horizontal borders
  for x in [rect.x + 1 : rect.x + rect.width - 1] do
    result := setStyledClipped result x rect.y chars.horizontal borderStyle clip
    result := setStyledClipped result x (rect.y + rect.height - 1) chars.horizontal borderStyle clip

  -- Draw vertical borders
  for y in [rect.y + 1 : rect.y + rect.height - 1] do
    result := setStyledClipped result rect.x y chars.vertical borderStyle clip
    result := setStyledClipped result (rect.x + rect.width - 1) y chars.vertical borderStyle clip

  -- Draw title if present
  match title with
  | some t =>
    if rect.width >= 4 then
      let maxLen := rect.width - 4
      let displayTitle := " " ++ t.take maxLen ++ " "
      result := writeStringClipped result (rect.x + 1) rect.y displayTitle borderStyle clip
  | none => pure ()

  result

/-- Look up clip context for a node in an array of (nodeId, clipContext). -/
def lookupClipContext (contexts : Array (Nat × ClipContext)) (nodeId : Nat) : ClipContext :=
  match contexts.find? (fun (id, _) => id == nodeId) with
  | some (_, ctx) => ctx
  | none => {}

/-- Build clip contexts for all nodes by traversing the tree.
    Returns an array of (nodeId, clipContext) pairs. -/
partial def buildClipContexts (nodeMap : Array (Nat × RNode)) (layouts : LayoutResult)
    : Array (Nat × ClipContext) := Id.run do
  let mut contexts : Array (Nat × ClipContext) := #[]

  -- For now, use a simple approach: each node gets the default context
  -- Clipping is applied per-node based on ancestors
  for (nodeId, node) in nodeMap do
    let ctx : ClipContext := match node with
    | .clipped _ =>
      match layouts.get nodeId with
      | some computed =>
        let rect := toTerminalRect computed.contentRect
        ClipContext.intersect {} rect
      | none => {}
    | _ => {}
    contexts := contexts.push (nodeId, ctx)

  contexts

/-- Render an RNode to a Buffer using computed layout with clipping.
    - nodeMap: Mapping from Trellis node IDs to RNodes
    - layouts: Computed layout result from Trellis
    - clip: Current clipping context -/
def renderNodeClipped (nodeId : Nat) (node : RNode) (layouts : LayoutResult)
    (buf : Buffer) (clip : ClipContext) : Buffer :=
  match layouts.get nodeId with
  | none => buf  -- No layout for this node
  | some computed =>
    let rect := toTerminalRect computed.contentRect
    match node with
    | .text content style =>
      renderTextClipped content style rect buf clip

    | .block title borderType borderStyle _ =>
      -- Render border using the border rect (includes border area)
      let borderRect := toTerminalRect computed.borderRect
      renderBlockBorderClipped title borderType borderStyle borderRect buf clip

    | .clipped _ | .scrolled _ _ _ =>
      -- These nodes don't render directly - they affect clip context
      buf

    | .row _ _ _ | .column _ _ _ | .spacer _ _ | .empty =>
      -- Container nodes don't render directly - children handle themselves
      buf

/-- Render an RNode to a Buffer using computed layout (no clipping). -/
def renderNode (nodeId : Nat) (node : RNode) (layouts : LayoutResult) (buf : Buffer) : Buffer :=
  renderNodeClipped nodeId node layouts buf {}

/-- Render entire RNode tree to Buffer with clipping support.
    Traverses the nodeMap and renders each node at its computed position. -/
def renderTreeClipped (nodeMap : Array (Nat × RNode)) (layouts : LayoutResult)
    (buf : Buffer) (_defaultClip : ClipContext := {}) : Buffer := Id.run do
  -- Build clip contexts for all nodes
  let contexts := buildClipContexts nodeMap layouts

  -- Render each node with its appropriate clip context
  let mut result := buf
  for (nodeId, node) in nodeMap do
    let clip := lookupClipContext contexts nodeId
    result := renderNodeClipped nodeId node layouts result clip
  result

/-- Render entire RNode tree to Buffer (backwards compatible, no clipping). -/
def renderTree (nodeMap : Array (Nat × RNode)) (layouts : LayoutResult) (buf : Buffer) : Buffer :=
  renderTreeClipped nodeMap layouts buf

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
