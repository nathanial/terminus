/-
  Terminus Reactive - Render Pipeline
  Converts RNode trees to terminal Buffer output using internal Layout system.
-/
import Terminus.Reactive.Types

namespace Terminus.Reactive

/-! ## ID Generation

We need unique IDs for each RNode when computing layout.
-/

/-- State for generating unique node IDs during tree traversal. -/
structure IdGen where
  nextId : Nat := 0

/-- Get next ID and increment counter. -/
def IdGen.fresh (g : IdGen) : Nat × IdGen :=
  (g.nextId, { nextId := g.nextId + 1 })

/-! ## Layout Computation

Compute layout for RNode trees using Terminus internal Layout system.
-/

/-- Result of layout computation: mapping node IDs to Rects. -/
abbrev LayoutMap := Array (Nat × Rect)

/-- Compute the natural/minimum height of an RNode (for layout purposes). -/
partial def naturalHeight (node : RNode) : Nat :=
  match node with
  | .text _ _ => 1
  | .spacer _ h => h
  | .empty => 0
  | .row _ style children =>
    let padding := style.padding * 2
    let maxChildHeight := children.foldl (fun acc c => max acc (naturalHeight c)) 0
    padding + maxChildHeight
  | .column gap style children =>
    let padding := style.padding * 2
    let childHeights := children.foldl (fun acc c => acc + naturalHeight c) 0
    let gapTotal := if children.size > 1 then gap * (children.size - 1) else 0
    padding + childHeights + gapTotal
  | .block _ _ _ child =>
    -- Border adds 2 (top + bottom)
    2 + naturalHeight child
  | .clipped child => naturalHeight child
  | .scrolled _ _ child => naturalHeight child

/-- Compute the natural/minimum width of an RNode (for layout purposes). -/
partial def naturalWidth (node : RNode) : Nat :=
  match node with
  | .text content _ => content.length
  | .spacer w _ => w
  | .empty => 0
  | .row gap style children =>
    let padding := style.padding * 2
    let childWidths := children.foldl (fun acc c => acc + naturalWidth c) 0
    let gapTotal := if children.size > 1 then gap * (children.size - 1) else 0
    padding + childWidths + gapTotal
  | .column _ style children =>
    let padding := style.padding * 2
    let maxChildWidth := children.foldl (fun acc c => max acc (naturalWidth c)) 0
    padding + maxChildWidth
  | .block _ _ _ child =>
    -- Border adds 2 (left + right)
    2 + naturalWidth child
  | .clipped child => naturalWidth child
  | .scrolled _ _ child => naturalWidth child

/-- Compute layout for RNode tree recursively using internal Layout system. -/
partial def computeLayoutRec (node : RNode) (area : Rect) : StateM IdGen LayoutMap := do
  let (nodeId, gen) ← get >>= fun g => pure g.fresh
  set gen

  match node with
  | .text _content _ =>
    -- Text takes full width, height of 1 (single line)
    let textRect := { area with height := max 1 (min area.height 1) }
    pure #[(nodeId, textRect)]

  | .row gap style children =>
    if children.isEmpty then pure #[(nodeId, area)]
    else
      let innerArea := if style.padding > 0 then area.inner style.padding else area
      -- Compute child widths using natural width
      -- All nodes get fixed width based on their natural size
      let constraints := children.toList.map (fun child => Constraint.fixed (naturalWidth child))
      let layout := Layout.horizontal constraints |>.withSpacing gap
      let childRects := layout.split innerArea
      let mut result : LayoutMap := #[(nodeId, area)]
      for h : i in [:children.size] do
        let child := children[i]
        let childRect := childRects.getD i innerArea
        let childLayouts ← computeLayoutRec child childRect
        result := result ++ childLayouts
      pure result

  | .column gap style children =>
    if children.isEmpty then pure #[(nodeId, area)]
    else
      let innerArea := if style.padding > 0 then area.inner style.padding else area
      -- Compute child heights using natural height
      -- All nodes get fixed height based on their natural size
      let constraints := children.toList.map (fun child => Constraint.fixed (naturalHeight child))
      let layout := Layout.vertical constraints |>.withSpacing gap
      let childRects := layout.split innerArea
      let mut result : LayoutMap := #[(nodeId, area)]
      for h : i in [:children.size] do
        let child := children[i]
        let childRect := childRects.getD i innerArea
        let childLayouts ← computeLayoutRec child childRect
        result := result ++ childLayouts
      pure result

  | .block _ _ _ child =>
    -- Block has 1-cell border padding
    let innerArea := area.inner 1
    let mut result : LayoutMap := #[(nodeId, area)]
    let childLayouts ← computeLayoutRec child innerArea
    result := result ++ childLayouts
    pure result

  | .spacer w h =>
    let spacerRect := { area with
      width := min w area.width
      height := min h area.height }
    pure #[(nodeId, spacerRect)]

  | .empty =>
    pure #[(nodeId, { area with width := 0, height := 0 })]

  | .clipped child =>
    let mut result : LayoutMap := #[(nodeId, area)]
    let childLayouts ← computeLayoutRec child area
    result := result ++ childLayouts
    pure result

  | .scrolled _ _ child =>
    let mut result : LayoutMap := #[(nodeId, area)]
    let childLayouts ← computeLayoutRec child area
    result := result ++ childLayouts
    pure result

/-- Look up rect by node ID in layout map. -/
def lookupRect (layouts : LayoutMap) (nodeId : Nat) : Option Rect :=
  layouts.find? (fun (id, _) => id == nodeId) |>.map (·.2)

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

/-- Render state tracking node ID counter and buffer. -/
structure RenderState where
  nextId : Nat := 0
  buf : Buffer

/-- Render an RNode recursively with proper clip context propagation. -/
partial def renderNodeRecursive (node : RNode) (layouts : LayoutMap)
    (clip : ClipContext) : StateM RenderState Unit := do
  let st ← get
  let nodeId := st.nextId
  set { st with nextId := nodeId + 1 }

  match lookupRect layouts nodeId with
  | none => pure ()
  | some rect =>
    match node with
    | .text content style =>
      modify fun s => { s with buf := renderTextClipped content style rect s.buf clip }

    | .block title borderType borderStyle child =>
      -- For blocks, the rect is the border rect; content is inside
      modify fun s => { s with buf := renderBlockBorderClipped title borderType borderStyle rect s.buf clip }
      renderNodeRecursive child layouts clip

    | .row _ _ children =>
      for child in children do
        renderNodeRecursive child layouts clip

    | .column _ _ children =>
      for child in children do
        renderNodeRecursive child layouts clip

    | .clipped child =>
      let newClip := clip.intersect rect
      renderNodeRecursive child layouts newClip

    | .scrolled _offsetX _offsetY child =>
      renderNodeRecursive child layouts clip

    | .spacer _ _ | .empty =>
      pure ()

/-- Render entire RNode tree to Buffer with clipping support. -/
def renderTree (node : RNode) (layouts : LayoutMap) (buf : Buffer) : Buffer :=
  let (_, st) := renderNodeRecursive node layouts {} |>.run { nextId := 0, buf := buf }
  st.buf

/-! ## Main Render Function -/

/-- Compute layout for an RNode tree. -/
def computeLayout (root : RNode) (width height : Nat) : LayoutMap :=
  let area : Rect := { x := 0, y := 0, width, height }
  let (layouts, _) := computeLayoutRec root area |>.run { nextId := 0 }
  layouts

/-- Render an RNode tree to a Buffer.
    This is the main entry point for rendering reactive widgets. -/
def renderToBuffer (root : RNode) (width height : Nat) (buf : Buffer) : Buffer :=
  let layouts := computeLayout root width height
  renderTree root layouts buf

/-- Create a fresh buffer and render an RNode tree to it. -/
def render (root : RNode) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  renderToBuffer root width height buf

end Terminus.Reactive
