/-
  Terminus Reactive - Render Pipeline
  Converts RNode trees to terminal Buffer output using internal Layout system.
-/
import Terminus.Reactive.Types

namespace Terminus.Reactive

/-! ## ID Generation

We need unique IDs for each RNode when computing layout.
-/

structure DeferredLayout where
  node : RNode
  area : Rect
  fullArea : Rect
  deriving Inhabited

/-- State for layout computation, tracking IDs and deferred overlays. -/
structure LayoutState where
  nextId : Nat := 0
  deferred : Array DeferredLayout := #[]

/-- Get next ID and increment counter. -/
def LayoutState.fresh (s : LayoutState) : Nat × LayoutState :=
  (s.nextId, { s with nextId := s.nextId + 1 })


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
  | .image _ _ _ h _ _ => h
  | .row _ style children =>
    let padding := style.padding * 2
    let maxChildHeight := children.foldl (fun acc c => max acc (naturalHeight c)) 0
    padding + maxChildHeight
  | .column gap style children =>
    let padding := style.padding * 2
    let childHeights := children.foldl (fun acc c => acc + naturalHeight c) 0
    let gapTotal := if children.size > 1 then gap * (children.size - 1) else 0
    padding + childHeights + gapTotal
  | .block _ _ _ _ child =>
    -- Border adds 2 (top + bottom)
    2 + naturalHeight child
  | .clipped child => naturalHeight child
  | .scrolled _ _ child => naturalHeight child
  | .dockBottom footerHeight content _footer =>
    footerHeight + naturalHeight content
  | .overlay _base content _ =>
    -- Overlay takes the content's natural height (base doesn't affect overlay size)
    naturalHeight content

/-- Compute the natural/minimum width of an RNode (for layout purposes). -/
partial def naturalWidth (node : RNode) : Nat :=
  match node with
  | .text content _ => content.length
  | .spacer w _ => w
  | .empty => 0
  | .image _ _ w _ _ _ => w
  | .row gap style children =>
    let padding := style.padding * 2
    let childWidths := children.foldl (fun acc c => acc + naturalWidth c) 0
    let gapTotal := if children.size > 1 then gap * (children.size - 1) else 0
    padding + childWidths + gapTotal
  | .column _ style children =>
    let padding := style.padding * 2
    let maxChildWidth := children.foldl (fun acc c => max acc (naturalWidth c)) 0
    padding + maxChildWidth
  | .block _ _ _ _ child =>
    -- Border adds 2 (left + right)
    2 + naturalWidth child
  | .clipped child => naturalWidth child
  | .scrolled _ _ child => naturalWidth child
  | .dockBottom _footerHeight content footer =>
    max (naturalWidth content) (naturalWidth footer)
  | .overlay _base content _ =>
    -- Overlay takes the content's natural width
    naturalWidth content

/-- Compute layout for RNode tree recursively using internal Layout system.
    `area` is the current layout area, `fullArea` is the full screen for overlay centering. -/
partial def computeLayoutRec (node : RNode) (area : Rect) (fullArea : Rect) : StateM LayoutState LayoutMap := do
  let (nodeId, gen) ← get >>= fun g => pure g.fresh
  set gen

  match node with
  | .text _content _ =>
    -- Text takes full width, height of 1 (single line) but respects 0-height areas
    let textRect := { area with height := min 1 area.height }
    pure #[(nodeId, textRect)]

  | .row gap style children =>
    if children.isEmpty then pure #[(nodeId, area)]
    else
      let innerArea := if style.padding > 0 then area.inner style.padding else area
      -- If inner area is empty (no space), skip child layout entirely
      if innerArea.isEmpty then pure #[(nodeId, area)]
      else
        -- Compute child widths using natural width
        -- All nodes get fixed width based on their natural size
        let constraints := children.toList.map (fun child => Constraint.fixed (naturalWidth child))
        let layout := Layout.horizontal constraints |>.withSpacing gap
        let childRects := layout.split innerArea
        let mut result : LayoutMap := #[(nodeId, area)]
        for h : i in [:children.size] do
          let child := children[i]
          let childRect := childRects.getD i innerArea
          let childLayouts ← computeLayoutRec child childRect fullArea
          result := result ++ childLayouts
        pure result

  | .column gap style children =>
    if children.isEmpty then pure #[(nodeId, area)]
    else
      let innerArea := if style.padding > 0 then area.inner style.padding else area
      -- If inner area is empty (no space), skip child layout entirely
      if innerArea.isEmpty then pure #[(nodeId, area)]
      else
        -- Compute child heights using natural height
        -- All nodes get fixed height based on their natural size
        let constraints := children.toList.map (fun child => Constraint.fixed (naturalHeight child))
        let layout := Layout.vertical constraints |>.withSpacing gap
        let childRects := layout.split innerArea
        let mut result : LayoutMap := #[(nodeId, area)]
        for h : i in [:children.size] do
          let child := children[i]
          let childRect := childRects.getD i innerArea
          let childLayouts ← computeLayoutRec child childRect fullArea
          result := result ++ childLayouts
        pure result

  | .block _ _ _ _ child =>
    -- Block has 1-cell border padding
    let innerArea := area.inner 1
    let mut result : LayoutMap := #[(nodeId, area)]
    -- Only layout child if there's space inside the block
    if innerArea.isEmpty then pure result
    else
      let childLayouts ← computeLayoutRec child innerArea fullArea
      result := result ++ childLayouts
      pure result

  | .overlay base content _ =>
    -- Layout base content using current area
    let mut result : LayoutMap := #[(nodeId, fullArea)]
    let baseLayouts ← computeLayoutRec base area fullArea
    result := result ++ baseLayouts

    -- Compute overlay content's natural size
    let overlayNatWidth := naturalWidth content
    let overlayNatHeight := naturalHeight content

    -- Center the overlay in the FULL SCREEN area (not parent area)
    let overlayWidth := min overlayNatWidth fullArea.width
    let overlayHeight := min overlayNatHeight fullArea.height
    let overlayX := fullArea.x + (fullArea.width - overlayWidth) / 2
    let overlayY := fullArea.y + (fullArea.height - overlayHeight) / 2

    let overlayRect : Rect := {
      x := overlayX
      y := overlayY
      width := overlayWidth
      height := overlayHeight
    }

    -- Defer content layout
    modify fun s => { s with deferred := s.deferred.push { node := content, area := overlayRect, fullArea := fullArea } }
    pure result

  | .dockBottom footerHeight content footer =>
    let footerH := min footerHeight area.height
    let contentH := if area.height > footerH then area.height - footerH else 0
    let contentRect := { area with height := contentH }
    let footerRect := { area with y := area.y + contentH, height := footerH }
    let mut result : LayoutMap := #[(nodeId, area)]
    let contentLayouts ← computeLayoutRec content contentRect fullArea
    let footerLayouts ← computeLayoutRec footer footerRect fullArea
    result := result ++ contentLayouts ++ footerLayouts
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
    -- Skip child if area is empty
    if area.isEmpty then pure result
    else
      let childLayouts ← computeLayoutRec child area fullArea
      result := result ++ childLayouts
      pure result

  | .scrolled _ _ child =>
    let mut result : LayoutMap := #[(nodeId, area)]
    -- Skip child if area is empty
    if area.isEmpty then pure result
    else
      let childLayouts ← computeLayoutRec child area fullArea
      result := result ++ childLayouts
      pure result

  | .image _ _ w h _ _ =>
    let imageRect := { area with
      width := min w area.width
      height := min h area.height }
    pure #[(nodeId, imageRect)]

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

/-- Render state tracking node ID counter, buffer, and terminal commands. -/
structure DeferredRender where
  node : RNode
  rect : Rect
  backdrop : Option Style
  clip : ClipContext
  deriving Inhabited

/-- Render state tracking node ID counter, buffer, and terminal commands. -/
structure RenderState where
  nextId : Nat := 0
  buf : Buffer
  commands : Array TerminalCommand := #[]
  deferred : Array DeferredRender := #[]

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

    | .block title borderType borderStyle fillStyle child =>
      -- Fill background first if specified
      match fillStyle with
      | some style =>
        let innerRect := rect.inner 1
        if !innerRect.isEmpty then
          let bgCell := Cell.styled ' ' style
          modify fun s => { s with buf := s.buf.fillRect innerRect bgCell }
      | none => pure ()
      -- For blocks, the rect is the border rect; content is inside
      modify fun s => { s with buf := renderBlockBorderClipped title borderType borderStyle rect s.buf clip }
      renderNodeRecursive child layouts clip

    | .overlay base content backdropStyle =>
      -- 1. Render base content first
      renderNodeRecursive base layouts clip

      -- 2. Defer overlay content and backdrop
      let task : DeferredRender := {
        node := content
        rect := rect
        backdrop := backdropStyle
        clip := clip
      }
      modify fun s => { s with deferred := s.deferred.push task }

    | .row _ _ children =>
      for child in children do
        renderNodeRecursive child layouts clip

    | .column _ _ children =>
      for child in children do
        renderNodeRecursive child layouts clip

    | .dockBottom _ content footer =>
      renderNodeRecursive content layouts clip
      renderNodeRecursive footer layouts clip

    | .clipped child =>
      let newClip := clip.intersect rect
      renderNodeRecursive child layouts newClip

    | .scrolled _offsetX _offsetY child =>
      renderNodeRecursive child layouts clip

    | .image source protocol _ _ preserveAspect altText =>
      -- Add image command
      let cmd : ImageCommand := {
        rect := rect
        source := source
        protocol := protocol
        preserveAspectRatio := preserveAspect
      }
      modify fun s => { s with commands := s.commands.push (.image cmd) }
      -- Also render alt text as fallback (for terminals that don't support images)
      let fallbackText := s!"[{altText}]"
      modify fun s => { s with buf := renderTextClipped fallbackText Style.dim rect s.buf clip }

    | .spacer _ _ | .empty =>
      pure ()

/-- Result of rendering an RNode tree. -/
structure RenderResult where
  /-- The rendered buffer. -/
  buffer : Buffer
  /-- Terminal commands (images, clipboard, etc.) -/
  commands : Array TerminalCommand := #[]
  deriving Inhabited

/-- Process deferred render queue. -/
private partial def processRenderQueue (layouts : LayoutMap) : StateM RenderState Unit := do
  let st ← get
  if st.deferred.isEmpty then pure ()
  else
    -- Pop first task
    let task := st.deferred[0]!
    set { st with deferred := st.deferred.extract 1 st.deferred.size }

    -- Render backdrop
    match task.backdrop with
    | some style =>
       let bgCell := Cell.styled ' ' style
       modify fun s => { s with buf := s.buf.fillRect task.rect bgCell }
    | none => pure ()

    -- Render content
    let overlayClip : ClipContext := {}
    renderNodeRecursive task.node layouts overlayClip

    -- Continue
    processRenderQueue layouts

/-- Render entire RNode tree to Buffer with clipping support. -/
def renderTree (node : RNode) (layouts : LayoutMap) (buf : Buffer) : RenderResult :=
  let actions : StateM RenderState Unit := do
    -- Initial render
    renderNodeRecursive node layouts {}
    -- Process deferred queue
    processRenderQueue layouts

  let (_, st) := actions.run { nextId := 0, buf := buf }
  { buffer := st.buf, commands := st.commands }

/-! ## Main Render Function -/

/-- Process deferred layouts. -/
private partial def processLayoutQueue (acc : LayoutMap) : StateM LayoutState LayoutMap := do
  let st ← get
  if st.deferred.isEmpty then pure acc
  else
    let task := st.deferred[0]!
    set { st with deferred := st.deferred.extract 1 st.deferred.size }

    let taskLayouts ← computeLayoutRec task.node task.area task.fullArea
    processLayoutQueue (acc ++ taskLayouts)

/-- Compute layout for an RNode tree. -/
def computeLayout (root : RNode) (width height : Nat) : LayoutMap :=
  let area : Rect := { x := 0, y := 0, width, height }
  let (layouts, _) := (do
    let layouts ← computeLayoutRec root area area
    processLayoutQueue layouts
  ).run { nextId := 0 }
  layouts

/-- Render an RNode tree to a Buffer and collect commands.
    This is the main entry point for rendering reactive widgets. -/
def renderToBuffer (root : RNode) (width height : Nat) (buf : Buffer) : RenderResult :=
  let layouts := computeLayout root width height
  renderTree root layouts buf

/-- Create a fresh buffer and render an RNode tree to it. -/
def render (root : RNode) (width height : Nat) : RenderResult :=
  let buf := Buffer.new width height
  renderToBuffer root width height buf

/-- Render an RNode tree to just a Buffer (ignoring commands).
    Use this for simple cases where image/clipboard commands aren't needed. -/
def renderToBufferOnly (root : RNode) (width height : Nat) (buf : Buffer) : Buffer :=
  (renderToBuffer root width height buf).buffer

/-- Create a fresh buffer and render, returning just the buffer. -/
def renderOnly (root : RNode) (width height : Nat) : Buffer :=
  (render root width height).buffer

end Terminus.Reactive
