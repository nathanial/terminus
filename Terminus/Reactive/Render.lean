/-
  Terminus Reactive - Render Pipeline
  Converts RNode trees to terminal Buffer output using internal Layout system.
-/
import Terminus.Reactive.Types
import Terminus.Reactive.LayoutEngine
namespace Terminus.Reactive


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
