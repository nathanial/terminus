/-
  Terminus Reactive - Layout Engine
  Computes layout for RNode trees using Terminus internal Layout system.
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

/-- Lookup rect by node ID in layout map. -/
def lookupRect (layouts : LayoutMap) (nodeId : Nat) : Option Rect :=
  layouts.find? (fun (id, _) => id == nodeId) |>.map (·.2)

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

end Terminus.Reactive
