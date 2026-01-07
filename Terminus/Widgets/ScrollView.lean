-- Terminus.Widgets.ScrollView: Scrollable container for arbitrary widgets

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Widgets.Scrollbar

namespace Terminus

/-- A scrollable viewport over content rendered to an offscreen buffer.

Since Terminus widgets don't currently expose intrinsic sizing, `contentSize` must be provided. -/
structure ScrollView (α : Type) where
  content : α
  /-- (contentWidth, contentHeight) in terminal cells. -/
  contentSize : Nat × Nat := (0, 0)
  /-- (offsetX, offsetY) in terminal cells. -/
  offset : Nat × Nat := (0, 0)
  block : Option Block := none
  deriving Inhabited

namespace ScrollView

def new (content : α) : ScrollView α := { content }

def withContentSize (s : ScrollView α) (w h : Nat) : ScrollView α := { s with contentSize := (w, h) }
def withOffset (s : ScrollView α) (x y : Nat) : ScrollView α := { s with offset := (x, y) }
def withBlock (s : ScrollView α) (b : Block) : ScrollView α := { s with block := some b }

def contentWidth (s : ScrollView α) : Nat := s.contentSize.1
def contentHeight (s : ScrollView α) : Nat := s.contentSize.2
def offsetX (s : ScrollView α) : Nat := s.offset.1
def offsetY (s : ScrollView α) : Nat := s.offset.2

def scrollToTop (s : ScrollView α) : ScrollView α := { s with offset := (s.offsetX, 0) }
def scrollToLeft (s : ScrollView α) : ScrollView α := { s with offset := (0, s.offsetY) }

def scrollDown (s : ScrollView α) (n : Nat := 1) : ScrollView α := { s with offset := (s.offsetX, s.offsetY + n) }
def scrollUp (s : ScrollView α) (n : Nat := 1) : ScrollView α := { s with offset := (s.offsetX, s.offsetY - n) }
def scrollRight (s : ScrollView α) (n : Nat := 1) : ScrollView α := { s with offset := (s.offsetX + n, s.offsetY) }
def scrollLeft (s : ScrollView α) (n : Nat := 1) : ScrollView α := { s with offset := (s.offsetX - n, s.offsetY) }

private def clampOffset (s : ScrollView α) (viewportW viewportH : Nat) : Nat × Nat :=
  let maxX := if s.contentWidth > viewportW then s.contentWidth - viewportW else 0
  let maxY := if s.contentHeight > viewportH then s.contentHeight - viewportH else 0
  (Nat.min s.offsetX maxX, Nat.min s.offsetY maxY)

/-- Convenience vertical scrollbar for this scroll view and viewport. -/
def verticalScrollbar (s : ScrollView α) (viewportH : Nat) : Scrollbar :=
  Scrollbar.vertical s.offsetY s.contentHeight viewportH

/-- Convenience horizontal scrollbar for this scroll view and viewport. -/
def horizontalScrollbar (s : ScrollView α) (viewportW : Nat) : Scrollbar :=
  Scrollbar.horizontal s.offsetX s.contentWidth viewportW

end ScrollView

instance [Widget α] : Widget (ScrollView α) where
  render s area buf := Id.run do
    let (viewport, buf') := renderBlockAndGetInner s.block area buf
    if viewport.isEmpty then return buf'
    let mut result := buf'

    let contentW := s.contentWidth
    let contentH := s.contentHeight
    if contentW == 0 || contentH == 0 then return result

    let (offX, offY) := ScrollView.clampOffset s viewport.width viewport.height

    let offscreenArea : Rect := { x := 0, y := 0, width := contentW, height := contentH }
    let offscreen := Widget.render s.content offscreenArea (Buffer.new contentW contentH)

    for row in [:viewport.height] do
      for col in [:viewport.width] do
        let srcX := offX + col
        let srcY := offY + row
        if srcX < contentW && srcY < contentH then
          let cell := offscreen.get srcX srcY
          result := result.set (viewport.x + col) (viewport.y + row) cell

    result

end Terminus
