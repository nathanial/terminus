-- Terminus.Widgets.ScrollView: Scrollable container for arbitrary widgets

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Widgets.Scrollbar

namespace Terminus

/-- A scrollable viewport over content rendered to an offscreen buffer.

If `contentSize` is (0, 0), the widget's preferred size is used when available. -/
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

private def clampOffset (s : ScrollView α) (contentW contentH viewportW viewportH : Nat) : Nat × Nat :=
  let maxX := if contentW > viewportW then contentW - viewportW else 0
  let maxY := if contentH > viewportH then contentH - viewportH else 0
  (Nat.min s.offsetX maxX, Nat.min s.offsetY maxY)

private def resolveContentSize [Widget α] (s : ScrollView α) : Nat × Nat :=
  let (w, h) := s.contentSize
  if w > 0 && h > 0 then
    (w, h)
  else
    match Widget.preferredSize s.content with
    | some size => size
    | none => (0, 0)

private def borderSize (block : Option Block) : Nat :=
  match block with
  | some b => if b.borderType == .none then 0 else 2
  | none => 0

/-- Convenience vertical scrollbar for this scroll view and viewport. -/
def verticalScrollbar [Widget α] (s : ScrollView α) (viewportH : Nat) : Scrollbar :=
  let (_, contentH) := ScrollView.resolveContentSize s
  Scrollbar.vertical s.offsetY contentH viewportH

/-- Convenience horizontal scrollbar for this scroll view and viewport. -/
def horizontalScrollbar [Widget α] (s : ScrollView α) (viewportW : Nat) : Scrollbar :=
  let (contentW, _) := ScrollView.resolveContentSize s
  Scrollbar.horizontal s.offsetX contentW viewportW

end ScrollView

instance [Widget α] : Widget (ScrollView α) where
  render s area buf := Id.run do
    let (viewport, buf') := renderBlockAndGetInner s.block area buf
    if viewport.isEmpty then return buf'
    let mut result := buf'

    let (contentW, contentH) := ScrollView.resolveContentSize s
    if contentW == 0 || contentH == 0 then return result

    let (offX, offY) := ScrollView.clampOffset s contentW contentH viewport.width viewport.height

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
  preferredSize s :=
    let (w, h) := ScrollView.resolveContentSize s
    if w == 0 || h == 0 then
      none
    else
      let border := ScrollView.borderSize s.block
      some (w + border, h + border)
  handleEvent s event :=
    { s with content := Widget.handleEvent s.content event }
  focusable s := Widget.focusable s.content
  setFocused s focused :=
    { s with content := Widget.setFocused s.content focused }

end Terminus
