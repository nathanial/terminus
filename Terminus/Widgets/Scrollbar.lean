-- Terminus.Widgets.Scrollbar: Visual scroll indicator

import Terminus.Widgets.Widget

namespace Terminus

/-- Scrollbar widget - shows scroll position in a list or content area -/
structure Scrollbar where
  position : Nat := 0         -- Current scroll position (first visible item)
  totalItems : Nat := 0       -- Total number of items
  visibleItems : Nat := 0     -- Number of items visible in viewport
  orientation : Orientation := .vertical
  trackChar : Char := '░'
  thumbChar : Char := '█'
  trackStyle : Style := Style.dim
  thumbStyle : Style := Style.default
  deriving Repr, Inhabited

namespace Scrollbar

def new (position total visible : Nat) : Scrollbar := {
  position := position
  totalItems := total
  visibleItems := visible
}

def vertical (position total visible : Nat) : Scrollbar :=
  { new position total visible with orientation := .vertical }

def horizontal (position total visible : Nat) : Scrollbar :=
  { new position total visible with orientation := .horizontal }

def withTrackChar (s : Scrollbar) (c : Char) : Scrollbar := { s with trackChar := c }
def withThumbChar (s : Scrollbar) (c : Char) : Scrollbar := { s with thumbChar := c }
def withTrackStyle (s : Scrollbar) (st : Style) : Scrollbar := { s with trackStyle := st }
def withThumbStyle (s : Scrollbar) (st : Style) : Scrollbar := { s with thumbStyle := st }

def setPosition (s : Scrollbar) (pos : Nat) : Scrollbar := { s with position := pos }
def setTotal (s : Scrollbar) (total : Nat) : Scrollbar := { s with totalItems := total }
def setVisible (s : Scrollbar) (visible : Nat) : Scrollbar := { s with visibleItems := visible }

/-- Check if scrollbar is needed (total > visible) -/
def isNeeded (s : Scrollbar) : Bool := s.totalItems > s.visibleItems

/-- Calculate thumb size and position -/
def thumbMetrics (s : Scrollbar) (trackLength : Nat) : (Nat × Nat) :=
  if s.totalItems == 0 || trackLength == 0 then (0, 0)
  else if s.totalItems <= s.visibleItems then (trackLength, 0)  -- Full thumb, no scrolling needed
  else
    -- Thumb size proportional to visible/total
    let thumbSize := Nat.max 1 (s.visibleItems * trackLength / s.totalItems)
    -- Thumb position based on scroll position
    let scrollRange := s.totalItems - s.visibleItems
    let trackRange := trackLength - thumbSize
    let thumbPos := if scrollRange > 0 then s.position * trackRange / scrollRange else 0
    (thumbSize, thumbPos)

end Scrollbar

instance : Widget Scrollbar where
  render s area buf := Id.run do
    if area.isEmpty then return buf

    let mut result := buf

    match s.orientation with
    | .vertical =>
      let trackLength := area.height
      let (thumbSize, thumbPos) := s.thumbMetrics trackLength
      let x := area.x

      for i in [:trackLength] do
        let y := area.y + i
        let inThumb := i >= thumbPos && i < thumbPos + thumbSize
        let char := if inThumb then s.thumbChar else s.trackChar
        let style := if inThumb then s.thumbStyle else s.trackStyle
        result := result.setStyled x y char style

    | .horizontal =>
      let trackLength := area.width
      let (thumbSize, thumbPos) := s.thumbMetrics trackLength
      let y := area.y

      for i in [:trackLength] do
        let x := area.x + i
        let inThumb := i >= thumbPos && i < thumbPos + thumbSize
        let char := if inThumb then s.thumbChar else s.trackChar
        let style := if inThumb then s.thumbStyle else s.trackStyle
        result := result.setStyled x y char style

    result

end Terminus
