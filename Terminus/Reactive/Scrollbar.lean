/-
  Terminus Reactive - Scrollbar Widget
  Visual scroll indicator for scrollable content.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Scrollbar Configuration -/

/-- Scrollbar orientation. -/
inductive ScrollbarOrientation where
  | vertical
  | horizontal
  deriving Repr, BEq, Inhabited

/-- Configuration for scrollbar appearance. -/
structure ScrollbarConfig where
  /-- Scrollbar orientation. -/
  orientation : ScrollbarOrientation := .vertical
  /-- Character for track (background). -/
  trackChar : Char := '░'
  /-- Character for thumb (draggable part). -/
  thumbChar : Char := '█'
  /-- Style for track. -/
  trackStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for thumb. -/
  thumbStyle : Style := {}
  /-- Length of scrollbar (in characters). -/
  length : Nat := 10
  deriving Repr, Inhabited

/-! ## Scrollbar Widget -/

/-- Calculate thumb size and position based on scroll metrics.
    Returns (thumbSize, thumbPosition). -/
private def thumbMetrics (position totalItems visibleItems trackLength : Nat) : (Nat × Nat) :=
  if totalItems == 0 || trackLength == 0 then (0, 0)
  else if totalItems <= visibleItems then (trackLength, 0)  -- Full thumb, no scrolling needed
  else
    -- Thumb size proportional to visible/total
    let thumbSize := max 1 (visibleItems * trackLength / totalItems)
    -- Thumb position based on scroll position
    let scrollRange := totalItems - visibleItems
    let trackRange := trackLength - thumbSize
    let thumbPos := if scrollRange > 0 then position * trackRange / scrollRange else 0
    (thumbSize, thumbPos)

/-- Create a static scrollbar widget.

    The scrollbar displays the current scroll position within a scrollable area.
    This is a display-only widget - it doesn't handle input.

    Example:
    ```
    scrollbar' 5 100 20 { orientation := .vertical, length := 20 }
    ```
-/
def scrollbar' (position totalItems visibleItems : Nat) (config : ScrollbarConfig := {})
    : WidgetM Unit := do
  let trackLength := config.length
  let (thumbSize, thumbPos) := thumbMetrics position totalItems visibleItems trackLength

  let mut parts : Array RNode := #[]

  for i in [:trackLength] do
    let inThumb := i >= thumbPos && i < thumbPos + thumbSize
    let char := if inThumb then config.thumbChar else config.trackChar
    let style := if inThumb then config.thumbStyle else config.trackStyle
    parts := parts.push (RNode.text (String.singleton char) style)

  match config.orientation with
  | .vertical => emitStatic (RNode.column 0 {} parts)
  | .horizontal => emitStatic (RNode.row 0 {} parts)

/-- Create a dynamic scrollbar widget.

    Takes dynamic values for scroll position, total items, and visible items.

    Example:
    ```
    let scrollPos ← someScrollPosition
    let totalCount ← someTotalCount
    dynScrollbar' scrollPos totalCount (constDyn 20) { length := 15 }
    ```
-/
def dynScrollbar' (position : Reactive.Dynamic Spider Nat)
    (totalItems : Reactive.Dynamic Spider Nat)
    (visibleItems : Reactive.Dynamic Spider Nat)
    (config : ScrollbarConfig := {}) : WidgetM Unit := do
  let node ← position.zipWith3' (fun pos total visible =>
    Id.run do
      let trackLength := config.length
      let (thumbSize, thumbPos) := thumbMetrics pos total visible trackLength

      let mut parts : Array RNode := #[]

      for i in [:trackLength] do
        let inThumb := i >= thumbPos && i < thumbPos + thumbSize
        let char := if inThumb then config.thumbChar else config.trackChar
        let style := if inThumb then config.thumbStyle else config.trackStyle
        parts := parts.push (RNode.text (String.singleton char) style)

      match config.orientation with
      | .vertical => return RNode.column 0 {} parts
      | .horizontal => return RNode.row 0 {} parts
  ) totalItems visibleItems
  emit node

/-- Create a horizontal scrollbar. -/
def hScrollbar' (position totalItems visibleItems : Nat) (length : Nat := 20)
    (config : ScrollbarConfig := {}) : WidgetM Unit :=
  scrollbar' position totalItems visibleItems { config with orientation := .horizontal, length }

/-- Create a vertical scrollbar. -/
def vScrollbar' (position totalItems visibleItems : Nat) (length : Nat := 10)
    (config : ScrollbarConfig := {}) : WidgetM Unit :=
  scrollbar' position totalItems visibleItems { config with orientation := .vertical, length }

end Terminus.Reactive
