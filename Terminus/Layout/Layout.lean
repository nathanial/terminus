-- Terminus.Layout.Layout: Layout solver for splitting areas

import Terminus.Core.Rect
import Terminus.Layout.Constraint

namespace Terminus

/-- Layout configuration -/
structure Layout where
  direction : Direction := .vertical
  constraints : List Constraint := []
  margin : Nat := 0
  spacing : Nat := 0
  deriving Repr, Inhabited

namespace Layout

/-- Create a vertical layout -/
def vertical (constraints : List Constraint) : Layout := {
  direction := .vertical
  constraints
}

/-- Create a horizontal layout -/
def horizontal (constraints : List Constraint) : Layout := {
  direction := .horizontal
  constraints
}

/-- Add margin to the layout -/
def withMargin (l : Layout) (m : Nat) : Layout := { l with margin := m }

/-- Add spacing between elements -/
def withSpacing (l : Layout) (s : Nat) : Layout := { l with spacing := s }

/-- Calculate sizes for each constraint -/
private def computeSizes (constraints : List Constraint) (available : Nat) (spacing : Nat) : List Nat := Id.run do
  let n := constraints.length
  if n == 0 then return []

  -- Account for spacing between elements
  let totalSpacing := if n > 1 then spacing * (n - 1) else 0
  let availableForContent := if available > totalSpacing then available - totalSpacing else 0

  -- First pass: resolve fixed, percent, and ratio constraints
  let mut sizes : List Nat := []
  let mut fixedTotal : Nat := 0
  let mut fillCount : Nat := 0
  let mut ratioSum : Nat := 0

  for c in constraints do
    match c with
    | .fixed size =>
      let s := min size availableForContent
      sizes := sizes ++ [s]
      fixedTotal := fixedTotal + s
    | .percent pct =>
      let s := (availableForContent * (min pct 100)) / 100
      sizes := sizes ++ [s]
      fixedTotal := fixedTotal + s
    | .ratio num _ =>
      sizes := sizes ++ [num] -- Store ratio numerator temporarily
      ratioSum := ratioSum + num
    | .min size =>
      sizes := sizes ++ [size]
      fixedTotal := fixedTotal + size
    | .max _ =>
      sizes := sizes ++ [0] -- Will be filled later
      fillCount := fillCount + 1
    | .fill =>
      sizes := sizes ++ [0] -- Will be filled later
      fillCount := fillCount + 1

  -- Calculate remaining space
  let remaining := if availableForContent > fixedTotal then availableForContent - fixedTotal else 0

  -- Second pass: distribute remaining space to fill/max constraints
  let fillSize := if fillCount > 0 then remaining / fillCount else 0
  let ratioSpace := if ratioSum > 0 then remaining else 0

  let mut finalSizes : List Nat := []
  let mut idx := 0
  for c in constraints do
    match c with
    | .fill | .max _ =>
      finalSizes := finalSizes ++ [fillSize]
    | .ratio num denom =>
      let size := if denom == 0 || ratioSum == 0 then 0
                  else (ratioSpace * num) / ratioSum
      finalSizes := finalSizes ++ [size]
    | _ =>
      finalSizes := finalSizes ++ [sizes.getD idx 0]
    idx := idx + 1

  finalSizes

/-- Split a rectangle according to layout constraints -/
def split (l : Layout) (area : Rect) : List Rect := Id.run do
  let innerArea := area.inner l.margin
  if innerArea.isEmpty || l.constraints.isEmpty then
    return []

  let totalSize := match l.direction with
    | .horizontal => innerArea.width
    | .vertical => innerArea.height

  let sizes := computeSizes l.constraints totalSize l.spacing

  let mut rects : List Rect := []
  let mut offset : Nat := 0

  for i in [0 : sizes.length] do
    let size := sizes.getD i 0

    let rect := match l.direction with
      | .horizontal => {
          x := innerArea.x + offset
          y := innerArea.y
          width := size
          height := innerArea.height
        }
      | .vertical => {
          x := innerArea.x
          y := innerArea.y + offset
          width := innerArea.width
          height := size
        }

    rects := rects ++ [rect]
    offset := offset + size + l.spacing

  rects

end Layout

/-- Convenience function to split vertically with constraints -/
def vsplit (area : Rect) (constraints : List Constraint) : List Rect :=
  Layout.vertical constraints |>.split area

/-- Convenience function to split horizontally with constraints -/
def hsplit (area : Rect) (constraints : List Constraint) : List Rect :=
  Layout.horizontal constraints |>.split area

end Terminus
