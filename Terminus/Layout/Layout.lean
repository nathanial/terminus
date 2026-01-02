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

/-- Least common multiple for ratio normalization. -/
private def lcm (a b : Nat) : Nat :=
  if a == 0 || b == 0 then 0
  else a / Nat.gcd a b * b

/-- Calculate sizes for each constraint -/
private def computeSizes (constraints : List Constraint) (available : Nat) (spacing : Nat) : List Nat := Id.run do
  let n := constraints.length
  if n == 0 then return []

  -- Account for spacing between elements
  let totalSpacing := if n > 1 then spacing * (n - 1) else 0
  let availableForContent := if available > totalSpacing then available - totalSpacing else 0

  let denomLcm :=
    constraints.foldl
      (fun acc c =>
        match c with
        | .ratio _ denom => if denom == 0 then acc else lcm acc denom
        | _ => acc)
      1

  let weightFor : Constraint → Nat
    | .ratio num denom => if denom == 0 then 0 else num * (denomLcm / denom)
    | .fill => denomLcm
    | .max _ => denomLcm
    | _ => 0

  -- First pass: resolve fixed/percent/min and collect ratio/fill weights.
  let mut baseSizes : List Nat := []
  let mut weights : List Nat := []
  let mut caps : List (Option Nat) := []
  let mut fixedTotal : Nat := 0
  let mut remainingForFixed : Nat := availableForContent

  for c in constraints do
    match c with
    | .fixed size =>
      let s := min size remainingForFixed
      baseSizes := baseSizes ++ [s]
      weights := weights ++ [0]
      caps := caps ++ [none]
      fixedTotal := fixedTotal + s
      remainingForFixed := if remainingForFixed > s then remainingForFixed - s else 0
    | .percent pct =>
      let desired := (availableForContent * (min pct 100)) / 100
      let s := min desired remainingForFixed
      baseSizes := baseSizes ++ [s]
      weights := weights ++ [0]
      caps := caps ++ [none]
      fixedTotal := fixedTotal + s
      remainingForFixed := if remainingForFixed > s then remainingForFixed - s else 0
    | .min size =>
      let s := min size remainingForFixed
      baseSizes := baseSizes ++ [s]
      weights := weights ++ [0]
      caps := caps ++ [none]
      fixedTotal := fixedTotal + s
      remainingForFixed := if remainingForFixed > s then remainingForFixed - s else 0
    | .max size =>
      baseSizes := baseSizes ++ [0]
      weights := weights ++ [weightFor c]
      caps := caps ++ [some size]
    | .fill =>
      baseSizes := baseSizes ++ [0]
      weights := weights ++ [weightFor c]
      caps := caps ++ [none]
    | .ratio _ _ =>
      baseSizes := baseSizes ++ [0]
      weights := weights ++ [weightFor c]
      caps := caps ++ [none]

  -- Distribute remaining space across ratio/fill constraints.
  let remaining := if availableForContent > fixedTotal then availableForContent - fixedTotal else 0
  let weightSum := weights.foldl (· + ·) 0

  let mut finalSizes : List Nat := []
  let mut idx := 0
  for _ in constraints do
    let base := baseSizes.getD idx 0
    let w := weights.getD idx 0
    let cap := caps.getD idx none
    let size :=
      if w == 0 || weightSum == 0 then
        base
      else
        let alloc := (remaining * w) / weightSum
        match cap with
        | some m => min alloc m
        | none => alloc
    finalSizes := finalSizes ++ [size]
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
