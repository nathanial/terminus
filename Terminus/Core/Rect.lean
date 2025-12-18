-- Terminus.Core.Rect: Rectangular regions

namespace Terminus

/-- A rectangular region defined by position and size -/
structure Rect where
  x : Nat := 0
  y : Nat := 0
  width : Nat := 0
  height : Nat := 0
  deriving Repr, BEq, Inhabited

namespace Rect

def new (x y width height : Nat) : Rect := { x, y, width, height }

def area (r : Rect) : Nat := r.width * r.height

def isEmpty (r : Rect) : Bool := r.width == 0 || r.height == 0

def left (r : Rect) : Nat := r.x

def right (r : Rect) : Nat := r.x + r.width

def top (r : Rect) : Nat := r.y

def bottom (r : Rect) : Nat := r.y + r.height

def contains (r : Rect) (x y : Nat) : Bool :=
  x >= r.x && x < r.right && y >= r.y && y < r.bottom

/-- Get the inner rect after applying margin on all sides -/
def inner (r : Rect) (margin : Nat) : Rect :=
  if r.width <= margin * 2 || r.height <= margin * 2 then
    { x := r.x, y := r.y, width := 0, height := 0 }
  else
    { x := r.x + margin
      y := r.y + margin
      width := r.width - margin * 2
      height := r.height - margin * 2 }

/-- Get the inner rect after applying a 1-cell border -/
def innerBorder (r : Rect) : Rect := r.inner 1

/-- Intersect two rectangles -/
def intersect (r1 r2 : Rect) : Rect :=
  let x := max r1.x r2.x
  let y := max r1.y r2.y
  let right := min r1.right r2.right
  let bottom := min r1.bottom r2.bottom
  if x >= right || y >= bottom then
    { x := 0, y := 0, width := 0, height := 0 }
  else
    { x, y, width := right - x, height := bottom - y }

/-- Union of two rectangles (bounding box) -/
def union (r1 r2 : Rect) : Rect :=
  if r1.isEmpty then r2
  else if r2.isEmpty then r1
  else
    let x := min r1.x r2.x
    let y := min r1.y r2.y
    let right := max r1.right r2.right
    let bottom := max r1.bottom r2.bottom
    { x, y, width := right - x, height := bottom - y }

/-- Split horizontally at a given row (relative to top) -/
def splitHorizontal (r : Rect) (row : Nat) : Rect × Rect :=
  let row := min row r.height
  let top := { r with height := row }
  let bottom := { r with y := r.y + row, height := r.height - row }
  (top, bottom)

/-- Split vertically at a given column (relative to left) -/
def splitVertical (r : Rect) (col : Nat) : Rect × Rect :=
  let col := min col r.width
  let left := { r with width := col }
  let right := { r with x := r.x + col, width := r.width - col }
  (left, right)

end Rect

end Terminus
