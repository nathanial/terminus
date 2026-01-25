-- Terminus.Core.Buffer: 2D grid of styled cells

import Terminus.Core.Cell
import Terminus.Core.Rect
import Terminus.Core.Unicode

namespace Terminus

/-- A 2D buffer of cells representing terminal content -/
structure Buffer where
  width : Nat
  height : Nat
  cells : Array Cell
  deriving Repr, Inhabited

namespace Buffer

/-- Create a new buffer filled with empty cells -/
def new (width height : Nat) : Buffer := {
  width
  height
  cells := Array.replicate (width * height) Cell.empty
}

/-- Create a buffer from a Rect -/
def fromRect (r : Rect) : Buffer := new r.width r.height

/-- Get the area of the buffer -/
def area (buf : Buffer) : Nat := buf.width * buf.height

/-- Convert (x, y) coordinates to array index -/
def index (buf : Buffer) (x y : Nat) : Nat := y * buf.width + x

/-- Check if coordinates are within bounds -/
def inBounds (buf : Buffer) (x y : Nat) : Bool :=
  x < buf.width && y < buf.height

/-- Get a cell at (x, y), returns empty cell if out of bounds -/
def get (buf : Buffer) (x y : Nat) : Cell :=
  if buf.inBounds x y then
    buf.cells.getD (buf.index x y) Cell.empty
  else
    Cell.empty

/-- Set a cell at (x, y), returns unchanged buffer if out of bounds -/
def set (buf : Buffer) (x y : Nat) (cell : Cell) : Buffer :=
  if buf.inBounds x y then
    { buf with cells := buf.cells.modify (buf.index x y) (fun _ => cell) }
  else
    buf

/-- Set a character at (x, y) with default style -/
def setChar (buf : Buffer) (x y : Nat) (c : Char) : Buffer :=
  buf.set x y (Cell.new c)

/-- Set a styled character at (x, y) -/
def setStyled (buf : Buffer) (x y : Nat) (c : Char) (s : Style) : Buffer :=
  buf.set x y (Cell.styled c s)

/-- Fill the entire buffer with a cell -/
def fill (buf : Buffer) (cell : Cell) : Buffer :=
  { buf with cells := Array.replicate buf.cells.size cell }

/-- Fill a rectangular region with a cell -/
def fillRect (buf : Buffer) (r : Rect) (cell : Cell) : Buffer := Id.run do
  let mut result := buf
  for y in [r.y : r.y + r.height] do
    for x in [r.x : r.x + r.width] do
      result := result.set x y cell
  result

/-- Write a string horizontally starting at (x, y), handling Unicode widths -/
def writeString (buf : Buffer) (x y : Nat) (s : String) (style : Style := {}) : Buffer := Id.run do
  let mut result := buf
  let mut col := x
  for c in s.toList do
    let width := c.displayWidth
    -- Skip zero-width characters (combining marks)
    if width == 0 then
      continue
    result := result.set col y (Cell.styled c style)
    -- For wide characters, mark the next cell as a placeholder
    if width == 2 then
      result := result.set (col + 1) y Cell.placeholder
    col := col + width
  result

/-- Write a string within bounds, truncating if necessary, handling Unicode widths -/
def writeStringBounded (buf : Buffer) (x y : Nat) (maxWidth : Nat) (s : String) (style : Style := {}) : Buffer := Id.run do
  let mut result := buf
  let mut col := x
  let endCol := min (x + maxWidth) buf.width
  for c in s.toList do
    let width := c.displayWidth
    -- Skip zero-width characters (combining marks)
    if width == 0 then
      continue
    -- Check if character fits (including placeholder for wide chars)
    if col + width > endCol then break
    result := result.set col y (Cell.styled c style)
    -- For wide characters, mark the next cell as a placeholder
    if width == 2 then
      result := result.set (col + 1) y Cell.placeholder
    col := col + width
  result

/-- Write a hyperlinked string starting at (x, y), handling Unicode widths -/
def writeLink (buf : Buffer) (x y : Nat) (s : String) (url : String) (style : Style := {}) : Buffer := Id.run do
  let mut result := buf
  let mut col := x
  for c in s.toList do
    let width := c.displayWidth
    -- Skip zero-width characters (combining marks)
    if width == 0 then
      continue
    result := result.set col y (Cell.link c style url)
    -- For wide characters, mark the next cell as a placeholder
    if width == 2 then
      result := result.set (col + 1) y Cell.placeholder
    col := col + width
  result

/-- Write a hyperlinked string within bounds, truncating if necessary, handling Unicode widths -/
def writeLinkBounded (buf : Buffer) (x y : Nat) (maxWidth : Nat) (s : String) (url : String) (style : Style := {}) : Buffer := Id.run do
  let mut result := buf
  let mut col := x
  let endCol := min (x + maxWidth) buf.width
  for c in s.toList do
    let width := c.displayWidth
    -- Skip zero-width characters (combining marks)
    if width == 0 then
      continue
    -- Check if character fits (including placeholder for wide chars)
    if col + width > endCol then break
    result := result.set col y (Cell.link c style url)
    -- For wide characters, mark the next cell as a placeholder
    if width == 2 then
      result := result.set (col + 1) y Cell.placeholder
    col := col + width
  result

/-- Clear the buffer (fill with empty cells) -/
def clear (buf : Buffer) : Buffer := buf.fill Cell.empty

/-- Resize the buffer, preserving content where possible -/
def resize (buf : Buffer) (newWidth newHeight : Nat) : Buffer := Id.run do
  let mut newBuf := Buffer.new newWidth newHeight
  for y in [0 : min buf.height newHeight] do
    for x in [0 : min buf.width newWidth] do
      newBuf := newBuf.set x y (buf.get x y)
  newBuf

/-- Get the bounds as a Rect -/
def toRect (buf : Buffer) : Rect := { x := 0, y := 0, width := buf.width, height := buf.height }

/-- Compute differences between two buffers -/
def diff (old new_ : Buffer) : List (Nat × Nat × Cell) := Id.run do
  let mut changes : List (Nat × Nat × Cell) := []
  for y in [0 : new_.height] do
    for x in [0 : new_.width] do
      let newCell := new_.get x y
      let oldCell := old.get x y
      if newCell != oldCell then
        changes := (x, y, newCell) :: changes
  changes.reverse

/-- Merge another buffer on top at the given offset -/
def merge (buf : Buffer) (other : Buffer) (offsetX offsetY : Nat) : Buffer := Id.run do
  let mut result := buf
  for y in [0 : other.height] do
    for x in [0 : other.width] do
      let cell := other.get x y
      result := result.set (offsetX + x) (offsetY + y) cell
  result

/-- Convert buffer to plain text string (no styling, just characters).
    Wide character placeholders are skipped, and trailing spaces are trimmed. -/
def toPlainText (buf : Buffer) : String := Id.run do
  let mut lines : Array String := #[]
  for y in [0 : buf.height] do
    let mut line := ""
    for x in [0 : buf.width] do
      let cell := buf.get x y
      if cell.isPlaceholder then continue  -- Skip wide char placeholders
      line := line ++ cell.char.toString
    lines := lines.push line.trimRight  -- Trim trailing spaces
  String.intercalate "\n" lines.toList

end Buffer

end Terminus
