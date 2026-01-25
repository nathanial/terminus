-- Terminus.Core.Buffer: 2D grid of styled cells

import Terminus.Core.Cell
import Terminus.Core.Rect
import Terminus.Core.Unicode

namespace Terminus

/-- Row hash for fast diff checks (dual hashes to reduce collision risk). -/
structure RowHash where
  a : UInt64
  b : UInt64
  deriving Repr, BEq, Inhabited

namespace RowHash

def zero : RowHash := { a := 0, b := 0 }

def xor (h1 h2 : RowHash) : RowHash := {
  a := h1.a ^^^ h2.a
  b := h1.b ^^^ h2.b
}

end RowHash

/-- A 2D buffer of cells representing terminal content -/
structure Buffer where
  width : Nat
  height : Nat
  cells : Array Cell
  rowHashes : Array RowHash
  deriving Repr, Inhabited

namespace Buffer

private def mix64 (x : UInt64) : UInt64 :=
  let x := x ^^^ (x >>> 33)
  let x := x * 0xff51afd7ed558ccd
  let x := x ^^^ (x >>> 33)
  let x := x * 0xc4ceb9fe1a85ec53
  x ^^^ (x >>> 33)

private def hashCombine (h x : UInt64) : UInt64 :=
  mix64 (h ^^^ x)

private def fnv1a64 (bytes : ByteArray) : UInt64 := Id.run do
  let mut h : UInt64 := 0xcbf29ce484222325
  for b in bytes.data.toList do
    h := h ^^^ (UInt64.ofNat b.toNat)
    h := h * 0x00000100000001B3
  h

private def hashString (s : String) : UInt64 :=
  fnv1a64 s.toUTF8

private def hashColor16 : Color16 → UInt64
  | .black => 1
  | .red => 2
  | .green => 3
  | .yellow => 4
  | .blue => 5
  | .magenta => 6
  | .cyan => 7
  | .white => 8
  | .brightBlack => 9
  | .brightRed => 10
  | .brightGreen => 11
  | .brightYellow => 12
  | .brightBlue => 13
  | .brightMagenta => 14
  | .brightCyan => 15
  | .brightWhite => 16

private def hashColor : Color → UInt64
  | .default => 0x01
  | .ansi c => hashCombine 0x02 (hashColor16 c)
  | .indexed n => hashCombine 0x03 (UInt64.ofNat n.toNat)
  | .rgb r g b =>
      let packed := (UInt64.ofNat r.toNat <<< 16) ||| (UInt64.ofNat g.toNat <<< 8) ||| UInt64.ofNat b.toNat
      hashCombine 0x04 packed

private def modifierMask (m : Modifier) : UInt64 :=
  let mask : Nat :=
    (if m.bold then 1 else 0) +
    (if m.dim then 2 else 0) +
    (if m.italic then 4 else 0) +
    (if m.underline then 8 else 0) +
    (if m.blink then 16 else 0) +
    (if m.reverse then 32 else 0) +
    (if m.hidden then 64 else 0) +
    (if m.crossedOut then 128 else 0)
  UInt64.ofNat mask

private def hashStyle (s : Style) : UInt64 :=
  let h0 := hashCombine 0x1d2e3f4a (hashColor s.fg)
  let h1 := hashCombine h0 (hashColor s.bg)
  hashCombine h1 (modifierMask s.modifier)

private def hashOptionString : Option String → UInt64
  | none => 0
  | some s => hashCombine 0x9e3779b9 (hashString s)

private def hashCell (cell : Cell) : UInt64 :=
  let h0 := hashCombine 0xa5b35705 (UInt64.ofNat cell.char.toNat)
  let h1 := hashCombine h0 (hashStyle cell.style)
  let h2 := hashCombine h1 (hashOptionString cell.hyperlink)
  let h3 := hashCombine h2 (if cell.isPlaceholder then 1 else 0)
  h3

private def rowContrib (x : Nat) (cellHash : UInt64) : RowHash :=
  let pos := UInt64.ofNat (x + 1)
  let a := mix64 (cellHash ^^^ (pos * 0x9e3779b97f4a7c15))
  let b := mix64 (cellHash ^^^ (pos * 0xc2b2ae3d27d4eb4f))
  { a, b }

private def rowHashForCell (width : Nat) (cell : Cell) : RowHash := Id.run do
  let mut h := RowHash.zero
  let cellHash := hashCell cell
  for x in [0 : width] do
    h := RowHash.xor h (rowContrib x cellHash)
  h

private def emptyRowPrefix (width : Nat) : Array RowHash := Id.run do
  let mut prefixes : Array RowHash := #[]
  let mut h := RowHash.zero
  prefixes := prefixes.push h
  let cellHash := hashCell Cell.empty
  for x in [0 : width] do
    h := RowHash.xor h (rowContrib x cellHash)
    prefixes := prefixes.push h
  prefixes

/-- Create a new buffer filled with empty cells -/
def new (width height : Nat) : Buffer := {
  width
  height
  cells := Array.replicate (width * height) Cell.empty
  rowHashes := Array.replicate height (rowHashForCell width Cell.empty)
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
    let idx := buf.index x y
    let oldCell := buf.cells.getD idx Cell.empty
    if oldCell == cell then
      buf
    else
      let oldHash := hashCell oldCell
      let newHash := hashCell cell
      let rowHash := buf.rowHashes.getD y RowHash.zero
      let updatedRowHash := RowHash.xor rowHash (RowHash.xor (rowContrib x oldHash) (rowContrib x newHash))
      { buf with
        cells := buf.cells.set! idx cell
        rowHashes := buf.rowHashes.set! y updatedRowHash
      }
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
  let rowHash := rowHashForCell buf.width cell
  { buf with
    cells := Array.replicate buf.cells.size cell
    rowHashes := Array.replicate buf.height rowHash
  }

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
  let maxWidth := max old.width new_.width
  let maxHeight := max old.height new_.height
  let emptyPrefix := emptyRowPrefix maxWidth
  let emptyRowHash := emptyPrefix.getD maxWidth RowHash.zero
  let overlapWidth := min old.width new_.width
  let rowHashAt := fun (buf : Buffer) (y : Nat) =>
    if y < buf.height then
      let base := buf.rowHashes.getD y RowHash.zero
      if buf.width == maxWidth then
        base
      else
        let padding := RowHash.xor emptyRowHash (emptyPrefix.getD buf.width RowHash.zero)
        RowHash.xor base padding
    else
      emptyRowHash
  for y in [0 : maxHeight] do
    let inOldRow := y < old.height
    let inNewRow := y < new_.height
    if inNewRow && !inOldRow then
      for x in [0 : new_.width] do
        changes := (x, y, new_.get x y) :: changes
    else if inOldRow && !inNewRow then
      for x in [0 : old.width] do
        let oldCell := old.get x y
        if oldCell != Cell.empty then
          changes := (x, y, Cell.empty) :: changes
    else if inOldRow && inNewRow then
      let rowHashOld := rowHashAt old y
      let rowHashNew := rowHashAt new_ y
      if rowHashOld != rowHashNew then
        for x in [0 : overlapWidth] do
          let newCell := new_.get x y
          let oldCell := old.get x y
          if newCell != oldCell then
            changes := (x, y, newCell) :: changes
      if new_.width > old.width then
        for x in [old.width : new_.width] do
          -- Newly exposed area should always be refreshed, even if empty.
          changes := (x, y, new_.get x y) :: changes
      else if old.width > new_.width then
        for x in [new_.width : old.width] do
          let oldCell := old.get x y
          if oldCell != Cell.empty then
            changes := (x, y, Cell.empty) :: changes
    else
      pure ()
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
