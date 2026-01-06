-- Terminus.Widgets.Canvas: Free-form drawing with Braille characters

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-
  Braille character encoding:
  Each terminal cell can display a 2x4 dot pattern using Unicode Braille characters.
  Dots are numbered:
    1 4
    2 5
    3 6
    7 8

  The character code is U+2800 + (sum of dot bit values)
  Dot 1 = 0x01, Dot 2 = 0x02, Dot 3 = 0x04, Dot 4 = 0x08
  Dot 5 = 0x10, Dot 6 = 0x20, Dot 7 = 0x40, Dot 8 = 0x80
-/

/-- Braille dot positions and their bit values -/
private def brailleDotBit (row col : Nat) : UInt8 :=
  -- row: 0-3 (top to bottom), col: 0-1 (left to right)
  match (row, col) with
  | (0, 0) => 0x01  -- Dot 1
  | (1, 0) => 0x02  -- Dot 2
  | (2, 0) => 0x04  -- Dot 3
  | (3, 0) => 0x40  -- Dot 7
  | (0, 1) => 0x08  -- Dot 4
  | (1, 1) => 0x10  -- Dot 5
  | (2, 1) => 0x20  -- Dot 6
  | (3, 1) => 0x80  -- Dot 8
  | _ => 0

/-- Convert a bit pattern to a Braille character -/
private def brailleChar (pattern : UInt8) : Char :=
  Char.ofNat (0x2800 + pattern.toNat)

/-- A 2D grid of braille dots for sub-cell resolution drawing -/
structure BrailleGrid where
  /-- Width in terminal cells -/
  cellWidth : Nat
  /-- Height in terminal cells -/
  cellHeight : Nat
  /-- Dot patterns for each cell (row-major order) -/
  patterns : Array UInt8
  /-- Style for each cell -/
  styles : Array Style
  deriving Repr, Inhabited

namespace BrailleGrid

def new (cellWidth cellHeight : Nat) : BrailleGrid := {
  cellWidth
  cellHeight
  patterns := Array.replicate (cellWidth * cellHeight) 0
  styles := Array.replicate (cellWidth * cellHeight) Style.default
}

/-- Get pixel dimensions (2x width, 4x height) -/
def pixelWidth (g : BrailleGrid) : Nat := g.cellWidth * 2
def pixelHeight (g : BrailleGrid) : Nat := g.cellHeight * 4

/-- Set a single pixel (in pixel coordinates) -/
def setPixel (g : BrailleGrid) (px py : Nat) (style : Style) : BrailleGrid :=
  let cellX := px / 2
  let cellY := py / 4
  let dotCol := px % 2
  let dotRow := py % 4

  if cellX >= g.cellWidth || cellY >= g.cellHeight then g
  else
    let idx := cellY * g.cellWidth + cellX
    let bit := brailleDotBit dotRow dotCol
    let oldPattern := g.patterns.getD idx 0
    let newPattern := oldPattern ||| bit
    { g with
      patterns := g.patterns.modify idx (fun _ => newPattern)
      styles := g.styles.modify idx (fun _ => style)
    }

/-- Clear a single pixel -/
def clearPixel (g : BrailleGrid) (px py : Nat) : BrailleGrid :=
  let cellX := px / 2
  let cellY := py / 4
  let dotCol := px % 2
  let dotRow := py % 4

  if cellX >= g.cellWidth || cellY >= g.cellHeight then g
  else
    let idx := cellY * g.cellWidth + cellX
    let bit := brailleDotBit dotRow dotCol
    let oldPattern := g.patterns.getD idx 0
    let newPattern := oldPattern &&& (~~~bit)
    { g with patterns := g.patterns.modify idx (fun _ => newPattern) }

/-- Clear the entire grid -/
def clear (g : BrailleGrid) : BrailleGrid :=
  { g with
    patterns := Array.replicate (g.cellWidth * g.cellHeight) 0
    styles := Array.replicate (g.cellWidth * g.cellHeight) Style.default
  }

/-- Draw a line using Bresenham's algorithm -/
def drawLine (g : BrailleGrid) (x0 y0 x1 y1 : Int) (style : Style) : BrailleGrid := Id.run do
  let mut grid := g

  let dx := (x1 - x0).natAbs
  let dy := (y1 - y0).natAbs
  let sx : Int := if x0 < x1 then 1 else -1
  let sy : Int := if y0 < y1 then 1 else -1

  -- Standard Bresenham: err = dx - dy, use 2*err for comparisons
  let mut err : Int := Int.ofNat dx - Int.ofNat dy
  let mut x := x0
  let mut y := y0

  while true do
    if x >= 0 && y >= 0 then
      grid := grid.setPixel x.toNat y.toNat style

    if x == x1 && y == y1 then break

    let e2 := 2 * err
    -- Move in X direction if e2 > -dy (i.e., e2 >= -dy + 1)
    if e2 > -(Int.ofNat dy) then
      err := err - Int.ofNat dy
      x := x + sx
    -- Move in Y direction if e2 < dx (i.e., e2 <= dx - 1)
    if e2 < Int.ofNat dx then
      err := err + Int.ofNat dx
      y := y + sy

  grid

/-- Draw a rectangle outline -/
def drawRect (g : BrailleGrid) (x y w h : Nat) (style : Style) : BrailleGrid := Id.run do
  let mut grid := g
  let x1 := x + w - 1
  let y1 := y + h - 1

  -- Top and bottom edges
  for px in [x:x + w] do
    grid := grid.setPixel px y style
    grid := grid.setPixel px y1 style

  -- Left and right edges
  for py in [y:y + h] do
    grid := grid.setPixel x py style
    grid := grid.setPixel x1 py style

  grid

/-- Fill a rectangle -/
def fillRect (g : BrailleGrid) (x y w h : Nat) (style : Style) : BrailleGrid := Id.run do
  let mut grid := g
  for py in [y:y + h] do
    for px in [x:x + w] do
      grid := grid.setPixel px py style
  grid

/-- Draw a circle outline using midpoint algorithm -/
def drawCircle (g : BrailleGrid) (cx cy r : Int) (style : Style) : BrailleGrid := Id.run do
  let mut grid := g
  let mut x := r
  let mut y : Int := 0
  let mut err := 1 - r

  while x >= y do
    -- Draw 8 octants
    let points := [(x, y), (y, x), (-y, x), (-x, y), (-x, -y), (-y, -x), (y, -x), (x, -y)]
    for (dx, dy) in points do
      let px := cx + dx
      let py := cy + dy
      if px >= 0 && py >= 0 then
        grid := grid.setPixel px.toNat py.toNat style

    y := y + 1
    if err < 0 then
      err := err + 2 * y + 1
    else
      x := x - 1
      err := err + 2 * (y - x) + 1

  grid

/-- Get the character and style for a cell -/
def getCell (g : BrailleGrid) (cellX cellY : Nat) : (Char × Style) :=
  if cellX >= g.cellWidth || cellY >= g.cellHeight then ('⠀', Style.default)
  else
    let idx := cellY * g.cellWidth + cellX
    (brailleChar (g.patterns.getD idx 0), g.styles.getD idx Style.default)

end BrailleGrid

/-- Shape types for Canvas -/
inductive CanvasShape where
  | point (x y : Float) (style : Style)
  | line (x1 y1 x2 y2 : Float) (style : Style)
  | rect (x y w h : Float) (style : Style) (filled : Bool)
  | circle (cx cy r : Float) (style : Style) (filled : Bool)
  deriving Repr, Inhabited

/-- Canvas widget for free-form drawing -/
structure Canvas where
  shapes : List CanvasShape := []
  background : Style := Style.default
  block : Option Block := none
  deriving Repr, Inhabited

namespace Canvas

def new : Canvas := {}

def withBackground (c : Canvas) (s : Style) : Canvas := { c with background := s }
def withBlock (c : Canvas) (b : Block) : Canvas := { c with block := some b }

def addShape (c : Canvas) (s : CanvasShape) : Canvas := { c with shapes := c.shapes ++ [s] }
def clearShapes (c : Canvas) : Canvas := { c with shapes := [] }

/-- Add a point -/
def point (c : Canvas) (x y : Float) (style : Style := Style.default) : Canvas :=
  c.addShape (.point x y style)

/-- Add a line -/
def line (c : Canvas) (x1 y1 x2 y2 : Float) (style : Style := Style.default) : Canvas :=
  c.addShape (.line x1 y1 x2 y2 style)

/-- Add a rectangle outline -/
def rect (c : Canvas) (x y w h : Float) (style : Style := Style.default) : Canvas :=
  c.addShape (.rect x y w h style false)

/-- Add a filled rectangle -/
def filledRect (c : Canvas) (x y w h : Float) (style : Style := Style.default) : Canvas :=
  c.addShape (.rect x y w h style true)

/-- Add a circle outline -/
def circle (c : Canvas) (cx cy r : Float) (style : Style := Style.default) : Canvas :=
  c.addShape (.circle cx cy r style false)

/-- Add a filled circle -/
def filledCircle (c : Canvas) (cx cy r : Float) (style : Style := Style.default) : Canvas :=
  c.addShape (.circle cx cy r style true)

/-- Render shapes onto a BrailleGrid -/
def renderToGrid (c : Canvas) (grid : BrailleGrid) : BrailleGrid := Id.run do
  let mut g := grid

  for shape in c.shapes do
    match shape with
    | .point x y style =>
      let px := (x * 2).toUInt32.toNat
      let py := (y * 4).toUInt32.toNat
      g := g.setPixel px py style

    | .line x1 y1 x2 y2 style =>
      let px1 := Int.ofNat (x1 * 2).toUInt32.toNat
      let py1 := Int.ofNat (y1 * 4).toUInt32.toNat
      let px2 := Int.ofNat (x2 * 2).toUInt32.toNat
      let py2 := Int.ofNat (y2 * 4).toUInt32.toNat
      g := g.drawLine px1 py1 px2 py2 style

    | .rect x y w h style filled =>
      let px := (x * 2).toUInt32.toNat
      let py := (y * 4).toUInt32.toNat
      let pw := (w * 2).toUInt32.toNat
      let ph := (h * 4).toUInt32.toNat
      if filled then
        g := g.fillRect px py pw ph style
      else
        g := g.drawRect px py pw ph style

    | .circle cx cy r style filled =>
      let pcx := Int.ofNat (cx * 2).toUInt32.toNat
      let pcy := Int.ofNat (cy * 4).toUInt32.toNat
      let pr := Int.ofNat (r * 3).toUInt32.toNat  -- Approximate scaling for aspect ratio
      if filled then
        -- Simple filled circle: draw horizontal lines
        for dyi in [:pr.toNat * 2 + 1] do
          let dy := Int.ofNat dyi - pr
          let dxf := Float.sqrt (Float.ofNat pr.toNat * Float.ofNat pr.toNat - Float.ofInt dy * Float.ofInt dy)
          let dx := Int.ofNat dxf.toUInt32.toNat
          let x1 := pcx - dx
          let x2 := pcx + dx
          let y := pcy + dy
          if y >= 0 then
            g := g.drawLine x1 y x2 y style
      else
        g := g.drawCircle pcx pcy pr style

  g

end Canvas

instance : Widget Canvas where
  render c area buf := Id.run do
    -- Render block if present
    let mut result := match c.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get content area
    let contentArea := match c.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty then return result

    -- Create braille grid and render shapes
    let grid := BrailleGrid.new contentArea.width contentArea.height
    let renderedGrid := c.renderToGrid grid

    -- Copy braille grid to buffer
    for cellY in [:contentArea.height] do
      for cellX in [:contentArea.width] do
        let (char, style) := renderedGrid.getCell cellX cellY
        let x := contentArea.x + cellX
        let y := contentArea.y + cellY
        -- Only draw non-empty braille characters
        if char != '⠀' then
          result := result.setStyled x y char style

    result

end Terminus
