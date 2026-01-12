/-
  Terminus Reactive - Canvas Widget
  Free-form drawing with Braille characters for sub-cell resolution.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Braille Encoding

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

/-- Empty Braille character (no dots) -/
private def emptyBraille : Char := '⠀'

/-! ## Braille Grid -/

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

/-- Create a new empty braille grid -/
def new (cellWidth cellHeight : Nat) : BrailleGrid := {
  cellWidth
  cellHeight
  patterns := Array.replicate (cellWidth * cellHeight) 0
  styles := Array.replicate (cellWidth * cellHeight) Style.default
}

/-- Get pixel dimensions (2x width, 4x height) -/
def pixelWidth (g : BrailleGrid) : Nat := g.cellWidth * 2

/-- Get pixel dimensions (2x width, 4x height) -/
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

  let mut err : Int := Int.ofNat dx - Int.ofNat dy
  let mut x := x0
  let mut y := y0

  while true do
    if x >= 0 && y >= 0 then
      grid := grid.setPixel x.toNat y.toNat style

    if x == x1 && y == y1 then break

    let e2 := 2 * err
    if e2 > -(Int.ofNat dy) then
      err := err - Int.ofNat dy
      x := x + sx
    if e2 < Int.ofNat dx then
      err := err + Int.ofNat dx
      y := y + sy

  grid

/-- Draw a rectangle outline -/
def drawRect (g : BrailleGrid) (x y w h : Nat) (style : Style) : BrailleGrid := Id.run do
  let mut grid := g
  let x1 := x + w - 1
  let y1 := y + h - 1

  for px in [x:x + w] do
    grid := grid.setPixel px y style
    grid := grid.setPixel px y1 style

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
  if cellX >= g.cellWidth || cellY >= g.cellHeight then (emptyBraille, Style.default)
  else
    let idx := cellY * g.cellWidth + cellX
    (brailleChar (g.patterns.getD idx 0), g.styles.getD idx Style.default)

/-- Convert the grid to an array of RNodes (one per row) -/
def toRNodes (g : BrailleGrid) : Array RNode := Id.run do
  let mut rows : Array RNode := #[]
  for cellY in [:g.cellHeight] do
    let mut rowNodes : Array RNode := #[]
    for cellX in [:g.cellWidth] do
      let (char, style) := g.getCell cellX cellY
      rowNodes := rowNodes.push (RNode.text (String.singleton char) style)
    rows := rows.push (RNode.row 0 {} rowNodes)
  rows

end BrailleGrid

/-! ## Canvas Configuration -/

/-- Configuration for canvas widget -/
structure CanvasConfig where
  /-- Width in terminal cells -/
  width : Nat := 20
  /-- Height in terminal cells -/
  height : Nat := 10
  /-- Default style for drawing -/
  style : Style := {}
  deriving Repr, Inhabited

/-! ## Canvas Result -/

/-- Result from creating a canvas widget, with drawing operations -/
structure CanvasResult where
  /-- The current grid state as a dynamic -/
  grid : Reactive.Dynamic Spider BrailleGrid
  /-- Set a pixel at (x, y) in pixel coordinates -/
  setPixel : Nat → Nat → Style → IO Unit
  /-- Clear a pixel at (x, y) -/
  clearPixel : Nat → Nat → IO Unit
  /-- Draw a line from (x0, y0) to (x1, y1) -/
  drawLine : Int → Int → Int → Int → Style → IO Unit
  /-- Draw a rectangle outline -/
  drawRect : Nat → Nat → Nat → Nat → Style → IO Unit
  /-- Fill a rectangle -/
  fillRect : Nat → Nat → Nat → Nat → Style → IO Unit
  /-- Draw a circle outline -/
  drawCircle : Int → Int → Int → Style → IO Unit
  /-- Clear the entire canvas -/
  clear : IO Unit

/-! ## Canvas Widget -/

/-- Create a canvas widget with drawing operations.

    Example:
    ```
    let canvas ← canvas' { width := 40, height := 20, style := { fg := .ansi .green } }
    -- Draw something
    canvas.drawLine 0 0 79 79 { fg := .ansi .cyan }
    canvas.setPixel 40 40 { fg := .ansi .red }
    ```
-/
def canvas' (config : CanvasConfig := {}) : WidgetM CanvasResult := do
  let env ← SpiderM.getEnv
  let initialGrid := BrailleGrid.new config.width config.height

  -- Create mutable grid state
  let gridRef ← SpiderM.liftIO (IO.mkRef initialGrid)

  -- Create trigger event for updates (to signal render refresh)
  let (updateEvent, fireUpdate) ← Reactive.newTriggerEvent (t := Spider) (a := BrailleGrid)

  -- Create dynamic that tracks grid changes
  let gridDyn ← Reactive.holdDyn initialGrid updateEvent

  -- Helper to modify grid and trigger update
  let modifyGrid : (BrailleGrid → BrailleGrid) → IO Unit := fun f => do
    let g ← gridRef.get
    let g' := f g
    gridRef.set g'
    env.withFrame (fireUpdate g')

  -- Build result with drawing operations
  let result : CanvasResult := {
    grid := gridDyn
    setPixel := fun x y style => modifyGrid (·.setPixel x y style)
    clearPixel := fun x y => modifyGrid (·.clearPixel x y)
    drawLine := fun x0 y0 x1 y1 style => modifyGrid (·.drawLine x0 y0 x1 y1 style)
    drawRect := fun x y w h style => modifyGrid (·.drawRect x y w h style)
    fillRect := fun x y w h style => modifyGrid (·.fillRect x y w h style)
    drawCircle := fun cx cy r style => modifyGrid (·.drawCircle cx cy r style)
    clear := modifyGrid (·.clear)
  }

  let node ← gridDyn.map' fun grid =>
    let rows := grid.toRNodes
    RNode.column 0 {} rows
  emit node

  pure result

/-- Create a static canvas (non-interactive, just displays a grid).

    Example:
    ```
    let grid := BrailleGrid.new 20 10
      |>.drawLine 0 0 39 39 {}
      |>.drawRect 5 5 10 10 {}
    staticCanvas' grid
    ```
-/
def staticCanvas' (grid : BrailleGrid) : WidgetM Unit := do
  emitStatic (RNode.column 0 {} grid.toRNodes)

/-- Create a dynamic canvas from a grid dynamic.

    Example:
    ```
    let gridDyn ← someGridSource
    dynCanvas' gridDyn
    ```
-/
def dynCanvas' (grid : Reactive.Dynamic Spider BrailleGrid) : WidgetM Unit := do
  let node ← grid.map' fun g =>
    let rows := g.toRNodes
    RNode.column 0 {} rows
  emit node

end Terminus.Reactive
