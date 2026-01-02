-- Terminus.Widgets.Table: Table widget with headers and rows

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Layout.Constraint

namespace Terminus

/-- Table cell content -/
structure TableCell where
  content : String
  style : Style := {}
  deriving Repr, Inhabited

namespace TableCell

def new (s : String) : TableCell := { content := s }
def styled (s : String) (st : Style) : TableCell := { content := s, style := st }

end TableCell

/-- Table row -/
structure TableRow where
  cells : List TableCell
  height : Nat := 1
  deriving Repr, Inhabited

namespace TableRow

def new (cells : List String) : TableRow := {
  cells := cells.map TableCell.new
}

def fromCells (cells : List TableCell) : TableRow := { cells }

end TableRow

/-- Column constraint for table layout -/
inductive ColumnWidth where
  | fixed (width : Nat)
  | percent (pct : Nat)
  | ratio (n : Nat)
  | fill
  deriving Repr, BEq, Inhabited

/-- Table widget -/
structure Table where
  header : Option TableRow := none
  rows : List TableRow := []
  widths : List ColumnWidth := []
  selected : Option Nat := none
  headerStyle : Style := Style.bold
  selectedStyle : Style := Style.reversed
  borderStyle : Style := {}
  block : Option Block := none
  showHeader : Bool := true
  columnSpacing : Nat := 1
  deriving Repr, Inhabited

namespace Table

def new (rows : List (List String)) : Table := {
  rows := rows.map TableRow.new
}

def withHeader (t : Table) (header : List String) : Table := {
  t with header := some (TableRow.new header)
}

def withWidths (t : Table) (widths : List ColumnWidth) : Table := { t with widths }

def withSelected (t : Table) (idx : Nat) : Table :=
  if idx < t.rows.length then { t with selected := some idx }
  else t

def withBlock (t : Table) (b : Block) : Table := { t with block := some b }
def withHeaderStyle (t : Table) (s : Style) : Table := { t with headerStyle := s }
def withSelectedStyle (t : Table) (s : Style) : Table := { t with selectedStyle := s }

def selectNext (t : Table) : Table :=
  match t.selected with
  | none => if t.rows.isEmpty then t else { t with selected := some 0 }
  | some idx =>
    let newIdx := if idx + 1 >= t.rows.length then idx else idx + 1
    { t with selected := some newIdx }

def selectPrev (t : Table) : Table :=
  match t.selected with
  | none => if t.rows.isEmpty then t else { t with selected := some (t.rows.length - 1) }
  | some idx =>
    let newIdx := if idx == 0 then 0 else idx - 1
    { t with selected := some newIdx }

/-- Calculate column widths based on constraints -/
private def computeColumnWidths (widths : List ColumnWidth) (numCols totalWidth spacing : Nat) : List Nat := Id.run do
  if numCols == 0 then return []

  let totalSpacing := if numCols > 1 then spacing * (numCols - 1) else 0
  let available := if totalWidth > totalSpacing then totalWidth - totalSpacing else 0

  -- If no width specs, distribute evenly
  if widths.isEmpty then
    let colWidth := available / numCols
    return List.replicate numCols colWidth

  let mut baseSizes : List Nat := []
  let mut weights : List Nat := []
  let mut fixedTotal : Nat := 0
  let mut remainingForFixed : Nat := available

  for i in [0 : numCols] do
    let w := widths.getD i .fill
    match w with
    | .fixed size =>
      let s := min size remainingForFixed
      baseSizes := baseSizes ++ [s]
      weights := weights ++ [0]
      fixedTotal := fixedTotal + s
      remainingForFixed := if remainingForFixed > s then remainingForFixed - s else 0
    | .percent pct =>
      let desired := (available * (min pct 100)) / 100
      let s := min desired remainingForFixed
      baseSizes := baseSizes ++ [s]
      weights := weights ++ [0]
      fixedTotal := fixedTotal + s
      remainingForFixed := if remainingForFixed > s then remainingForFixed - s else 0
    | .ratio n =>
      baseSizes := baseSizes ++ [0]
      weights := weights ++ [n]
    | .fill =>
      baseSizes := baseSizes ++ [0]
      weights := weights ++ [1]

  -- Distribute remaining space
  let remaining := if available > fixedTotal then available - fixedTotal else 0
  let weightSum := weights.foldl (· + ·) 0

  let mut finalResult : List Nat := []
  for i in [0 : numCols] do
    let base := baseSizes.getD i 0
    let w := weights.getD i 0
    let size := if w == 0 || weightSum == 0 then base else (remaining * w) / weightSum
    finalResult := finalResult ++ [size]

  finalResult

/-- Render a single row -/
private def renderRow (row : TableRow) (colWidths : List Nat) (x y : Nat) (style : Style) (spacing : Nat) (buf : Buffer) : Buffer := Id.run do
  let mut result := buf
  let mut col := x

  for i in [0 : colWidths.length] do
    let width := colWidths.getD i 0
    let cell := row.cells.getD i (TableCell.new "")
    let cellStyle := Style.merge style cell.style
    result := result.writeStringBounded col y width cell.content cellStyle
    col := col + width + spacing

  result

end Table

instance : Widget Table where
  render t area buf := Id.run do
    -- Render block if present
    let mut result := match t.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get content area
    let contentArea := match t.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty then return result

    -- Determine number of columns
    let numCols := match t.header with
      | some h => h.cells.length
      | none => t.rows.headD (TableRow.new []) |>.cells.length

    if numCols == 0 then return result

    -- Calculate column widths
    let colWidths := Table.computeColumnWidths t.widths numCols contentArea.width t.columnSpacing

    let mut row := contentArea.y

    -- Render header
    if t.showHeader then
      match t.header with
      | some header =>
        result := Table.renderRow header colWidths contentArea.x row t.headerStyle t.columnSpacing result
        row := row + 1
      | none => pure ()

    -- Render data rows
    for i in [0 : t.rows.length] do
      if row >= contentArea.y + contentArea.height then break

      let dataRow := t.rows.getD i (TableRow.new [])
      let style := if t.selected == some i then t.selectedStyle else {}
      result := Table.renderRow dataRow colWidths contentArea.x row style t.columnSpacing result
      row := row + 1

    result

end Terminus
