-- TerminusTests.TableTests: Tests for Table widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Widgets.Widget
import Terminus.Widgets.Table

namespace TerminusTests.TableTests

open Terminus
open Crucible

testSuite "Table Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "TableCell.new creates cell with content" := do
  let cell := TableCell.new "Hello"
  cell.content ≡ "Hello"

test "TableCell.styled creates cell with style" := do
  let cell := TableCell.styled "Bold" Style.bold
  cell.content ≡ "Bold"
  cell.style.modifier.bold ≡ true

test "TableRow.new creates row from string list" := do
  let row := TableRow.new ["A", "B", "C"]
  row.cells.length ≡ 3

test "Table.new creates table from rows" := do
  let table := Table.new [["A", "B"], ["C", "D"]]
  table.rows.length ≡ 2

test "Table.withHeader sets header row" := do
  let table := Table.new [["1", "2"]] |>.withHeader ["Col1", "Col2"]
  table.header.isSome ≡ true

test "Table.withSelected sets selection index" := do
  let table := Table.new [["A"], ["B"], ["C"]] |>.withSelected 1
  table.selected ≡ some 1

test "Table.withSelected ignores out of bounds" := do
  let table := Table.new [["A"], ["B"]] |>.withSelected 10
  table.selected ≡ none

test "Table.selectNext advances selection" := do
  let table := Table.new [["A"], ["B"], ["C"]] |>.withSelected 0 |>.selectNext
  table.selected ≡ some 1

test "Table.selectNext stops at end" := do
  let table := Table.new [["A"], ["B"]] |>.withSelected 1 |>.selectNext
  table.selected ≡ some 1

test "Table.selectPrev moves selection back" := do
  let table := Table.new [["A"], ["B"], ["C"]] |>.withSelected 2 |>.selectPrev
  table.selected ≡ some 1

test "Table.selectPrev stops at beginning" := do
  let table := Table.new [["A"], ["B"]] |>.withSelected 0 |>.selectPrev
  table.selected ≡ some 0

test "Table renders without crash" := do
  let table := Table.new [["A", "B"], ["C", "D"]] |>.withHeader ["X", "Y"]
  let buf := renderWidget table 20 10
  buf.width ≡ 20

#generate_tests

end TerminusTests.TableTests
