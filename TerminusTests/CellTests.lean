-- TerminusTests.CellTests: Tests for Cell operations and Unicode width

import Crucible
import Terminus.Core.Cell
import Terminus.Core.Style
import Terminus.Core.Unicode

namespace TerminusTests.CellTests

open Terminus
open Crucible

testSuite "Cell Tests"

-- ============================================================================
-- Cell Tests
-- ============================================================================

test "Cell.empty has space character" := do
  Cell.empty.char ≡ ' '

test "Cell.empty has default style" := do
  Cell.empty.style ≡ Style.default

test "Cell.new creates cell with character" := do
  let cell := Cell.new 'X'
  cell.char ≡ 'X'

test "Cell.styled creates styled cell" := do
  let style := Style.bold.withFg Color.red
  let cell := Cell.styled 'A' style
  cell.char ≡ 'A'
  cell.style.modifier.bold ≡ true

test "Cell.withHyperlink adds hyperlink" := do
  let cell := Cell.new 'L' |>.withHyperlink "https://example.com"
  cell.hyperlink ≡ some "https://example.com"

test "Cell.link creates hyperlinked cell" := do
  let cell := Cell.link 'X' Style.underline "https://test.com"
  cell.char ≡ 'X'
  cell.hyperlink ≡ some "https://test.com"
  cell.style.modifier.underline ≡ true

test "Cell.placeholder is marked as placeholder" := do
  let cell := Cell.placeholder
  cell.isPlaceholder ≡ true

test "Cell.empty is not a placeholder" := do
  Cell.empty.isPlaceholder ≡ false

-- ============================================================================
-- Unicode Width Tests
-- ============================================================================

test "Char.displayWidth ASCII has width 1" := do
  'A'.displayWidth ≡ 1
  'z'.displayWidth ≡ 1
  ' '.displayWidth ≡ 1
  '!'.displayWidth ≡ 1

test "Char.displayWidth CJK has width 2" := do
  '中'.displayWidth ≡ 2
  '日'.displayWidth ≡ 2
  '本'.displayWidth ≡ 2

test "Char.displayWidth Hangul has width 2" := do
  '한'.displayWidth ≡ 2
  '글'.displayWidth ≡ 2

test "Char.displayWidth Hiragana has width 2" := do
  'あ'.displayWidth ≡ 2
  'い'.displayWidth ≡ 2

test "Char.displayWidth control characters have width 0" := do
  '\x00'.displayWidth ≡ 0
  '\x1F'.displayWidth ≡ 0

test "String.displayWidth sums correctly for ASCII" := do
  "Hello".displayWidth ≡ 5
  "".displayWidth ≡ 0

test "String.displayWidth sums correctly for CJK" := do
  "中文".displayWidth ≡ 4
  "日本語".displayWidth ≡ 6

test "String.displayWidth mixed content" := do
  "Hi中".displayWidth ≡ 4
  "a日b".displayWidth ≡ 4



end TerminusTests.CellTests
