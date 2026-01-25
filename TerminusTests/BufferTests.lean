-- TerminusTests.BufferTests: Tests for Buffer operations

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Cell
import Terminus.Core.Style

namespace TerminusTests.BufferTests

open Terminus
open Crucible

testSuite "Buffer Tests"

test "Buffer.new creates buffer of correct size" := do
  let buf := Buffer.new 10 5
  buf.width ≡ 10
  buf.height ≡ 5

test "Buffer.new fills with empty cells" := do
  let buf := Buffer.new 3 3
  (buf.get 0 0).char ≡ ' '
  (buf.get 2 2).char ≡ ' '

test "Buffer.set updates cell" := do
  let buf := Buffer.new 5 5
  let buf := buf.set 2 3 (Cell.new 'X')
  (buf.get 2 3).char ≡ 'X'

test "Buffer.set ignores out of bounds" := do
  let buf := Buffer.new 5 5
  let buf := buf.set 10 10 (Cell.new 'X')
  buf.width ≡ 5 -- unchanged

test "Buffer.get returns empty for out of bounds" := do
  let buf := Buffer.new 5 5
  (buf.get 100 100).char ≡ ' '

test "Buffer.writeString writes text horizontally" := do
  let buf := Buffer.new 10 3
  let buf := buf.writeString 2 1 "Hi"
  (buf.get 2 1).char ≡ 'H'
  (buf.get 3 1).char ≡ 'i'

test "Buffer.writeString applies style" := do
  let buf := Buffer.new 10 3
  let buf := buf.writeString 0 0 "X" Style.bold
  (buf.get 0 0).style.modifier.bold ≡ true

test "Buffer.writeString handles CJK wide characters" := do
  let buf := Buffer.new 10 3
  let buf := buf.writeString 0 0 "中文"
  -- First CJK char at column 0
  (buf.get 0 0).char ≡ '中'
  -- Placeholder at column 1
  (buf.get 1 0).isPlaceholder ≡ true
  -- Second CJK char at column 2
  (buf.get 2 0).char ≡ '文'
  -- Placeholder at column 3
  (buf.get 3 0).isPlaceholder ≡ true

test "Buffer.writeString mixed ASCII and CJK" := do
  let buf := Buffer.new 10 3
  let buf := buf.writeString 0 0 "Hi中"
  (buf.get 0 0).char ≡ 'H'
  (buf.get 1 0).char ≡ 'i'
  (buf.get 2 0).char ≡ '中'
  (buf.get 3 0).isPlaceholder ≡ true

test "Buffer.writeLink writes hyperlinked text" := do
  let buf := Buffer.new 20 3
  let buf := buf.writeLink 0 0 "Click" "https://x.com"
  (buf.get 0 0).char ≡ 'C'
  (buf.get 0 0).hyperlink ≡ some "https://x.com"
  (buf.get 4 0).char ≡ 'k'
  (buf.get 4 0).hyperlink ≡ some "https://x.com"

test "Buffer.clear fills with empty cells" := do
  let buf := Buffer.new 5 5
  let buf := buf.set 2 2 (Cell.new 'X')
  let buf := buf.clear
  (buf.get 2 2).char ≡ ' '

test "Buffer.diff returns empty for identical buffers" := do
  let buf := Buffer.new 5 5
  let changes := Buffer.diff buf buf
  changes.length ≡ 0

test "Buffer.diff detects changes" := do
  let old := Buffer.new 5 5
  let new_ := old.set 1 1 (Cell.new 'X')
  let changes := Buffer.diff old new_
  changes.length ≡ 1

test "Buffer.diff marks newly expanded area as changes" := do
  let old := Buffer.new 2 1
  let new_ := Buffer.new 3 1
  let changes := Buffer.diff old new_
  match changes with
  | [(x, y, cell)] =>
    x ≡ 2
    y ≡ 0
    cell.char ≡ ' '
  | _ =>
    ensure false s!"expected a single change for the new column, got {changes.length}"

test "Buffer.diff marks removed area as changes when buffer shrinks" := do
  let bgStyle : Style := { bg := .ansi .blue }
  let old := (Buffer.new 3 2).fill (Cell.styled ' ' bgStyle)
  let new_ := Buffer.new 2 1
  let changes := Buffer.diff old new_
  let hasOutside := changes.any (fun (x, y, _) =>
    x >= new_.width || y >= new_.height)
  ensure hasOutside "expected diff to include cells outside the new buffer bounds"
  changes.length ≡ old.area



end TerminusTests.BufferTests
