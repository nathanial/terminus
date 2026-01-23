-- TerminusTests.LayoutTests: Tests for Layout operations

import Crucible
import Terminus.Core.Rect
import Terminus.Layout.Constraint
import Terminus.Layout.Layout

namespace TerminusTests.LayoutTests

open Terminus
open Crucible

testSuite "Layout Tests"

test "hsplit divides area horizontally" := do
  let area : Rect := { x := 0, y := 0, width := 100, height := 10 }
  let rects := hsplit area [.percent 50, .fill]
  rects.length ≡ 2
  if h : 0 < rects.length then rects[0].width ≡ 50 else ensure false "expected rect"
  if h : 1 < rects.length then rects[1].width ≡ 50 else ensure false "expected rect"

test "vsplit divides area vertically" := do
  let area : Rect := { x := 0, y := 0, width := 10, height := 100 }
  let rects := vsplit area [.fixed 20, .fill]
  rects.length ≡ 2
  if h : 0 < rects.length then rects[0].height ≡ 20 else ensure false "expected rect"
  if h : 1 < rects.length then rects[1].height ≡ 80 else ensure false "expected rect"

test "Layout.fixed constraint uses exact size" := do
  let area : Rect := { x := 0, y := 0, width := 100, height := 10 }
  let rects := hsplit area [.fixed 30, .fill]
  if h : 0 < rects.length then rects[0].width ≡ 30 else ensure false "expected rect"

test "Layout.percent constraint uses percentage" := do
  let area : Rect := { x := 0, y := 0, width := 200, height := 10 }
  let rects := hsplit area [.percent 25, .fill]
  if h : 0 < rects.length then rects[0].width ≡ 50 else ensure false "expected rect"



end TerminusTests.LayoutTests
