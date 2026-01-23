-- TerminusTests.RectTests: Tests for Rect operations

import Crucible
import Terminus.Core.Rect

namespace TerminusTests.RectTests

open Terminus
open Crucible

testSuite "Rect Tests"

test "Rect.isEmpty returns true for zero dimensions" := do
  let r : Rect := { x := 0, y := 0, width := 0, height := 5 }
  r.isEmpty ≡ true

test "Rect.isEmpty returns false for valid rect" := do
  let r : Rect := { x := 0, y := 0, width := 10, height := 5 }
  r.isEmpty ≡ false

test "Rect.area calculates correctly" := do
  let r : Rect := { x := 0, y := 0, width := 4, height := 3 }
  r.area ≡ 12



end TerminusTests.RectTests
