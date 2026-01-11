-- TerminusTests.RadioGroupTests: Tests for RadioGroup widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Checkbox

namespace TerminusTests.RadioGroupTests

open Terminus
open Crucible

testSuite "RadioGroup Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "RadioGroup.new creates from labels" := do
  let rg := RadioGroup.new ["A", "B", "C"]
  rg.options.length ≡ 3

test "RadioGroup.withSelected sets selection" := do
  let rg := RadioGroup.new ["A", "B", "C"] |>.withSelected 1
  rg.selected ≡ some 1

test "RadioGroup.withSelected ignores out of bounds" := do
  let rg := RadioGroup.new ["A", "B"] |>.withSelected 10
  rg.selected ≡ none

test "RadioGroup.selectNext advances selection" := do
  let rg := RadioGroup.new ["A", "B", "C"] |>.withSelected 0 |>.selectNext
  rg.selected ≡ some 1

test "RadioGroup.selectPrev moves selection back" := do
  let rg := RadioGroup.new ["A", "B", "C"] |>.withSelected 2 |>.selectPrev
  rg.selected ≡ some 1

test "RadioGroup.getSelected returns selected option" := do
  let rg := RadioGroup.new ["First", "Second", "Third"] |>.withSelected 1
  match rg.getSelected with
  | some opt => opt.label ≡ "Second"
  | none => ensure false "expected selection"

test "RadioGroup.clearSelected clears selection" := do
  let rg := RadioGroup.new ["A", "B"] |>.withSelected 0 |>.clearSelected
  rg.selected ≡ none

test "RadioGroup renders without crash" := do
  let rg := RadioGroup.new ["Option A", "Option B"] |>.withSelected 0
  let buf := renderWidget rg 20 5
  buf.width ≡ 20

#generate_tests

end TerminusTests.RadioGroupTests
