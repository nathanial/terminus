-- TerminusTests.CheckboxTests: Tests for Checkbox widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Checkbox

namespace TerminusTests.CheckboxTests

open Terminus
open Crucible

testSuite "Checkbox Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Checkbox.new creates checkbox with label" := do
  let cb := Checkbox.new "Option"
  cb.label ≡ "Option"
  cb.checked ≡ false

test "Checkbox.toggle flips checked state" := do
  let cb := Checkbox.new "Test" |>.toggle
  cb.checked ≡ true
  let cb2 := cb.toggle
  cb2.checked ≡ false

test "Checkbox.withChecked sets checked state" := do
  let cb := Checkbox.new "Test" |>.withChecked true
  cb.checked ≡ true

test "Checkbox checked renders checkedSymbol" := do
  let cb := Checkbox.new "Test" |>.withChecked true
  let buf := renderWidget cb 20 1
  -- Default checked symbol is "[x]"
  (buf.get 0 0).char ≡ '['
  (buf.get 1 0).char ≡ 'x'
  (buf.get 2 0).char ≡ ']'

test "Checkbox unchecked renders uncheckedSymbol" := do
  let cb := Checkbox.new "Test"
  let buf := renderWidget cb 20 1
  -- Default unchecked symbol is "[ ]"
  (buf.get 0 0).char ≡ '['
  (buf.get 1 0).char ≡ ' '
  (buf.get 2 0).char ≡ ']'

test "Checkbox.withSymbols sets custom symbols" := do
  let cb := Checkbox.new "Test" |>.withSymbols "✓" "○"
  cb.checkedSymbol ≡ "✓"
  cb.uncheckedSymbol ≡ "○"

#generate_tests

end TerminusTests.CheckboxTests
