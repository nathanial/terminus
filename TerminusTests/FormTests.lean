-- TerminusTests.FormTests: Tests for Form widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Paragraph
import Terminus.Widgets.TextInput
import Terminus.Widgets.Form

namespace TerminusTests.FormTests

open Terminus
open Crucible

testSuite "Form Widget Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Form.new creates empty form" := do
  let form := Form.new
  form.rows.length ≡ 0

test "Form.withLabelSuffix changes suffix" := do
  let form := Form.new |>.withLabelSuffix " ="
  form.labelSuffix ≡ " ="

test "Form.withLabelGap sets gap" := do
  let form := Form.new |>.withLabelGap 4
  form.labelGap ≡ 4

test "Form.withRowSpacing sets spacing" := do
  let form := Form.new |>.withRowSpacing 2
  form.rowSpacing ≡ 2

test "FormRow.gap creates spacer" := do
  let row := FormRow.gap 3
  row.height ≡ 3

test "FormRow.fieldOf creates field row" := do
  let row := FormRow.fieldOf "Name" TextInput.new
  row.height ≡ 1

test "FormRow.fullOf creates full-width row" := do
  let row := FormRow.fullOf (Paragraph.fromString "Full width content")
  row.height ≡ 1

test "Form renders without crash" := do
  let form := Form.new
    |>.withRows [
      FormRow.fieldOf "Username" TextInput.new,
      FormRow.gap 1,
      FormRow.fieldOf "Password" TextInput.new
    ]
  let buf := renderWidget form 40 5
  buf.width ≡ 40

test "AnyWidget.empty creates no-op widget" := do
  let w := AnyWidget.empty
  let buf := renderWidget w 5 3
  buf.width ≡ 5

#generate_tests

end TerminusTests.FormTests
