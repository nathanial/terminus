-- TerminusTests.PopupTests: Tests for Popup widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Popup

namespace TerminusTests.PopupTests

open Terminus
open Crucible

testSuite "Popup Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Popup.new creates popup from content" := do
  let popup := Popup.new "Hello\nWorld"
  popup.lines.length ≡ 2

test "Popup.fromLines creates popup from line list" := do
  let popup := Popup.fromLines ["Line 1", "Line 2", "Line 3"]
  popup.lines.length ≡ 3

test "Popup.withTitle sets title" := do
  let popup := Popup.new "Content" |>.withTitle "My Dialog"
  popup.title ≡ some "My Dialog"

test "Popup.withSize sets dimensions" := do
  let popup := Popup.new "Content" |>.withSize 40 20
  popup.width ≡ some 40
  popup.height ≡ some 20

test "Popup.computeSize calculates size from content" := do
  let popup := Popup.new "Short"
  let (w, h) := popup.computeSize 100 100
  -- Content + border + padding
  ensure (w >= 7) "width includes content"
  ensure (h >= 3) "height includes content"

test "Popup renders without crash" := do
  let popup := Popup.new "This is a popup message" |>.withTitle "Info"
  let buf := renderWidget popup 50 20
  buf.width ≡ 50

test "ConfirmPopup.new creates confirmation dialog" := do
  let popup := ConfirmPopup.new "Are you sure?"
  popup.message ≡ "Are you sure?"
  popup.selectedYes ≡ true

test "ConfirmPopup.toggle switches selection" := do
  let popup := ConfirmPopup.new "Confirm?" |>.toggle
  popup.selectedYes ≡ false

test "ConfirmPopup renders without crash" := do
  let popup := ConfirmPopup.new "Delete this file?"
  let buf := renderWidget popup 50 20
  buf.width ≡ 50

#generate_tests

end TerminusTests.PopupTests
