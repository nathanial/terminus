-- TerminusTests.MenuTests: Tests for Menu widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Menu

namespace TerminusTests.MenuTests

open Terminus
open Crucible

testSuite "Menu Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "MenuItem.new creates item with label" := do
  let item := MenuItem.new "File"
  item.label ≡ "File"
  item.enabled ≡ true

test "MenuItem.separator creates separator" := do
  let item := MenuItem.separator
  item.isSeparator ≡ true

test "MenuItem.disabled creates disabled item" := do
  let item := MenuItem.disabled "Unavailable"
  item.label ≡ "Unavailable"
  item.enabled ≡ false

test "MenuItem.withHotkey sets hotkey" := do
  let item := MenuItem.new "Save" |>.withHotkey "Ctrl+S"
  item.hotkey ≡ some "Ctrl+S"

test "MenuItem.hasSubmenu detects submenu" := do
  let item := MenuItem.new "Options" |>.withSubmenu [MenuItem.new "Sub1"]
  item.hasSubmenu ≡ true

test "Menu.new creates menu from items" := do
  let menu := Menu.new [MenuItem.new "A", MenuItem.new "B"]
  menu.items.length ≡ 2

test "Menu.withSelected sets selection" := do
  let menu := Menu.new [MenuItem.new "A", MenuItem.new "B"] |>.withSelected 1
  menu.selectedPath ≡ [1]

test "Menu renders without crash" := do
  let menu := Menu.new [MenuItem.new "File", MenuItem.new "Edit", MenuItem.separator, MenuItem.new "Exit"]
    |>.withSelected 0
  let buf := renderWidget menu 20 10
  buf.width ≡ 20

#generate_tests

end TerminusTests.MenuTests
