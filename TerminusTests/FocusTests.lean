-- TerminusTests.FocusTests: Tests for Focus Management

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Input.Key
import Terminus.Widgets.Widget
import Terminus.Widgets.TextInput
import Terminus.Widgets.Form
import Terminus.Core.Focus

namespace TerminusTests.FocusTests

open Terminus
open Crucible

testSuite "Focus Management Tests"

test "FocusState.normalize selects first focusable widget" := do
  let items : Array AnyWidget := #[
    AnyWidget.of (Text.new "A"),
    AnyWidget.of TextInput.new,
    AnyWidget.of (Text.new "B")
  ]
  let state : FocusState := {}
  let normalized := FocusState.normalize state items
  normalized.focused ≡ some 1

test "FocusState.handleKey tab skips non-focusable widgets" := do
  let items : Array AnyWidget := #[
    AnyWidget.of TextInput.new,
    AnyWidget.of (Text.new "skip"),
    AnyWidget.of TextInput.new
  ]
  let state : FocusState := { focused := some 0 }
  let next := FocusState.handleKey state items KeyEvent.tab
  next.focused ≡ some 2

test "FocusState.handleKey shift+tab wraps to last focusable widget" := do
  let items : Array AnyWidget := #[
    AnyWidget.of TextInput.new,
    AnyWidget.of (Text.new "skip"),
    AnyWidget.of TextInput.new
  ]
  let state : FocusState := { focused := some 0 }
  let prev := FocusState.handleKey state items (KeyEvent.tab.withShift)
  prev.focused ≡ some 2

test "FocusState.apply updates focus flags" := do
  let items : Array TextInput := #[
    TextInput.new.blur,
    TextInput.new.blur
  ]
  let state : FocusState := { focused := some 1 }
  let applied := FocusState.apply state items
  (applied.getD 0 TextInput.new).focused ≡ false
  (applied.getD 1 TextInput.new).focused ≡ true

test "FocusState.update routes events to focused widget" := do
  let items : Array TextInput := #[TextInput.new.blur]
  let state : FocusState := { focused := some 0 }
  let (_, updated) := FocusState.update state items (.key (KeyEvent.char 'a'))
  let item := updated.getD 0 TextInput.new
  item.value ≡ "a"
  item.focused ≡ true

#generate_tests

end TerminusTests.FocusTests
