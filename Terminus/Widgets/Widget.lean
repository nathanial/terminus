-- Terminus.Widgets.Widget: Widget typeclass and utilities

import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Input.Key

namespace Terminus

universe u

/-- Widget typeclass - anything that can be rendered to a buffer. -/
class Widget (α : Type u) where
  render : α → Rect → Buffer → Buffer
  /-- Preferred size in cells (width, height), if known. -/
  preferredSize : α → Option (Nat × Nat) := fun _ => none
  /-- Handle an input event, returning updated widget state. -/
  handleEvent : α → Event → α := fun w _ => w
  /-- Whether this widget can receive focus. -/
  focusable : α → Bool := fun _ => false
  /-- Set focus state for widgets that track it. -/
  setFocused : α → Bool → α := fun w _ => w

namespace Widget

/-- Render a widget into a fresh buffer -/
def renderNew [Widget α] (w : α) (area : Rect) : Buffer :=
  Widget.render w area (Buffer.new area.width area.height)

/-- Render a widget if the area is not empty -/
def renderIfVisible [Widget α] (w : α) (area : Rect) (buf : Buffer) : Buffer :=
  if area.isEmpty then buf else Widget.render w area buf

/-- Get the preferred size of a widget, or a fallback default. -/
def preferredSizeOr [Widget α] (w : α) (fallback : Nat × Nat) : Nat × Nat :=
  (Widget.preferredSize w).getD fallback

/-- Handle an optional input event with a widget. -/
def handleEventOpt [Widget α] (w : α) (event : Option Event) : α :=
  match event with
  | some e => Widget.handleEvent w e
  | none => w

/-- Whether a widget can receive focus. -/
def isFocusable [Widget α] (w : α) : Bool :=
  Widget.focusable w

/-- Set focus state for a widget. -/
def setFocus [Widget α] (w : α) (focused : Bool) : α :=
  Widget.setFocused w focused

end Widget

/-- A simple text widget for quick rendering -/
structure Text where
  content : String
  style : Style := {}
  deriving Repr, Inhabited

namespace Text

def new (s : String) : Text := { content := s }
def styled (s : String) (st : Style) : Text := { content := s, style := st }

end Text

instance : Widget Text where
  render t area buf := buf.writeStringBounded area.x area.y area.width t.content t.style
  preferredSize t := some (t.content.length, 1)

/-- Empty widget that renders nothing -/
structure Empty where
  deriving Repr, Inhabited

instance : Widget Empty where
  render _ _ buf := buf
  preferredSize _ := some (0, 0)

end Terminus
