-- Terminus.Widgets.Widget: Widget typeclass and utilities

import Terminus.Core.Buffer
import Terminus.Core.Rect

namespace Terminus

/-- Widget typeclass - anything that can be rendered to a buffer -/
class Widget (α : Type) where
  render : α → Rect → Buffer → Buffer

namespace Widget

/-- Render a widget into a fresh buffer -/
def renderNew [Widget α] (w : α) (area : Rect) : Buffer :=
  Widget.render w area (Buffer.new area.width area.height)

/-- Render a widget if the area is not empty -/
def renderIfVisible [Widget α] (w : α) (area : Rect) (buf : Buffer) : Buffer :=
  if area.isEmpty then buf else Widget.render w area buf

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

/-- Empty widget that renders nothing -/
structure Empty where
  deriving Repr, Inhabited

instance : Widget Empty where
  render _ _ buf := buf

end Terminus
