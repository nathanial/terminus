-- Terminus.Widgets.Clear: Widget that clears/fills an area with empty cells

import Terminus.Widgets.Widget

namespace Terminus

/-- Clear widget - fills an area with empty or styled cells.
    Useful for layered rendering to reset an area before redrawing. -/
structure Clear where
  style : Style := {}
  deriving Repr, Inhabited

namespace Clear

/-- Create a Clear widget with default (empty) style -/
def new : Clear := {}

/-- Create a Clear widget with a background style -/
def withStyle (c : Clear) (s : Style) : Clear := { c with style := s }

/-- Create a Clear widget with a background color -/
def withBg (c : Clear) (color : Color) : Clear := { c with style := { bg := color } }

end Clear

instance : Widget Clear where
  render c area buf :=
    let cell := if c.style == {} then Cell.empty else { char := ' ', style := c.style }
    buf.fillRect area cell

end Terminus
