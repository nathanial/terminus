-- Terminus.Widgets.Spinner: Animated loading indicator widget

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Spinner widget for animated loading indicators.
    The frameIndex should be updated from application state (e.g., state.tick). -/
structure Spinner where
  frames : Array String := #["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
  frameIndex : Nat := 0
  label : Option String := none
  style : Style := {}
  labelStyle : Style := {}
  block : Option Block := none
  deriving Repr, Inhabited

namespace Spinner

/-- Create a spinner with custom frames -/
def new (frames : Array String) : Spinner := { frames }

/-- Braille dots spinner (default) -/
def dots : Spinner := { frames := #["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"] }

/-- Line rotation spinner -/
def line : Spinner := { frames := #["─", "╲", "│", "╱"] }

/-- Arc rotation spinner -/
def arc : Spinner := { frames := #["◜", "◠", "◝", "◞", "◡", "◟"] }

/-- Arrow rotation spinner -/
def arrows : Spinner := { frames := #["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"] }

/-- Quarter block spinner -/
def blocks : Spinner := { frames := #["▖", "▘", "▝", "▗"] }

/-- Growing bar spinner -/
def growing : Spinner := { frames := #["▏", "▎", "▍", "▌", "▋", "▊", "▉", "█", "▉", "▊", "▋", "▌", "▍", "▎", "▏"] }

/-- Simple ASCII spinner -/
def ascii : Spinner := { frames := #["|", "/", "-", "\\"] }

/-- Set the current frame index (typically from state.tick) -/
def withFrame (s : Spinner) (idx : Nat) : Spinner :=
  { s with frameIndex := if s.frames.isEmpty then 0 else idx % s.frames.size }

/-- Set the spinner style (applied to the spinner character) -/
def withStyle (s : Spinner) (st : Style) : Spinner := { s with style := st }

/-- Set the label text -/
def withLabel (s : Spinner) (l : String) : Spinner := { s with label := some l }

/-- Set the label style -/
def withLabelStyle (s : Spinner) (st : Style) : Spinner := { s with labelStyle := st }

/-- Set the block container -/
def withBlock (s : Spinner) (b : Block) : Spinner := { s with block := some b }

/-- Get the current frame string -/
def currentFrame (s : Spinner) : String :=
  if s.frames.isEmpty then "?"
  else s.frames.getD (s.frameIndex % s.frames.size) "?"

end Spinner

instance : Widget Spinner where
  render s area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner s.block area buf
    if contentArea.isEmpty || contentArea.height == 0 then return buf'
    let mut result := buf'

    -- Get current frame
    let frame := s.currentFrame

    -- Render on the middle row
    let y := contentArea.y + contentArea.height / 2
    let mut x := contentArea.x

    -- Render spinner character
    for c in frame.toList do
      if x < contentArea.x + contentArea.width then
        result := result.setStyled x y c s.style
        x := x + 1

    -- Render label if present
    match s.label with
    | some label =>
      if x + 1 < contentArea.x + contentArea.width then
        x := x + 1  -- Space between spinner and label
        let remainingWidth := contentArea.x + contentArea.width - x
        result := result.writeStringBounded x y remainingWidth label s.labelStyle
    | none => pure ()

    result

end Terminus
