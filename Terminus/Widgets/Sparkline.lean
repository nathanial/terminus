-- Terminus.Widgets.Sparkline: Mini inline chart with vertical bars

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Unicode block characters for sparkline bars (8 levels) -/
private def sparklineChars : Array Char := #['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█']

/-- Sparkline widget - displays data as vertical bars -/
structure Sparkline where
  data : List Float := []
  max : Option Float := none  -- Auto-calculated if none
  style : Style := Style.default
  block : Option Block := none
  deriving Repr, Inhabited

namespace Sparkline

def new (data : List Float) : Sparkline := { data := data }

def fromInts (data : List Int) : Sparkline :=
  { data := data.map Float.ofInt }

def fromNats (data : List Nat) : Sparkline :=
  { data := data.map (fun n => Float.ofNat n) }

def withMax (s : Sparkline) (m : Float) : Sparkline := { s with max := some m }
def withStyle (s : Sparkline) (st : Style) : Sparkline := { s with style := st }
def withBlock (s : Sparkline) (b : Block) : Sparkline := { s with block := some b }

def setData (s : Sparkline) (data : List Float) : Sparkline := { s with data := data }
def appendData (s : Sparkline) (value : Float) : Sparkline :=
  { s with data := s.data ++ [value] }
def pushData (s : Sparkline) (value : Float) (maxLen : Nat) : Sparkline :=
  let newData := s.data ++ [value]
  { s with data := if newData.length > maxLen then newData.drop 1 else newData }

/-- Calculate the maximum value from data -/
def computeMax (s : Sparkline) : Float :=
  match s.max with
  | some m => m
  | none => s.data.foldl (fun a b => if a > b then a else b) 0.0

/-- Map a value to a bar character index (0-7) -/
private def valueToBarIndex (value : Float) (maxVal : Float) : Nat :=
  if maxVal <= 0.0 then 0
  else
    let normalized := value / maxVal
    let index := (normalized * 7.0).toUInt32.toNat
    Nat.min index 7

end Sparkline

instance : Widget Sparkline where
  render s area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner s.block area buf
    if contentArea.isEmpty || contentArea.height == 0 then return buf'
    let mut result := buf'

    -- Calculate max value
    let maxVal := s.computeMax

    -- Render each data point as a bar character
    let y := contentArea.y + contentArea.height / 2
    let dataToRender := s.data.take contentArea.width

    for i in [:dataToRender.length] do
      match dataToRender[i]? with
      | some value =>
        let x := contentArea.x + i
        if x < contentArea.x + contentArea.width then
          let barIdx := Sparkline.valueToBarIndex value maxVal
          let char := sparklineChars.getD barIdx ' '
          result := result.setStyled x y char s.style
      | none => pure ()

    result

end Terminus
