-- PieChart: Demonstrates PieChart widget (Braille + cell resolution)
-- Press q to quit

import Terminus

open Terminus

namespace PieChartDemo

def slices : List PieSlice := [
  (PieSlice.new "A" 40) |>.withStyle (Style.fgColor Color.green),
  (PieSlice.new "B" 35) |>.withStyle (Style.fgColor Color.cyan),
  (PieSlice.new "C" 25) |>.withStyle (Style.fgColor Color.magenta)
]

structure State where
  deriving Inhabited

def draw (frame : Frame) (_ : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let main := Block.double
    |>.withTitle "PieChart"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render main area

  let inner := main.innerArea area
  if inner.isEmpty then return f

  let panels := hsplit inner [.fill, .fill]

  if h : 0 < panels.length then
    let left := panels[0]
    let chart := PieChart.new slices
      |>.withResolution .braille
      |>.withDonutRatio 0.35
      |>.withBlock (Block.rounded.withTitle "Braille (Donut)")
    f := f.render chart left

  if h : 1 < panels.length then
    let right := panels[1]
    let chart := PieChart.new slices
      |>.withResolution .cell
      |>.withDonutRatio 0.0
      |>.withBlock (Block.rounded.withTitle "Cell (Pie)")
    f := f.render chart right

  let help := "q: Quit"
  if area.height > 0 then
    f := f.writeString (area.x + 2) (area.y + area.height - 1) help Style.dim

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    match k.code with
    | .char 'q' => (state, true)
    | _ =>
      if k.isCtrlC || k.isCtrlQ then (state, true)
      else (state, false)

end PieChartDemo

def main : IO Unit := do
  App.runApp ({} : PieChartDemo.State) PieChartDemo.draw PieChartDemo.update

