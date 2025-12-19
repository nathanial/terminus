-- Counter: Interactive counter example with Gauge widget
-- Use +/- or Up/Down arrows to change the count, 'q' to quit

import Terminus

open Terminus

structure CounterState where
  count : Int := 0
  maxCount : Int := 100
  deriving Inhabited

def draw (frame : Frame) (state : CounterState) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  -- Create main block
  let mainBlock := Block.double
    |>.withTitle "Counter Demo"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)

  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  -- Split into sections
  let sections := vsplit inner [.fixed 3, .fixed 3, .fixed 1, .fill]

  -- Counter display
  if h : 0 < sections.length then
    let counterArea := sections[0]
    let counterText := s!"Count: {state.count}"
    let counterStyle := if state.count >= 0 then Style.bold.withFg Color.green
                        else Style.bold.withFg Color.red
    let counterBlock := Block.single.withTitle "Value"
    let counter := Paragraph.fromString counterText
      |>.centered
      |>.withStyle counterStyle
      |>.withBlock counterBlock
    f := f.render counter counterArea

  -- Gauge
  if h : 1 < sections.length then
    let gaugeArea := sections[1]
    let ratio := if state.maxCount == 0 then 0.0
                 else Float.ofInt (max 0 state.count) / Float.ofInt state.maxCount
    let gauge := Gauge.new ratio
      |>.withLabel "Progress"
      |>.withFilledStyle (Style.fgColor Color.green)
      |>.withUnfilledStyle (Style.fgColor Color.white)
      |>.withBlock (Block.single.withTitle "Gauge")
    f := f.render gauge gaugeArea

  -- Instructions
  if h : 3 < sections.length then
    let instrArea := sections[3]
    let instructions := Paragraph.fromLines [
      "Controls:",
      "  Up/+ : Increase count",
      "  Down/- : Decrease count",
      "  r   : Reset to 0",
      "  q   : Quit"
    ] |>.withStyle Style.dim
      |>.withBlock (Block.rounded.withTitle "Help")
    f := f.render instructions instrArea

  f

def update (state : CounterState) (key : Option KeyEvent) : CounterState × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    match k.code with
    | .char 'q' => (state, true)  -- Quit
    | .char '+' | .up => ({ state with count := min (state.count + 1) state.maxCount }, false)
    | .char '-' | .down => ({ state with count := state.count - 1 }, false)
    | .char 'r' => ({ state with count := 0 }, false)
    | _ =>
      if k.isCtrlC || k.isCtrlQ then (state, true)
      else (state, false)

def main : IO Unit := do
  let initialState : CounterState := { count := 50 }
  App.runApp initialState draw update
