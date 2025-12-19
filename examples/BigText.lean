-- BigText: Demonstrate large pixel fonts

import Terminus

open Terminus

private def renderDemo (term : Terminal) : Frame := Id.run do
  let area := term.area
  let mut frame := Frame.new area

  let sections := (Layout.vertical [.fixed 10, .fixed 10, .fixed 6, .fill])
    |>.withSpacing 1
    |>.split area

  if h : 0 < sections.length then
    let block := Block.rounded
      |>.withTitle "Block Font"
      |>.withTitleStyle (Style.bold.withFg Color.cyan)
      |>.withBorderStyle (Style.fgColor Color.blue)
    let big := BigText.new "TERMINUS"
      |>.withFont BigFont.block
      |>.withStyle (Style.bold.withFg Color.cyan)
      |>.withSpacing 0
      |>.withBlock block
    frame := frame.render big sections[0]

  if h : 1 < sections.length then
    let block := Block.rounded
      |>.withTitle "Slant Font"
      |>.withTitleStyle (Style.bold.withFg Color.magenta)
      |>.withBorderStyle (Style.fgColor Color.magenta)
    let big := BigText.new "BIG TEXT"
      |>.withFont BigFont.slant
      |>.withStyle (Style.fgColor Color.magenta)
      |>.withSpacing 0
      |>.withBlock block
    frame := frame.render big sections[1]

  if h : 2 < sections.length then
    let block := Block.rounded
      |>.withTitle "Small Font"
      |>.withTitleStyle (Style.bold.withFg Color.green)
      |>.withBorderStyle (Style.fgColor Color.green)
    let big := BigText.new "lean 4"
      |>.withFont BigFont.small
      |>.withStyle (Style.fgColor Color.green)
      |>.withSpacing 0
      |>.withBlock block
    frame := frame.render big sections[2]

  let footer := "Press any key to exit"
  let footerX := if area.width > footer.length then (area.width - footer.length) / 2 else 0
  let footerY := area.y + area.height - 1
  frame := frame.writeString footerX footerY footer Style.dim

  frame

def main : IO Unit := do
  Terminal.setup
  try
    let term ← Terminal.new
    let frame := renderDemo term
    let term := term.setBuffer frame.buffer
    let _ ← term.draw
    let _ ← Events.read
  finally
    Terminal.teardown
