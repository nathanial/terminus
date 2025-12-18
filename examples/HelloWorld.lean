-- HelloWorld: Minimal Terminus example
-- Displays a centered greeting with a styled border

import Terminus

open Terminus

def main : IO Unit := do
  Terminal.setup
  try
    let term ← Terminal.new
    let area := term.area

    -- Create a centered block with the greeting
    let blockWidth := 40
    let blockHeight := 5
    let blockX := (area.width - blockWidth) / 2
    let blockY := (area.height - blockHeight) / 2

    let block := Block.rounded
      |>.withTitle "Terminus"
      |>.withTitleStyle (Style.bold.withFg Color.cyan)
      |>.withBorderStyle (Style.fgColor Color.blue)

    let greeting := Paragraph.fromLines [
      "",
      "Hello, Terminus!",
      ""
    ] |>.centered |>.withStyle (Style.bold.withFg Color.green)
      |>.withBlock block

    -- Create frame and render
    let frame := Frame.new area
    let blockArea : Rect := { x := blockX, y := blockY, width := blockWidth, height := blockHeight }
    let frame := frame.render greeting blockArea

    -- Also show instructions at the bottom
    let instructions := "Press any key to exit..."
    let instrX := (area.width - instructions.length) / 2
    let instrY := area.height - 2
    let frame := frame.writeString instrX instrY instructions Style.dim

    -- Update terminal and draw
    let term := term.setBuffer frame.buffer
    let _ ← term.draw

    -- Wait for any key
    let _ ← Events.read

  finally
    Terminal.teardown

  IO.println "Thanks for using Terminus!"
