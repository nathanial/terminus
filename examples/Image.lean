-- Image: Demo of the Image widget (iTerm2/WezTerm inline images)
-- Usage: `TERMINUS_IMAGE_PATH=/path/to/image.png lake exe image`
-- p toggles aspect ratio, q/Esc quits

import Terminus

open Terminus

namespace ImageDemo

private def samplePng : ByteArray :=
  ByteArray.mk #[
    137, 80, 78, 71, 13, 10, 26, 10, 0, 0, 0, 13, 73, 72, 68, 82, 0, 0, 0, 1, 0, 0, 0, 1, 8, 4,
    0, 0, 0, 181, 28, 12, 2, 0, 0, 0, 11, 73, 68, 65, 84, 120, 218, 99, 252, 255, 31, 0, 3, 3, 2,
    0, 239, 1, 207, 107, 0, 0, 0, 0, 73, 69, 78, 68, 174, 66, 96, 130
  ]

structure State where
  image : Image
  preserve : Bool := true
  deriving Inhabited

def init (path? : Option String) : State :=
  let img :=
    match path? with
    | some p => Image.fromPath p
    | none =>
      Image.fromBytes samplePng
        |>.withName (some "sample.png")
  { image := img }

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame
  if area.isEmpty then return f

  let sections := vsplit area [.fixed 3, .fill, .fixed 1]

  if h : 0 < sections.length then
    let title := Block.single
      |>.withTitle "Image Demo"
      |>.withBorderStyle (Style.fgColor Color.blue)
    let help := Paragraph.fromLines [
      "Requires iTerm2 or WezTerm (iTerm2 inline images).",
      "p: toggle preserve aspect ratio | q/Esc: quit"
    ] |>.withStyle Style.dim
      |>.withBlock title
    f := f.render help sections[0]

  if h : 1 < sections.length then
    let imgBlock := Block.rounded
      |>.withTitle (if state.preserve then "Image (preserve AR)" else "Image (stretch)")
      |>.withBorderStyle (Style.fgColor Color.cyan)
    let img := state.image
      |>.withPreserveAspectRatio state.preserve
      |>.withBlock imgBlock
      |>.withBackground (Style.bgColor Color.black)
    f := f.render img sections[1]

  if h : 2 < sections.length then
    let status := sections[2]
    f := f.writeString (status.x + 1) status.y "Tip: set TERMINUS_IMAGE_PATH to render a real file" Style.dim

  f

def update (state : State) (event : Option Event) : State × Bool :=
  match event with
  | none => (state, false)
  | some (.key k) =>
    if k.isCtrlC || k.isCtrlQ then (state, true)
    else
      match k.code with
      | .escape => (state, true)
      | .char 'q' => (state, true)
      | .char 'p' => ({ state with preserve := !state.preserve }, false)
      | _ => (state, false)
  | _ => (state, false)

end ImageDemo

def main : IO Unit := do
  let path? ← IO.getEnv "TERMINUS_IMAGE_PATH"
  App.runApp (ImageDemo.init path?) ImageDemo.draw ImageDemo.update
