-- Terminus.Widgets.Image: Render real images using terminal image protocols

import Terminus.Core.Cell
import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Frame

namespace Terminus

/-- Image widget. Uses terminal commands to display real images (not braille/halfblocks). -/
structure Image where
  source : ImageSource
  protocol : ImageProtocol := .iterm2
  preserveAspectRatio : Bool := true
  name : Option String := none
  background : Style := {}
  block : Option Block := none
  altText : String := "Image"
  deriving Inhabited

namespace Image

def fromBytes (bytes : ByteArray) : Image :=
  { source := .bytes bytes }

def fromPath (path : System.FilePath) : Image :=
  { source := .path path }

def withProtocol (img : Image) (p : ImageProtocol) : Image := { img with protocol := p }
def withName (img : Image) (name : Option String) : Image := { img with name := name }
def withPreserveAspectRatio (img : Image) (b : Bool) : Image := { img with preserveAspectRatio := b }
def withBackground (img : Image) (s : Style) : Image := { img with background := s }
def withAltText (img : Image) (s : String) : Image := { img with altText := s }
def withBlock (img : Image) (b : Block) : Image := { img with block := some b }

private def fillArea (buf : Buffer) (r : Rect) (style : Style) : Buffer :=
  buf.fillRect r (Cell.styled ' ' style)

end Image

instance : Frame.FrameWidget Image where
  render img area frame := Id.run do
    let mut f := frame
    let mut contentArea := area

    -- Render block first, if present.
    match img.block with
    | none => pure ()
    | some b =>
      f := f.render b area
      contentArea := b.innerArea area

    if contentArea.isEmpty then
      return f

    -- Clear background under image (also ensures deterministic "erase" behavior).
    f := { f with buffer := Image.fillArea f.buffer contentArea img.background }

    -- Emit terminal command to display the image at this rectangle.
    let cmd : ImageCommand := {
      rect := contentArea
      source := img.source
      protocol := img.protocol
      preserveAspectRatio := img.preserveAspectRatio
      name := img.name
    }
    f := f.addCommand (.image cmd)
    f

/-- Fallback text-only rendering (does not display real images). -/
instance : Widget Image where
  render img area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner img.block area buf
    if contentArea.isEmpty then return buf'
    let mut result := buf'

    result := Image.fillArea result contentArea img.background

    let label := s!"{img.altText} (use iTerm2/WezTerm)"
    let x := contentArea.x + 1
    let y := contentArea.y + (contentArea.height / 2)
    result.writeStringBounded x y (if contentArea.width > 2 then contentArea.width - 2 else 0) label Style.dim

end Terminus
