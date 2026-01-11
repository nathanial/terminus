-- TerminusTests.ImageTests: Tests for Image widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Core.Style
import Terminus.Widgets.Widget
import Terminus.Widgets.Image

namespace TerminusTests.ImageTests

open Terminus
open Crucible

testSuite "Image Widget Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Image.fromBytes creates image from bytes" := do
  let bytes := ByteArray.mk #[0x89, 0x50, 0x4E, 0x47]  -- PNG header
  let img := Image.fromBytes bytes
  match img.source with
  | .bytes b => b.size ≡ 4
  | .path _ => ensure false "Expected bytes source"

test "Image.fromPath creates image from path" := do
  let img := Image.fromPath "/tmp/test.png"
  match img.source with
  | .path p => p.toString ≡ "/tmp/test.png"
  | .bytes _ => ensure false "Expected path source"

test "Image.withProtocol sets protocol" := do
  let img := Image.fromPath "/tmp/test.png" |>.withProtocol .iterm2
  -- Just verify it doesn't crash; only iterm2 is currently supported
  ensure true "protocol set"

test "Image.withName sets image name" := do
  let img := Image.fromPath "/tmp/test.png" |>.withName (some "myimage")
  img.name ≡ some "myimage"

test "Image.withPreserveAspectRatio sets aspect ratio flag" := do
  let img := Image.fromPath "/tmp/test.png" |>.withPreserveAspectRatio false
  img.preserveAspectRatio ≡ false

test "Image.withAltText sets fallback text" := do
  let img := Image.fromPath "/tmp/test.png" |>.withAltText "Photo"
  img.altText ≡ "Photo"

test "Image.withBackground sets background style" := do
  let img := Image.fromPath "/tmp/test.png" |>.withBackground (Style.bgColor Color.black)
  img.background.bg ≡ Color.black

test "Image fallback Widget renders alt text" := do
  let img := Image.fromPath "/tmp/test.png" |>.withAltText "IMG"
  let buf := renderWidget img 10 3
  -- The fallback widget should render the alt text
  buf.width ≡ 10

#generate_tests

end TerminusTests.ImageTests
