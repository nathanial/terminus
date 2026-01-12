/-
  Terminus Reactive - Image Widget
  Display images using terminal image protocols (iTerm2, Sixel).
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Image Configuration -/

/-- Configuration for image widget -/
structure ImageConfig where
  /-- Width in terminal cells -/
  width : Nat := 40
  /-- Height in terminal cells -/
  height : Nat := 20
  /-- Image protocol to use -/
  protocol : ImageProtocol := .iterm2
  /-- Preserve aspect ratio when scaling -/
  preserveAspect : Bool := true
  /-- Alternative text to show if image cannot be displayed -/
  altText : String := "Image"
  deriving Repr, Inhabited

/-! ## Image Widget -/

/-- Create an image widget from a file path.

    Example:
    ```
    image' "/path/to/image.png" { width := 60, height := 30 }
    ```
-/
def image' (path : System.FilePath) (config : ImageConfig := {}) : WidgetM Unit := do
  emitStatic (RNode.image (.path path) config.protocol config.width config.height
    config.preserveAspect config.altText)

/-- Create an image widget from raw bytes.

    Example:
    ```
    let imageBytes ← IO.FS.readBinFile "/path/to/image.png"
    imageFromBytes' imageBytes { width := 60, height := 30, altText := "My Image" }
    ```
-/
def imageFromBytes' (bytes : ByteArray) (config : ImageConfig := {}) : WidgetM Unit := do
  emitStatic (RNode.image (.bytes bytes) config.protocol config.width config.height
    config.preserveAspect config.altText)

/-- Create a dynamic image widget from a file path dynamic.

    Example:
    ```
    let pathDyn ← somePathSource
    dynImage' pathDyn { width := 60, height := 30 }
    ```
-/
def dynImage' (path : Reactive.Dynamic Spider System.FilePath) (config : ImageConfig := {})
    : WidgetM Unit := do
  let node ← path.map' fun p =>
    RNode.image (.path p) config.protocol config.width config.height
      config.preserveAspect config.altText
  emit node

/-- Create a dynamic image widget from raw bytes dynamic.

    Example:
    ```
    let bytesDyn ← someImageSource
    dynImageFromBytes' bytesDyn { width := 60, height := 30 }
    ```
-/
def dynImageFromBytes' (bytes : Reactive.Dynamic Spider ByteArray) (config : ImageConfig := {})
    : WidgetM Unit := do
  let node ← bytes.map' fun b =>
    RNode.image (.bytes b) config.protocol config.width config.height
      config.preserveAspect config.altText
  emit node

/-- Create an image placeholder (for when no image is available).
    Shows the alt text in a bordered area.

    Example:
    ```
    imagePlaceholder' "Loading..." { width := 40, height := 20 }
    ```
-/
def imagePlaceholder' (altText : String) (_config : ImageConfig := {}) : WidgetM Unit := do
  let text := s!"[{altText}]"
  emitStatic (RNode.text text Style.dim)

end Terminus.Reactive
