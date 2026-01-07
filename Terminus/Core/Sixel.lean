-- Terminus.Core.Sixel: Sixel graphics protocol encoder
-- Sixel encodes images as vertical 6-pixel strips for terminal display.
-- Supported by xterm, mlterm, foot, and other terminals.

namespace Terminus

namespace Sixel

/-- RGB color (8-bit per channel) -/
structure RGB where
  r : UInt8
  g : UInt8
  b : UInt8
  deriving BEq, Repr, Inhabited

/-- A 256-color palette -/
structure Palette where
  colors : Array RGB
  deriving Repr, Inhabited

/-- Raw image data for Sixel encoding -/
structure RawImage where
  width : Nat
  height : Nat
  pixels : Array RGB    -- row-major, length = width * height
  deriving Repr

/-- Quantized/indexed image ready for Sixel encoding -/
structure IndexedImage where
  width : Nat
  height : Nat
  palette : Palette
  indices : Array UInt8   -- index into palette for each pixel
  deriving Repr

/-- Sixel encoder configuration -/
structure SixelConfig where
  maxColors : Nat := 216    -- Maximum palette size (default: 6x6x6 cube)
  deriving Repr, Inhabited

namespace RGB

def black : RGB := { r := 0, g := 0, b := 0 }
def white : RGB := { r := 255, g := 255, b := 255 }

/-- Squared Euclidean distance between two colors -/
def distanceSquared (a b : RGB) : Nat :=
  let dr := (a.r.toNat : Int) - (b.r.toNat : Int)
  let dg := (a.g.toNat : Int) - (b.g.toNat : Int)
  let db := (a.b.toNat : Int) - (b.b.toNat : Int)
  (dr * dr + dg * dg + db * db).toNat

end RGB

/-- Create a uniform 6x6x6 color cube palette (216 colors) -/
def uniformPalette : Palette := Id.run do
  let mut colors : Array RGB := Array.mkEmpty 216
  for r in [0:6] do
    for g in [0:6] do
      for b in [0:6] do
        let r8 := (r * 255 / 5).toUInt8
        let g8 := (g * 255 / 5).toUInt8
        let b8 := (b * 255 / 5).toUInt8
        colors := colors.push { r := r8, g := g8, b := b8 }
  { colors }

/-- Find the nearest palette index for a color using Euclidean distance -/
def nearestPaletteIndex (color : RGB) (palette : Palette) : UInt8 := Id.run do
  let mut bestIdx : Nat := 0
  let mut bestDist : Nat := RGB.distanceSquared color (palette.colors.getD 0 RGB.black)
  for i in [1:palette.colors.size] do
    let dist := RGB.distanceSquared color (palette.colors.getD i RGB.black)
    if dist < bestDist then
      bestIdx := i
      bestDist := dist
  bestIdx.toUInt8

/-- Quantize a raw image to an indexed image using the given palette -/
def quantizeImage (img : RawImage) (palette : Palette) : IndexedImage := Id.run do
  let mut indices : Array UInt8 := Array.mkEmpty img.pixels.size
  for pixel in img.pixels do
    indices := indices.push (nearestPaletteIndex pixel palette)
  { width := img.width, height := img.height, palette, indices }

/-- Create RawImage from RGBA byte array (ignores alpha) -/
def RawImage.fromRGBA (width height : Nat) (data : ByteArray) : Option RawImage :=
  if data.size < width * height * 4 then none
  else
    let pixels := Id.run do
      let mut arr : Array RGB := Array.mkEmpty (width * height)
      for i in [:width * height] do
        let r := data.get! (i * 4)
        let g := data.get! (i * 4 + 1)
        let b := data.get! (i * 4 + 2)
        arr := arr.push { r, g, b }
      arr
    some { width, height, pixels }

/-- Create RawImage from RGB byte array (no alpha) -/
def RawImage.fromRGB (width height : Nat) (data : ByteArray) : Option RawImage :=
  if data.size < width * height * 3 then none
  else
    let pixels := Id.run do
      let mut arr : Array RGB := Array.mkEmpty (width * height)
      for i in [:width * height] do
        let r := data.get! (i * 3)
        let g := data.get! (i * 3 + 1)
        let b := data.get! (i * 3 + 2)
        arr := arr.push { r, g, b }
      arr
    some { width, height, pixels }

/-- Encode palette as Sixel color definitions -/
def encodePalette (palette : Palette) : String := Id.run do
  let mut result : Array Char := Array.mkEmpty (palette.colors.size * 20)
  for i in [:palette.colors.size] do
    let c := palette.colors.getD i RGB.black
    -- Convert 0-255 to 0-100 percentage
    let r100 := (c.r.toNat * 100 + 127) / 255
    let g100 := (c.g.toNat * 100 + 127) / 255
    let b100 := (c.b.toNat * 100 + 127) / 255
    -- Format: #<idx>;2;<r>;<g>;<b>
    let entry := s!"#{i};2;{r100};{g100};{b100}"
    for c in entry.toList do
      result := result.push c
  String.ofList result.toList

/-- Emit RLE-encoded run -/
private def emitRun (chars : Array Char) (c : Char) (count : Nat) : Array Char :=
  if count == 0 then chars
  else if count <= 3 then
    -- For short runs, just repeat the character
    let repeated := List.replicate count c
    chars ++ repeated.toArray
  else
    -- Use RLE: !<count><char>
    let countStr := toString count
    let rleChars := ['!'] ++ countStr.toList ++ [c]
    chars ++ rleChars.toArray

/-- Encode a single sixel band (6 rows) for one color -/
private def encodeBandForColor (img : IndexedImage) (colorIndex : UInt8)
    (bandY : Nat) : Array Char := Id.run do
  let mut result : Array Char := #[]

  -- Check if this color is used in this band at all
  let bandStartY := bandY * 6
  let mut colorUsed := false
  for dy in [:6] do
    let y := bandStartY + dy
    if y < img.height then
      for x in [:img.width] do
        let idx := y * img.width + x
        if h : idx < img.indices.size then
          if img.indices[idx] == colorIndex then
            colorUsed := true
            break
    if colorUsed then break

  if !colorUsed then return #[]

  -- Select color: #<n>
  let colorSel := s!"#{colorIndex.toNat}"
  for c in colorSel.toList do
    result := result.push c

  -- Encode each column
  let mut runChar : Char := '?'
  let mut runLength : Nat := 0

  for x in [:img.width] do
    -- Compute 6-pixel bit pattern for this column
    let mut pattern : UInt8 := 0
    for dy in [:6] do
      let y := bandStartY + dy
      if y < img.height then
        let idx := y * img.width + x
        if h : idx < img.indices.size then
          if img.indices[idx] == colorIndex then
            pattern := pattern ||| (1 <<< dy.toUInt8)

    let char := Char.ofNat (pattern.toNat + 63)

    -- RLE encoding
    if runLength == 0 then
      runChar := char
      runLength := 1
    else if runChar == char then
      runLength := runLength + 1
    else
      result := emitRun result runChar runLength
      runChar := char
      runLength := 1

  -- Emit final run
  result := emitRun result runChar runLength

  -- Graphics carriage return
  result := result.push '$'
  result

/-- Encode full raster data for an indexed image -/
def encodeRaster (img : IndexedImage) : String := Id.run do
  let numBands := (img.height + 5) / 6
  let mut result : Array Char := Array.mkEmpty (img.width * numBands * 10)

  for bandY in [:numBands] do
    -- For each color, emit its pattern for this band
    for colorIdx in [:img.palette.colors.size] do
      let bandData := encodeBandForColor img colorIdx.toUInt8 bandY
      for c in bandData do
        result := result.push c
    -- Graphics newline (move down 6 pixels)
    result := result.push '-'

  String.ofList result.toList

/-- Generate complete Sixel escape sequence -/
def toEscapeSequence (img : IndexedImage) : String :=
  let dcs := "\x1bP"      -- DCS (Device Control String)
  let st := "\x1b\\"      -- ST (String Terminator)
  let params := "0;1"     -- Aspect ratio params (1:1)
  let palette := encodePalette img.palette
  let raster := encodeRaster img
  s!"{dcs}{params}q{palette}{raster}{st}"

/-- Convenience: encode raw image directly to Sixel escape sequence -/
def encodeRaw (img : RawImage) : String :=
  let palette := uniformPalette
  let indexed := quantizeImage img palette
  toEscapeSequence indexed

end Sixel

end Terminus
