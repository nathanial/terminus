-- Terminus.Backend.TerminalIO: Real FFI-based implementation of TerminalEffect

import Terminus.Backend.TerminalEffect
import Terminus.Backend.Raw
import Raster

namespace Terminus

/-- Real implementation of TerminalEffect using FFI -/
instance : TerminalEffect IO where
  enableRawMode := Terminus.enableRawMode
  disableRawMode := Terminus.disableRawMode
  getTerminalSize := Terminus.getTerminalSize
  readByte := Terminus.readByte
  readByteBlocking := Terminus.readByteBlocking
  unreadByte := Terminus.unreadByte
  writeStdout := Terminus.writeStdout
  flushStdout := Terminus.flushStdout
  readFileBytes path := do
    try
      IO.FS.readBinFile path
    catch _ =>
      pure ByteArray.empty
  decodeImageBytes buffer := do
    try
      -- Use raster to decode image bytes, forcing RGB format
      let img ← Raster.Image.loadFromMemoryAs buffer .rgb
      pure (some (img.width, img.height, img.data))
    catch _ =>
      pure none

end Terminus
