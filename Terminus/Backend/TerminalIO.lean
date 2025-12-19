-- Terminus.Backend.TerminalIO: Real FFI-based implementation of TerminalEffect

import Terminus.Backend.TerminalEffect
import Terminus.Backend.Raw

namespace Terminus

/-- Real implementation of TerminalEffect using FFI -/
instance : TerminalEffect IO where
  enableRawMode := Terminus.enableRawMode
  disableRawMode := Terminus.disableRawMode
  getTerminalSize := Terminus.getTerminalSize
  readByte := Terminus.readByte
  writeStdout := Terminus.writeStdout
  flushStdout := Terminus.flushStdout
  readFileBytes path := do
    try
      IO.FS.readBinFile path
    catch _ =>
      pure ByteArray.empty

end Terminus
