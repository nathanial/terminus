-- Terminus.Backend.TerminalEffect: Effect abstraction for terminal operations

namespace Terminus

/-- Type class abstracting terminal I/O operations.
    This enables testing terminal logic without FFI by providing mock implementations. -/
class TerminalEffect (m : Type → Type) where
  /-- Enable raw terminal mode (no echo, character-by-character input) -/
  enableRawMode : m Unit

  /-- Disable raw mode and restore original terminal settings -/
  disableRawMode : m Unit

  /-- Get terminal size as (width, height) -/
  getTerminalSize : m (Nat × Nat)

  /-- Read a single byte from stdin (non-blocking) -/
  readByte : m (Option UInt8)

  /-- Read a single byte from stdin (blocking until input is available) -/
  readByteBlocking : m (Option UInt8)

  /-- Push a byte back onto the input stream (for non-destructive peeks). -/
  unreadByte : UInt8 → m Unit

  /-- Write a string directly to stdout -/
  writeStdout : String → m Unit

  /-- Flush stdout -/
  flushStdout : m Unit

  /-- Read a file as raw bytes. Used for image protocols and other binary payloads. -/
  readFileBytes : System.FilePath → m ByteArray

  /-- Decode image bytes to raw RGB pixels. Returns (width, height, rgb_data) or none on failure. -/
  decodeImageBytes : ByteArray → m (Option (Nat × Nat × ByteArray))

namespace TerminalEffect

/-- Run an action with raw mode enabled, restoring settings on exit -/
def withRawMode [Monad m] [MonadFinally m] [TerminalEffect m] (action : m α) : m α := do
  enableRawMode
  try
    action
  finally
    disableRawMode

end TerminalEffect

end Terminus
