-- Terminus.Backend.Raw: FFI bindings for terminal raw mode

namespace Terminus

/-- Enable raw terminal mode (no echo, character-by-character input) -/
@[extern "terminus_enable_raw_mode"]
opaque enableRawMode : IO Unit

/-- Disable raw mode and restore original terminal settings -/
@[extern "terminus_disable_raw_mode"]
opaque disableRawMode : IO Unit

/-- Get terminal size as (width, height) -/
@[extern "terminus_get_size"]
opaque getTerminalSize : IO (Nat × Nat)

/-- Read a single byte from stdin (non-blocking) -/
@[extern "terminus_read_byte"]
opaque readByte : IO (Option UInt8)

/-- Read a single byte from stdin (blocking) -/
@[extern "terminus_read_byte_blocking"]
opaque readByteBlocking : IO (Option UInt8)

/-- Push a byte back onto the input stream. -/
@[extern "terminus_unread_byte"]
opaque unreadByte : UInt8 → IO Unit

/-- Write a string directly to stdout -/
@[extern "terminus_write_stdout"]
opaque writeStdout : @&String → IO Unit

/-- Flush stdout -/
@[extern "terminus_flush_stdout"]
opaque flushStdout : IO Unit

/-- Run an IO action with raw mode enabled, restoring settings on exit -/
def withRawMode (action : IO α) : IO α := do
  enableRawMode
  try
    action
  finally
    disableRawMode

end Terminus
