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

  /-- Write a string directly to stdout -/
  writeStdout : String → m Unit

  /-- Flush stdout -/
  flushStdout : m Unit

namespace TerminalEffect

/-- Run an action with raw mode enabled, restoring settings on exit -/
def withRawMode [Monad m] [TerminalEffect m] (action : m α) : m α := do
  enableRawMode
  let result ← action
  disableRawMode
  pure result

end TerminalEffect

end Terminus
