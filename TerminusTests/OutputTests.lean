-- TerminusTests.OutputTests: Tests for output writing operations

import Crucible
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalMock

namespace TerminusTests.OutputTests

open Terminus
open Crucible

testSuite "Output Tests"

test "writeStdout accumulates output" := do
  let action := do
    TerminalEffect.writeStdout "Hello, "
    TerminalEffect.writeStdout "World!"
  let (_, state) := MockTerminal.run action
  state.outputBuffer ≡ "Hello, World!"

test "writeStdout clears flushed flag" := do
  let action := TerminalEffect.writeStdout "test"
  let (_, state) := MockTerminal.run action
  state.flushed ≡ false

test "flushStdout sets flushed flag" := do
  let action := do
    TerminalEffect.writeStdout "test"
    TerminalEffect.flushStdout
  let (_, state) := MockTerminal.run action
  state.flushed ≡ true



end TerminusTests.OutputTests
