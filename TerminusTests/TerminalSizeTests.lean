-- TerminusTests.TerminalSizeTests: Tests for terminal size operations

import Crucible
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalMock

namespace TerminusTests.TerminalSizeTests

open Terminus
open Crucible

testSuite "Terminal Size Tests"

test "getTerminalSize returns default 80x24" := do
  let (size, _) := MockTerminal.run TerminalEffect.getTerminalSize
  size ≡ (80, 24)

test "getTerminalSize returns configured size" := do
  let initial := MockTerminal.withSize 120 40
  let (size, _) := MockTerminal.run TerminalEffect.getTerminalSize initial
  size ≡ (120, 40)



end TerminusTests.TerminalSizeTests
