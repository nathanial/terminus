-- TerminusTests.RawModeTests: Tests for raw mode terminal operations

import Crucible
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalMock

namespace TerminusTests.RawModeTests

open Terminus
open Crucible

testSuite "Raw Mode Tests"

test "enableRawMode sets rawModeEnabled to true" := do
  let (_, state) := MockTerminal.run TerminalEffect.enableRawMode
  state.rawModeEnabled ≡ true

test "disableRawMode sets rawModeEnabled to false" := do
  let initial : MockTerminalState := { rawModeEnabled := true }
  let (_, state) := MockTerminal.run TerminalEffect.disableRawMode initial
  state.rawModeEnabled ≡ false

test "withRawMode restores state after action" := do
  let action := TerminalEffect.withRawMode (pure ())
  let (_, state) := MockTerminal.run action
  state.rawModeEnabled ≡ false

test "withRawMode enables raw mode during action" := do
  let action := TerminalEffect.withRawMode do
    let s ← get
    pure s.rawModeEnabled
  let (wasEnabled, _) := MockTerminal.run action
  wasEnabled ≡ true



end TerminusTests.RawModeTests
