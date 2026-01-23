-- TerminusTests.Reactive.DataGridTests: DataGrid widget tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.DataGridTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive DataGrid Tests"

test "dataGrid' renders headers and cells" := do
  runSpider do
    let (events, _) ← createInputs
    let gridData := #[#["1", "Alice"], #["2", "Bob"]]
    let (_, render) ← (runWidget do
      dataGrid' gridData {
        columnHeaders := some #["ID", "Name"]
        cellWidth := 6
        rowHeaderWidth := 2
      }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "ID") "expected column header")
    SpiderM.liftIO (ensure (rnodeContainsText node "Alice") "expected cell content")
    SpiderM.liftIO (ensure (rnodeContainsText node "2") "expected row header")

test "dataGrid' arrow keys move selection" := do
  let env ← SpiderEnv.new
  let (gridResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let data := #[#["A1", "B1"], #["A2", "B2"]]
    let (result, _render) ← (runWidget do
      dataGrid' data { focusName := "grid_0", cellWidth := 4 }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  let (r0, c0) ← gridResult.selectedPos.sample
  ensure (r0 == 0 && c0 == 0) "expected initial selection 0,0"

  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }
  let (r1, c1) ← gridResult.selectedPos.sample
  ensure (r1 == 0 && c1 == 1) "expected selection 0,1 after right"

  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "grid_0" }
  let (r2, c2) ← gridResult.selectedPos.sample
  ensure (r2 == 1 && c2 == 1) "expected selection 1,1 after down"

  env.currentScope.dispose

test "dataGrid' edits cell on enter" := do
  let env ← SpiderEnv.new
  let (gridResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let data := #[#["A1", "B1"], #["A2", "B2"]]
    let (result, _render) ← (runWidget do
      dataGrid' data { focusName := "grid_0", cellWidth := 4 }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  inputs.fireKey { event := KeyEvent.enter, focusedWidget := some "grid_0" }
  inputs.fireKey { event := KeyEvent.char 'X', focusedWidget := some "grid_0" }
  inputs.fireKey { event := KeyEvent.enter, focusedWidget := some "grid_0" }

  let data' ← gridResult.data.sample
  if h : 0 < data'.size then
    let row := data'[0]
    if h2 : 0 < row.size then
      let value := row[0]
      ensure (value == "A1X") "expected edited value"
    else
      ensure false "missing cell 0,0"
  else
    ensure false "missing row 0"

  env.currentScope.dispose



end TerminusTests.Reactive.DataGridTests
