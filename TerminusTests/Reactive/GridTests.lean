-- TerminusTests.Reactive.GridTests: Grid widget tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.GridTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Grid Tests"

-- ============================================================================
-- GridCell Tests
-- ============================================================================

test "GridCell.empty creates blank cell" := do
  let cell := GridCell.empty 2
  ensure (cell.content == "  ") "expected 2 spaces"

test "GridCell.filled creates repeated char" := do
  let cell := GridCell.filled '#' 3
  ensure (cell.content == "###") "expected 3 hashes"

test "GridCell.mk' creates custom cell" := do
  let cell := GridCell.mk' "AB" { fg := .ansi .red }
  ensure (cell.content == "AB") "expected AB"

-- ============================================================================
-- Grid Rendering Tests
-- ============================================================================

test "grid' renders simple grid" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      grid' 3 2 (fun x y =>
        { content := s!"{x}{y}", style := {} }
      ) {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Grid should contain cell content like "00", "10", "01", etc.
    SpiderM.liftIO (ensure (rnodeContainsText node "00") "expected cell 0,0")
    SpiderM.liftIO (ensure (rnodeContainsText node "10") "expected cell 1,0")
    SpiderM.liftIO (ensure (rnodeContainsText node "01") "expected cell 0,1")

test "grid' handles empty grid" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      grid' 0 0 (fun _ _ => GridCell.empty 2) {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Empty grid should render as empty node
    match node with
    | RNode.empty => pure ()
    | _ => SpiderM.liftIO (ensure false "expected empty node")

test "grid' renders with border" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      grid' 2 2 (fun _ _ =>
        { content := "XX", style := {} }
      ) { borderType := .rounded }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain border chars
    SpiderM.liftIO (ensure (rnodeContainsText node "╭") "expected top-left border")
    SpiderM.liftIO (ensure (rnodeContainsText node "╮") "expected top-right border")
    SpiderM.liftIO (ensure (rnodeContainsText node "XX") "expected cell content")

-- ============================================================================
-- Cursor Grid Tests
-- ============================================================================

test "cursorGrid' starts at position 0,0" := do
  let env ← SpiderEnv.new
  let (gridResult, _) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let (result, _render) ← (runWidget do
      cursorGrid' 4 4 (fun _x _y isCursor =>
        let content := if isCursor then ">>" else "  "
        { content, style := {} }
      ) { focusName := "grid_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  let (cx, cy) ← gridResult.cursorPos.sample
  ensure (cx == 0 && cy == 0) "cursor should start at 0,0"

  env.currentScope.dispose

test "cursorGrid' arrow keys move cursor" := do
  let env ← SpiderEnv.new
  let (gridResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let (result, _render) ← (runWidget do
      cursorGrid' 4 4 (fun _ _ _ =>
        { content := "  ", style := {} }
      ) { focusName := "grid_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move right
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }
  let (cx1, cy1) ← gridResult.cursorPos.sample
  ensure (cx1 == 1 && cy1 == 0) "cursor should be at 1,0 after right"

  -- Move down
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "grid_0" }
  let (cx2, cy2) ← gridResult.cursorPos.sample
  ensure (cx2 == 1 && cy2 == 1) "cursor should be at 1,1 after down"

  -- Move left
  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "grid_0" }
  let (cx3, cy3) ← gridResult.cursorPos.sample
  ensure (cx3 == 0 && cy3 == 1) "cursor should be at 0,1 after left"

  -- Move up
  inputs.fireKey { event := KeyEvent.up, focusedWidget := some "grid_0" }
  let (cx4, cy4) ← gridResult.cursorPos.sample
  ensure (cx4 == 0 && cy4 == 0) "cursor should be at 0,0 after up"

  env.currentScope.dispose

test "cursorGrid' vim keys work" := do
  let env ← SpiderEnv.new
  let (gridResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let (result, _render) ← (runWidget do
      cursorGrid' 4 4 (fun _ _ _ =>
        { content := "  ", style := {} }
      ) { focusName := "grid_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- l = right
  inputs.fireKey { event := KeyEvent.char 'l', focusedWidget := some "grid_0" }
  let (cx1, _) ← gridResult.cursorPos.sample
  ensure (cx1 == 1) "l should move right"

  -- j = down
  inputs.fireKey { event := KeyEvent.char 'j', focusedWidget := some "grid_0" }
  let (_, cy1) ← gridResult.cursorPos.sample
  ensure (cy1 == 1) "j should move down"

  -- h = left
  inputs.fireKey { event := KeyEvent.char 'h', focusedWidget := some "grid_0" }
  let (cx2, _) ← gridResult.cursorPos.sample
  ensure (cx2 == 0) "h should move left"

  -- k = up
  inputs.fireKey { event := KeyEvent.char 'k', focusedWidget := some "grid_0" }
  let (_, cy2) ← gridResult.cursorPos.sample
  ensure (cy2 == 0) "k should move up"

  env.currentScope.dispose

test "cursorGrid' respects boundaries" := do
  let env ← SpiderEnv.new
  let (gridResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let (result, _render) ← (runWidget do
      cursorGrid' 3 3 (fun _ _ _ =>
        { content := "  ", style := {} }
      ) { focusName := "grid_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Try to move left at boundary
  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "grid_0" }
  let (cx1, _) ← gridResult.cursorPos.sample
  ensure (cx1 == 0) "should not move left past boundary"

  -- Try to move up at boundary
  inputs.fireKey { event := KeyEvent.up, focusedWidget := some "grid_0" }
  let (_, cy1) ← gridResult.cursorPos.sample
  ensure (cy1 == 0) "should not move up past boundary"

  -- Move to far edge
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }
  let (cx2, _) ← gridResult.cursorPos.sample
  ensure (cx2 == 2) "should stop at right boundary"

  env.currentScope.dispose

test "cursorGrid' home moves to start of row" := do
  let env ← SpiderEnv.new
  let (gridResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let (result, _render) ← (runWidget do
      cursorGrid' 5 5 (fun _ _ _ =>
        { content := "  ", style := {} }
      ) { focusName := "grid_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move right a few times
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }

  -- Press home
  inputs.fireKey { event := { code := .home }, focusedWidget := some "grid_0" }
  let (cx, _) ← gridResult.cursorPos.sample
  ensure (cx == 0) "home should move to start of row"

  env.currentScope.dispose

test "cursorGrid' end moves to end of row" := do
  let env ← SpiderEnv.new
  let (gridResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let (result, _render) ← (runWidget do
      cursorGrid' 5 5 (fun _ _ _ =>
        { content := "  ", style := {} }
      ) { focusName := "grid_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Press end
  inputs.fireKey { event := { code := .«end» }, focusedWidget := some "grid_0" }
  let (cx, _) ← gridResult.cursorPos.sample
  ensure (cx == 4) "end should move to end of row"

  env.currentScope.dispose

test "cursorGrid' fires onSelect on enter" := do
  let env ← SpiderEnv.new
  let (lastSelect, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let (result, _render) ← (runWidget do
      cursorGrid' 3 3 (fun _ _ _ =>
        { content := "  ", style := {} }
      ) { focusName := "grid_0" }
    ).run events
    let lastSelect ← captureLatest result.onSelect
    pure (lastSelect, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move to position and select
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "grid_0" }
  inputs.fireKey { event := KeyEvent.enter, focusedWidget := some "grid_0" }

  let selected ← lastSelect.sample
  match selected with
  | some (1, 1) => pure ()
  | _ => ensure false "should have selected (1, 1)"

  env.currentScope.dispose

test "cursorGrid' fires onCursorMove" := do
  let env ← SpiderEnv.new
  let (lastMove, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "grid_0")
    let (result, _render) ← (runWidget do
      cursorGrid' 3 3 (fun _ _ _ =>
        { content := "  ", style := {} }
      ) { focusName := "grid_0" }
    ).run events
    let lastMove ← captureLatest result.onCursorMove
    pure (lastMove, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move cursor
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "grid_0" }

  let moved ← lastMove.sample
  match moved with
  | some (1, 0) => pure ()
  | _ => ensure false "should have moved to (1, 0)"

  env.currentScope.dispose

-- ============================================================================
-- Convenience Function Tests
-- ============================================================================

test "charGrid' renders char array" := do
  runSpider do
    let (events, _) ← createInputs
    let cells : Array (Array (Char × Style)) := #[
      #[('A', {}), ('B', {})],
      #[('C', {}), ('D', {})]
    ]
    let (_, render) ← (runWidget do
      charGrid' cells {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "AA") "expected AA")
    SpiderM.liftIO (ensure (rnodeContainsText node "BB") "expected BB")
    SpiderM.liftIO (ensure (rnodeContainsText node "CC") "expected CC")
    SpiderM.liftIO (ensure (rnodeContainsText node "DD") "expected DD")

test "blockGrid' renders colored blocks" := do
  runSpider do
    let (events, _) ← createInputs
    let cells : Array (Array (Option Color)) := #[
      #[some (.ansi .red), none],
      #[none, some (.ansi .blue)]
    ]
    let (_, render) ← (runWidget do
      blockGrid' cells {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain filled blocks
    SpiderM.liftIO (ensure (rnodeContainsText node "██") "expected filled blocks")



end TerminusTests.Reactive.GridTests
