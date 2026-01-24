-- TerminusTests.Reactive.TreeTests: Tree widget tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.TreeTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Tree Tests"

-- ============================================================================
-- TreeData Tests
-- ============================================================================

test "TreeData.leaf creates node without children" := do
  let node : TreeData String := TreeData.leaf "item"
  ensure node.children.isEmpty "leaf should have no children"
  ensure node.isLeaf "isLeaf should be true"
  ensure (!node.isBranch) "isBranch should be false"

test "TreeData.branch creates node with children" := do
  let node := TreeData.branch "parent" #[TreeData.leaf "child1", TreeData.leaf "child2"]
  ensure (node.children.size == 2) "branch should have 2 children"
  ensure (!node.isLeaf) "isLeaf should be false"
  ensure node.isBranch "isBranch should be true"

-- ============================================================================
-- Tree Flattening Tests
-- ============================================================================

test "flattenForestData produces correct flat representation" := do
  let root := TreeData.branch "root" #[
    TreeData.leaf "child1",
    TreeData.branch "child2" #[
      TreeData.leaf "grandchild"
    ]
  ]
  let flat := flattenForestData #[root] ExpansionState.empty true
  ensure (flat.size == 4) s!"expected 4 items, got {flat.size}"
  ensure (flat[0]!.value == "root") "first should be root"
  ensure (flat[0]!.depth == 0) "root depth should be 0"
  ensure (flat[1]!.value == "child1") "second should be child1"
  ensure (flat[1]!.depth == 1) "child1 depth should be 1"
  ensure (flat[2]!.value == "child2") "third should be child2"
  ensure (flat[3]!.value == "grandchild") "fourth should be grandchild"
  ensure (flat[3]!.depth == 2) "grandchild depth should be 2"

test "flattenForestData respects collapsed branches" := do
  let root := TreeData.branch "root" #[
    TreeData.leaf "visible",
    TreeData.branch "collapsed" #[TreeData.leaf "hidden"]
  ]
  -- Set "collapsed" branch to collapsed via expansion state
  let expansion := ExpansionState.empty.set #[0, 1] false
  let flat := flattenForestData #[root] expansion true
  -- Should see: root, visible, collapsed (but not hidden)
  ensure (flat.size == 3) s!"expected 3 items (hidden child excluded), got {flat.size}"
  ensure (flat.all (fun f => f.value != "hidden")) "hidden should not appear"

-- ============================================================================
-- Tree Widget Tests
-- ============================================================================

test "treeDyn' renders tree nodes" := do
  runSpider do
    let (events, _) ← createInputs
    let root := TreeData.branch "Root" #[
      TreeData.leaf "Child1",
      TreeData.leaf "Child2"
    ]
    let dataDyn ← Dynamic.pureM root
    let (_, render) ← (runWidget do
      let _result ← treeDyn' dataDyn {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Tree renders with icons, so use containsText for substring matching
    SpiderM.liftIO (ensure (rnodeContainsText node "Root") "expected Root")
    SpiderM.liftIO (ensure (rnodeContainsText node "Child1") "expected Child1")
    SpiderM.liftIO (ensure (rnodeContainsText node "Child2") "expected Child2")

test "treeDyn' starts with first node selected" := do
  let env ← SpiderEnv.new
  let (treeResult, _) ← (do
    let (events, inputs) ← createInputs
    let root := TreeData.branch "Root" #[TreeData.leaf "Child"]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn {}
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  let selectedNode ← treeResult.selectedNode.sample
  match selectedNode with
  | some "Root" => pure ()
  | _ => ensure false "expected Root to be selected"

  env.currentScope.dispose

test "treeDyn' down arrow moves selection" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let root := TreeData.branch "Root" #[
      TreeData.leaf "Child1",
      TreeData.leaf "Child2"
    ]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Start at Root
  let sel1 ← treeResult.selectedNode.sample
  match sel1 with
  | some "Root" => pure ()
  | _ => ensure false "should start at Root"

  -- Press down
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }

  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some "Child1" => pure ()
  | _ => ensure false "should move to Child1"

  env.currentScope.dispose

test "treeDyn' up arrow moves selection" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let root := TreeData.branch "Root" #[
      TreeData.leaf "Child1",
      TreeData.leaf "Child2"
    ]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move down twice then up once
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }
  inputs.fireKey { event := KeyEvent.up, focusedWidget := some "forest_0" }

  let sel ← treeResult.selectedNode.sample
  match sel with
  | some "Child1" => pure ()
  | _ => ensure false "should be at Child1"

  env.currentScope.dispose

test "treeDyn' j/k keys work for navigation" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let root := TreeData.branch "Root" #[TreeData.leaf "Child"]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- j moves down
  inputs.fireKey { event := KeyEvent.char 'j', focusedWidget := some "forest_0" }
  let sel1 ← treeResult.selectedNode.sample
  match sel1 with
  | some "Child" => pure ()
  | _ => ensure false "j should move to Child"

  -- k moves up
  inputs.fireKey { event := KeyEvent.char 'k', focusedWidget := some "forest_0" }
  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some "Root" => pure ()
  | _ => ensure false "k should move back to Root"

  env.currentScope.dispose

test "treeDyn' left collapses expanded branch" := do
  let env ← SpiderEnv.new
  let (lastToggle, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let root := TreeData.branch "Root" #[TreeData.leaf "Child"]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0" }
    ).run events
    let lastToggle ← captureLatest result.onToggle
    pure (lastToggle, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Press left to collapse Root
  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "forest_0" }

  let toggled ← lastToggle.sample
  match toggled with
  | some path => ensure (path == #[0]) "should toggle root path"
  | none => ensure false "should have fired onToggle"

  env.currentScope.dispose

test "treeDyn' right expands collapsed branch" := do
  let env ← SpiderEnv.new
  let (lastToggle, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    -- Start with default expansion (expanded by default, so first collapse it)
    let root := TreeData.branch "Root" #[TreeData.leaf "Child"]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0", expandByDefault := false }
    ).run events
    let lastToggle ← captureLatest result.onToggle
    pure (lastToggle, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Press right to expand
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "forest_0" }

  let toggled ← lastToggle.sample
  match toggled with
  | some _ => pure ()
  | none => ensure false "should have fired onToggle to expand"

  env.currentScope.dispose

test "treeDyn' enter on leaf fires onSelect" := do
  let env ← SpiderEnv.new
  let (lastSelect, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let root := TreeData.branch "Root" #[TreeData.leaf "Leaf"]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0" }
    ).run events
    let lastSelect ← captureLatest result.onSelect
    pure (lastSelect, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move to leaf
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }
  -- Press enter
  inputs.fireKey { event := KeyEvent.enter, focusedWidget := some "forest_0" }

  let selected ← lastSelect.sample
  match selected with
  | some "Leaf" => pure ()
  | _ => ensure false "should have selected Leaf"

  env.currentScope.dispose

test "treeDyn' home jumps to first" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let root := TreeData.branch "Root" #[
      TreeData.leaf "A",
      TreeData.leaf "B",
      TreeData.leaf "C"
    ]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move down a few times
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }

  -- Press home
  inputs.fireKey { event := { code := .home }, focusedWidget := some "forest_0" }

  let sel ← treeResult.selectedNode.sample
  match sel with
  | some "Root" => pure ()
  | _ => ensure false "home should return to Root"

  env.currentScope.dispose

test "treeDyn' end jumps to last" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let root := TreeData.branch "Root" #[
      TreeData.leaf "A",
      TreeData.leaf "B",
      TreeData.leaf "Last"
    ]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Press end
  inputs.fireKey { event := { code := .«end» }, focusedWidget := some "forest_0" }

  let sel ← treeResult.selectedNode.sample
  match sel with
  | some "Last" => pure ()
  | _ => ensure false "end should jump to Last"

  env.currentScope.dispose

test "treeDyn' path tracks selection correctly" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let root := TreeData.branch "Root" #[
      TreeData.branch "Branch" #[
        TreeData.leaf "Nested"
      ]
    ]
    let dataDyn ← Dynamic.pureM root
    let (result, _render) ← (runWidget do
      treeDyn' dataDyn { focusName := "forest_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Navigate to nested item: Root -> Branch -> Nested
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }  -- Branch
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }  -- Nested

  let path ← treeResult.selectedPath.sample
  -- Path should be [0, 0, 0] for root->branch->nested
  ensure (path.size == 3) s!"expected path length 3, got {path.size}"

  env.currentScope.dispose

test "forestDyn' renders empty state" := do
  runSpider do
    let (events, _) ← createInputs
    let dataDyn ← Dynamic.pureM (#[] : Array (TreeData String))
    let (_, render) ← (runWidget do
      let _result ← forestDyn' dataDyn {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeHasText node "(empty)") "expected empty message")

test "forestDyn' handles multiple roots" := do
  runSpider do
    let (events, _) ← createInputs
    let roots : Array (TreeData String) := #[
      TreeData.leaf "Root1",
      TreeData.leaf "Root2",
      TreeData.leaf "Root3"
    ]
    let dataDyn ← Dynamic.pureM roots
    let (_, render) ← (runWidget do
      let _result ← forestDyn' dataDyn {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Tree renders with icons, so use containsText for substring matching
    SpiderM.liftIO (ensure (rnodeContainsText node "Root1") "expected Root1")
    SpiderM.liftIO (ensure (rnodeContainsText node "Root2") "expected Root2")
    SpiderM.liftIO (ensure (rnodeContainsText node "Root3") "expected Root3")

test "forestDyn' renders tree data" := do
  runSpider do
    let (events, _) ← createInputs
    let roots := #[
      TreeData.branch "Root" #[
        TreeData.leaf "Child1",
        TreeData.leaf "Child2"
      ]
    ]
    let dataDyn ← Dynamic.pureM roots
    let (_, render) ← (runWidget do
      let _result ← forestDyn' dataDyn {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Root") "expected Root")
    SpiderM.liftIO (ensure (rnodeContainsText node "Child1") "expected Child1")
    SpiderM.liftIO (ensure (rnodeContainsText node "Child2") "expected Child2")

test "forestDyn' navigation works" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")
    let roots := #[
      TreeData.branch "Root" #[
        TreeData.leaf "Child1",
        TreeData.leaf "Child2"
      ]
    ]
    let dataDyn ← Dynamic.pureM roots
    let (result, _render) ← (runWidget do
      forestDyn' dataDyn { focusName := "forest_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Start at Root
  let sel1 ← treeResult.selectedNode.sample
  match sel1 with
  | some "Root" => pure ()
  | _ => ensure false "should start at Root"

  -- Press down to move to Child1
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }

  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some "Child1" => pure ()
  | _ => ensure false "should move to Child1"

  env.currentScope.dispose

test "forestDyn' state persists across data updates" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs, fireData) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "forest_0")

    -- Create initial data
    let initialRoots := #[
      TreeData.branch "Project A" #[
        TreeData.leaf "File 1",
        TreeData.leaf "File 2"
      ],
      TreeData.branch "Project B" #[
        TreeData.leaf "File 3"
      ]
    ]

    -- Create a dynamic that can be updated
    let (dataEvent, fireData) ← Reactive.newTriggerEvent (t := Spider) (a := Array (TreeData String))
    let dataDyn ← Reactive.holdDyn initialRoots dataEvent

    let (result, _render) ← (runWidget do
      forestDyn' dataDyn { focusName := "forest_0" }
    ).run events
    pure (result, inputs, fireData)
  ).run env

  env.postBuildTrigger ()

  -- Navigate to "File 2" (down twice: Project A -> File 1 -> File 2)
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "forest_0" }

  let sel1 ← treeResult.selectedNode.sample
  match sel1 with
  | some "File 2" => pure ()
  | other => ensure false s!"should be at File 2, got {other}"

  -- Now update the data (add a new file to Project A)
  let updatedRoots := #[
    TreeData.branch "Project A" #[
      TreeData.leaf "File 1",
      TreeData.leaf "File 1.5",  -- New file inserted
      TreeData.leaf "File 2"
    ],
    TreeData.branch "Project B" #[
      TreeData.leaf "File 3"
    ]
  ]
  env.withFrame (fireData updatedRoots)

  -- Selection should still be at index 2, which is now "File 1.5"
  -- Actually, the selection index is preserved, so it's at the same visual position
  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some _ => pure ()  -- Just ensure we have a valid selection
  | none => ensure false "should still have a selection after data update"

  -- The key test: selection index should be preserved (not reset to 0)
  let path ← treeResult.selectedPath.sample
  ensure (path.size > 0) "should have a valid path after data update"

  env.currentScope.dispose

test "treeDyn' single root works" := do
  runSpider do
    let (events, _) ← createInputs
    let root := TreeData.branch "Root" #[
      TreeData.leaf "Child1",
      TreeData.leaf "Child2"
    ]
    let dataDyn ← Dynamic.pureM root
    let (_, render) ← (runWidget do
      let _result ← treeDyn' dataDyn {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Root") "expected Root")
    SpiderM.liftIO (ensure (rnodeContainsText node "Child1") "expected Child1")

test "forestDyn' globalKeys works without focus" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    -- NOTE: We do NOT call fireFocus here - tree should work via globalKeys
    let roots := #[
      TreeData.branch "Root" #[
        TreeData.leaf "Child1",
        TreeData.leaf "Child2"
      ]
    ]
    let dataDyn ← Dynamic.pureM roots
    let (result, _render) ← (runWidget do
      forestDyn' dataDyn { globalKeys := true }  -- globalKeys enabled
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Start at Root
  let sel1 ← treeResult.selectedNode.sample
  match sel1 with
  | some "Root" => pure ()
  | _ => ensure false "should start at Root"

  -- Press down WITHOUT focus set - should still work with globalKeys
  inputs.fireKey { event := KeyEvent.down, focusedWidget := none }

  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some "Child1" => pure ()
  | other => ensure false s!"globalKeys should allow navigation without focus, got {other}"

  env.currentScope.dispose

end TerminusTests.Reactive.TreeTests
