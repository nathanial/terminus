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
-- TreeNode Tests
-- ============================================================================

test "TreeNode.leaf creates node without children" := do
  let node : TreeNode String := TreeNode.leaf "item"
  ensure node.children.isEmpty "leaf should have no children"
  ensure node.isLeaf "isLeaf should be true"
  ensure (!node.isBranch) "isBranch should be false"

test "TreeNode.branch creates node with children" := do
  let node := TreeNode.branch "parent" #[TreeNode.leaf "child1", TreeNode.leaf "child2"]
  ensure (node.children.size == 2) "branch should have 2 children"
  ensure (!node.isLeaf) "isLeaf should be false"
  ensure node.isBranch "isBranch should be true"

test "TreeNode.toggle toggles expanded state" := do
  let node := TreeNode.branch "test" #[TreeNode.leaf "child"]
  ensure node.expanded "should start expanded"
  let toggled := node.toggle
  ensure (!toggled.expanded) "should be collapsed after toggle"
  let toggled2 := toggled.toggle
  ensure toggled2.expanded "should be expanded after second toggle"

test "TreeNode.toggle does nothing to leaf nodes" := do
  let node : TreeNode String := TreeNode.leaf "item"
  let toggled := node.toggle
  ensure (toggled.expanded == node.expanded) "toggle should not change leaf"

-- ============================================================================
-- Tree Flattening Tests
-- ============================================================================

test "flattenTree produces correct flat representation" := do
  let root := TreeNode.branch "root" #[
    TreeNode.leaf "child1",
    TreeNode.branch "child2" #[
      TreeNode.leaf "grandchild"
    ]
  ]
  let flat := flattenTree root 0 #[0] true
  ensure (flat.size == 4) s!"expected 4 items, got {flat.size}"
  ensure (flat[0]!.value == "root") "first should be root"
  ensure (flat[0]!.depth == 0) "root depth should be 0"
  ensure (flat[1]!.value == "child1") "second should be child1"
  ensure (flat[1]!.depth == 1) "child1 depth should be 1"
  ensure (flat[2]!.value == "child2") "third should be child2"
  ensure (flat[3]!.value == "grandchild") "fourth should be grandchild"
  ensure (flat[3]!.depth == 2) "grandchild depth should be 2"

test "flattenTree respects collapsed branches" := do
  let root := TreeNode.mk "root" #[
    TreeNode.leaf "visible",
    TreeNode.mk "collapsed" #[TreeNode.leaf "hidden"] false  -- expanded = false
  ] true
  let flat := flattenTree root 0 #[0] true
  -- Should see: root, visible, collapsed (but not hidden)
  ensure (flat.size == 3) s!"expected 3 items (hidden child excluded), got {flat.size}"
  ensure (flat.all (fun f => f.value != "hidden")) "hidden should not appear"

-- ============================================================================
-- Tree Widget Tests
-- ============================================================================

test "tree' renders tree nodes" := do
  runSpider do
    let (events, _) ← createInputs
    let root := TreeNode.branch "Root" #[
      TreeNode.leaf "Child1",
      TreeNode.leaf "Child2"
    ]
    let (_, render) ← (runWidget do
      let _result ← tree' root {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Tree renders with icons, so use containsText for substring matching
    SpiderM.liftIO (ensure (rnodeContainsText node "Root") "expected Root")
    SpiderM.liftIO (ensure (rnodeContainsText node "Child1") "expected Child1")
    SpiderM.liftIO (ensure (rnodeContainsText node "Child2") "expected Child2")

test "tree' starts with first node selected" := do
  let env ← SpiderEnv.new
  let (treeResult, _) ← (do
    let (events, inputs) ← createInputs
    let root := TreeNode.branch "Root" #[TreeNode.leaf "Child"]
    let (result, _render) ← (runWidget do
      tree' root {}
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  let selectedNode ← treeResult.selectedNode.sample
  match selectedNode with
  | some "Root" => pure ()
  | _ => ensure false "expected Root to be selected"

  env.currentScope.dispose

test "tree' down arrow moves selection" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    let root := TreeNode.branch "Root" #[
      TreeNode.leaf "Child1",
      TreeNode.leaf "Child2"
    ]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
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
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }

  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some "Child1" => pure ()
  | _ => ensure false "should move to Child1"

  env.currentScope.dispose

test "tree' up arrow moves selection" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    let root := TreeNode.branch "Root" #[
      TreeNode.leaf "Child1",
      TreeNode.leaf "Child2"
    ]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move down twice then up once
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }
  inputs.fireKey { event := KeyEvent.up, focusedWidget := some "tree_0" }

  let sel ← treeResult.selectedNode.sample
  match sel with
  | some "Child1" => pure ()
  | _ => ensure false "should be at Child1"

  env.currentScope.dispose

test "tree' j/k keys work for navigation" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    let root := TreeNode.branch "Root" #[TreeNode.leaf "Child"]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- j moves down
  inputs.fireKey { event := KeyEvent.char 'j', focusedWidget := some "tree_0" }
  let sel1 ← treeResult.selectedNode.sample
  match sel1 with
  | some "Child" => pure ()
  | _ => ensure false "j should move to Child"

  -- k moves up
  inputs.fireKey { event := KeyEvent.char 'k', focusedWidget := some "tree_0" }
  let sel2 ← treeResult.selectedNode.sample
  match sel2 with
  | some "Root" => pure ()
  | _ => ensure false "k should move back to Root"

  env.currentScope.dispose

test "tree' left collapses expanded branch" := do
  let env ← SpiderEnv.new
  let (lastToggle, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    let root := TreeNode.branch "Root" #[TreeNode.leaf "Child"]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
    ).run events
    let lastToggle ← captureLatest result.onToggle
    pure (lastToggle, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Press left to collapse Root
  inputs.fireKey { event := KeyEvent.left, focusedWidget := some "tree_0" }

  let toggled ← lastToggle.sample
  match toggled with
  | some path => ensure (path == #[0]) "should toggle root path"
  | none => ensure false "should have fired onToggle"

  env.currentScope.dispose

test "tree' right expands collapsed branch" := do
  let env ← SpiderEnv.new
  let (lastToggle, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    -- Start with collapsed root
    let root := TreeNode.collapsedBranch "Root" #[TreeNode.leaf "Child"]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
    ).run events
    let lastToggle ← captureLatest result.onToggle
    pure (lastToggle, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Press right to expand
  inputs.fireKey { event := KeyEvent.right, focusedWidget := some "tree_0" }

  let toggled ← lastToggle.sample
  match toggled with
  | some _ => pure ()
  | none => ensure false "should have fired onToggle to expand"

  env.currentScope.dispose

test "tree' enter on leaf fires onSelect" := do
  let env ← SpiderEnv.new
  let (lastSelect, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    let root := TreeNode.branch "Root" #[TreeNode.leaf "Leaf"]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
    ).run events
    let lastSelect ← captureLatest result.onSelect
    pure (lastSelect, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move to leaf
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }
  -- Press enter
  inputs.fireKey { event := KeyEvent.enter, focusedWidget := some "tree_0" }

  let selected ← lastSelect.sample
  match selected with
  | some "Leaf" => pure ()
  | _ => ensure false "should have selected Leaf"

  env.currentScope.dispose

test "tree' home jumps to first" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    let root := TreeNode.branch "Root" #[
      TreeNode.leaf "A",
      TreeNode.leaf "B",
      TreeNode.leaf "C"
    ]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Move down a few times
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }

  -- Press home
  inputs.fireKey { event := { code := .home }, focusedWidget := some "tree_0" }

  let sel ← treeResult.selectedNode.sample
  match sel with
  | some "Root" => pure ()
  | _ => ensure false "home should return to Root"

  env.currentScope.dispose

test "tree' end jumps to last" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    let root := TreeNode.branch "Root" #[
      TreeNode.leaf "A",
      TreeNode.leaf "B",
      TreeNode.leaf "Last"
    ]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Press end
  inputs.fireKey { event := { code := .«end» }, focusedWidget := some "tree_0" }

  let sel ← treeResult.selectedNode.sample
  match sel with
  | some "Last" => pure ()
  | _ => ensure false "end should jump to Last"

  env.currentScope.dispose

test "tree' path tracks selection correctly" := do
  let env ← SpiderEnv.new
  let (treeResult, inputs) ← (do
    let (events, inputs) ← createInputs
    SpiderM.liftIO <| events.registry.fireFocus (some "tree_0")
    let root := TreeNode.branch "Root" #[
      TreeNode.branch "Branch" #[
        TreeNode.leaf "Nested"
      ]
    ]
    let (result, _render) ← (runWidget do
      tree' root { focusName := "tree_0" }
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Navigate to nested item: Root -> Branch -> Nested
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }  -- Branch
  inputs.fireKey { event := KeyEvent.down, focusedWidget := some "tree_0" }  -- Nested

  let path ← treeResult.selectedPath.sample
  -- Path should be [0, 0, 0] for root->branch->nested
  ensure (path.size == 3) s!"expected path length 3, got {path.size}"

  env.currentScope.dispose

test "tree' renders empty state" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      let _result ← forest' (#[] : Array (TreeNode String)) {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeHasText node "(empty)") "expected empty message")

test "forest' handles multiple roots" := do
  runSpider do
    let (events, _) ← createInputs
    let roots : Array (Terminus.Reactive.TreeNode String) := #[
      Terminus.Reactive.TreeNode.leaf "Root1",
      Terminus.Reactive.TreeNode.leaf "Root2",
      Terminus.Reactive.TreeNode.leaf "Root3"
    ]
    let (_, render) ← (runWidget do
      let _result ← forest' roots {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Tree renders with icons, so use containsText for substring matching
    SpiderM.liftIO (ensure (rnodeContainsText node "Root1") "expected Root1")
    SpiderM.liftIO (ensure (rnodeContainsText node "Root2") "expected Root2")
    SpiderM.liftIO (ensure (rnodeContainsText node "Root3") "expected Root3")

#generate_tests

end TerminusTests.Reactive.TreeTests
