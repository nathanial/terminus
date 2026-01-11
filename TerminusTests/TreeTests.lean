-- TerminusTests.TreeTests: Tests for Tree widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Tree

namespace TerminusTests.TreeTests

open Terminus
open Crucible

testSuite "Tree Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "TreeNode.mkLeaf creates leaf node" := do
  let node := TreeNode.mkLeaf "File.txt"
  node.isLeaf ≡ true
  node.label ≡ "File.txt"

test "TreeNode.mkBranch creates branch node" := do
  let node := TreeNode.mkBranch "Folder" [TreeNode.mkLeaf "Child"]
  node.isBranch ≡ true
  node.isExpanded ≡ true

test "TreeNode.mkCollapsed creates collapsed branch" := do
  let node := TreeNode.mkCollapsed "Folder" [TreeNode.mkLeaf "Child"]
  node.isExpanded ≡ false

test "TreeNode.toggle toggles expansion" := do
  let node := TreeNode.mkBranch "Folder" [] |>.toggle
  node.isExpanded ≡ false
  let node2 := node.toggle
  node2.isExpanded ≡ true

test "TreeNode.children returns child nodes" := do
  let child := TreeNode.mkLeaf "Child"
  let node := TreeNode.mkBranch "Parent" [child]
  node.children.length ≡ 1

test "Tree.new creates tree from nodes" := do
  let tree := Tree.new [TreeNode.mkLeaf "A", TreeNode.mkLeaf "B"]
  tree.nodes.length ≡ 2

test "Tree.selectNext advances selection" := do
  let tree := Tree.new [TreeNode.mkLeaf "A", TreeNode.mkLeaf "B"]
    |>.withSelected 0
    |>.selectNext
  tree.selected ≡ 1

test "Tree.selectPrev moves selection back" := do
  let tree := Tree.new [TreeNode.mkLeaf "A", TreeNode.mkLeaf "B"]
    |>.withSelected 1
    |>.selectPrev
  tree.selected ≡ 0

test "Tree.visibleCount counts visible lines" := do
  let tree := Tree.new [
    TreeNode.mkLeaf "A",
    TreeNode.mkBranch "B" [TreeNode.mkLeaf "B1", TreeNode.mkLeaf "B2"]
  ]
  tree.visibleCount ≡ 4  -- A, B, B1, B2

test "Tree renders without crash" := do
  let tree := Tree.new [
    TreeNode.mkBranch "Root" [
      TreeNode.mkLeaf "File1",
      TreeNode.mkBranch "SubFolder" [TreeNode.mkLeaf "File2"]
    ]
  ]
  let buf := renderWidget tree 30 10
  buf.width ≡ 30

#generate_tests

end TerminusTests.TreeTests
