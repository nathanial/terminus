import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Navigation Tab Content -/

def navigationContent (theme : Theme) (_events : TerminusEvents) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Tabs and tree widgets for navigation." theme.captionStyle

    row' (gap := 2) {} do
      -- Tabs demo
      column' (gap := 1) {} do
        titledBlock' "Tabs Widget" .rounded theme none do
          let tabResult ← tabs' #["Home", "Settings", "Help"] 0 {
            focusName := "inner-tabs"
            activeStyle := { fg := .ansi .cyan, modifier := { bold := true } }
          }

          emitDynamic do
            let idx ← tabResult.activeTab.sample
            let content := match idx with
              | 0 => "Welcome to the Home tab!"
              | 1 => "Configure your settings here."
              | _ => "Help: Use arrow keys to navigate."
            pure (RNode.text content theme.bodyStyle)

      -- Tree demo
      column' (gap := 1) {} do
        titledBlock' "Tree Widget" .rounded theme none do
          let fileTree := TreeNode.branch "project" #[
            TreeNode.branch "src" #[
              TreeNode.leaf "Main.lean",
              TreeNode.leaf "Types.lean"
            ],
            TreeNode.branch "tests" #[
              TreeNode.leaf "Tests.lean"
            ],
            TreeNode.leaf "lakefile.lean"
          ]

          let treeResult ← tree' fileTree {
            focusName := "file-tree"
            maxVisible := some 6
            selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
            branchStyle := { fg := .ansi .yellow, modifier := { bold := true } }
            expandedIcon := "▼ "
            collapsedIcon := "▶ "
          }

          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let node ← treeResult.selectedNode.sample
              pure (RNode.text (node.getD "(none)") theme.primaryStyle)
