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

          let node ← tabResult.activeTab.map' (fun idx =>
            let content := match idx with
              | 0 => "Welcome to the Home tab!"
              | 1 => "Configure your settings here."
              | _ => "Help: Use arrow keys to navigate."
            RNode.text content theme.bodyStyle
          )
          emit node

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
            let node ← treeResult.selectedNode.map' (fun selected =>
              RNode.text (selected.getD "(none)") theme.primaryStyle
            )
            emit node

    spacer' 0 1

    row' (gap := 2) {} do
      -- SplitPane demo
      column' (gap := 1) {} do
        titledBlock' "Resizable SplitPane" .rounded theme none do
          text' "Focus then ←/→ to resize" theme.captionStyle
          let (splitResult, _, _) ← resizableSplitPane' "demo-split"
            { initialRatio := 0.4, minRatio := 0.2, maxRatio := 0.8 }
            (do
              column' (gap := 0) {} do
                text' "Left" { fg := .ansi .cyan }
                text' "Panel" { fg := .ansi .cyan })
            (do
              column' (gap := 0) {} do
                text' "Right" { fg := .ansi .green }
                text' "Panel" { fg := .ansi .green })
          row' (gap := 1) {} do
            text' "Ratio:" theme.captionStyle
            let ratioStr ← Dynamic.map' splitResult.ratio fun r =>
              s!"{(r * 100).toUInt32}%"
            dynText' ratioStr theme.primaryStyle
