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
          let fileTree := TreeData.branch "project" #[
            TreeData.branch "src" #[
              TreeData.leaf "Main.lean",
              TreeData.leaf "Types.lean"
            ],
            TreeData.branch "tests" #[
              TreeData.leaf "Tests.lean"
            ],
            TreeData.leaf "lakefile.lean"
          ]
          let dataDyn ← Dynamic.pureM fileTree

          let maxVisDyn ← Dynamic.pureM 6
          let treeResult ← treeDyn' dataDyn {
            focusName := "file-tree"
            maxVisible := some maxVisDyn
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

    spacer' 0 1

    -- New widgets: Breadcrumb, Tooltip, Accordion
    row' (gap := 2) {} do
      -- Breadcrumb
      column' (gap := 1) {} do
        titledBlock' "Breadcrumb" .rounded theme none do
          text' "←/→ nav, Enter to select" theme.captionStyle
          let path := #["Home", "Products", "Electronics", "Phones"]
          let bc ← breadcrumb' "demo-breadcrumb" path {
            separator := " > "
          }
          row' (gap := 1) {} do
            text' "Focused:" theme.captionStyle
            let focusStr ← Dynamic.map' bc.focusedIndex (fun idx =>
              match idx with
              | some i => path[i]?.getD "(none)"
              | none => "(none)"
            )
            dynText' focusStr theme.primaryStyle

      -- Tooltip
      column' (gap := 1) {} do
        titledBlock' "Tooltip" .rounded theme none do
          text' "Focus items to see tooltips" theme.captionStyle
          column' (gap := 1) {} do
            tooltip' "tip1" "This is helpful information" {} do
              text' "[?] Hover me 1" { fg := .ansi .cyan }
            tooltip' "tip2" "Another useful tip" { position := .right, hintPrefix := "-> " } do
              text' "[?] Hover me 2" { fg := .ansi .green }

      -- Accordion
      column' (gap := 1) {} do
        titledBlock' "Accordion" .rounded theme none do
          text' "Enter to toggle, ↑/↓ nav" theme.captionStyle
          let sections : Array AccordionSection := #[
            { title := "Section 1", content := text' "Content for section 1" theme.bodyStyle, initiallyOpen := true },
            { title := "Section 2", content := text' "Content for section 2" theme.bodyStyle, initiallyOpen := false },
            { title := "Section 3", content := text' "Content for section 3" theme.bodyStyle, initiallyOpen := false }
          ]
          let acc ← accordion' sections { allowMultiple := true, gap := 0 }
          row' (gap := 1) {} do
            text' "Open:" theme.captionStyle
            let openStr ← Dynamic.map' acc.openSections (fun arr =>
              let count := arr.toList.filter id |>.length
              toString count
            )
            dynText' openStr theme.primaryStyle

    spacer' 0 1

    -- MenuBar and CommandPalette
    row' (gap := 2) {} do
      -- MenuBar
      column' (gap := 1) {} do
        titledBlock' "MenuBar" .rounded theme none do
          text' "←/→ nav, Enter opens, Esc closes" theme.captionStyle
          let menus : Array MenuBarItem := #[
            { label := "File", items := #[
              .item "New" (some "Ctrl+N"),
              .item "Open" (some "Ctrl+O"),
              .separator,
              .item "Exit"
            ]},
            { label := "Edit", items := #[
              .item "Cut" (some "Ctrl+X"),
              .item "Copy" (some "Ctrl+C"),
              .item "Paste" (some "Ctrl+V")
            ]},
            { label := "Help", items := #[
              .item "About"
            ]}
          ]
          let mb ← menuBar' "demo-menubar" menus {}
          let lastLabel ← Reactive.holdDyn "(none)" mb.onSelectLabel
          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            dynText' lastLabel theme.primaryStyle

      -- CommandPalette trigger
      column' (gap := 1) {} do
        titledBlock' "CommandPalette" .rounded theme none do
          text' "Press 'P' to open palette" theme.captionStyle
          let commands : Array Command := #[
            { name := "Build Project", shortcut := some "Ctrl+B" },
            { name := "Run Tests", shortcut := some "Ctrl+T" },
            { name := "Format Code", shortcut := some "Ctrl+Shift+F" },
            { name := "Git Commit" },
            { name := "Git Push" },
            { name := "Open Settings" }
          ]
          let palette ← commandPalette' "demo-palette" commands {}

          -- Wire 'P' key to open palette
          let keyEvents ← useKeyEventW
          let pKeys ← Event.filterM (fun (kd : KeyData) =>
            kd.event.code == .char 'p' || kd.event.code == .char 'P') keyEvents
          performEvent_ (← Event.mapM (fun _ => palette.openPalette) pKeys)

          row' (gap := 1) {} do
            text' "Last:" theme.captionStyle
            let cmdStr ← Dynamic.map' palette.selectedCommand (fun cmd =>
              match cmd with
              | some c => c.name
              | none => "(none)"
            )
            dynText' cmdStr theme.primaryStyle
