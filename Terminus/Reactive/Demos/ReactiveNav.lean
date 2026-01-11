/-
  Reactive Navigation Demo Widgets
  Shared widget tree for the reactive navigation demo (used by tests and the executable).
-/
import Terminus.Reactive

open Terminus.Reactive
open Reactive Reactive.Host

def reactiveNavApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Define focusable component names
  let focusableNames := #["tabs-demo", "file-tree"]
  let focusIndexRef ← SpiderM.liftIO (IO.mkRef 0)

  -- Subscribe to Tab key to cycle focus
  let keyEvents ← useKeyEvent
  let events ← getEvents
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    match kd.event.code with
    | .tab =>
      let idx ← focusIndexRef.get
      let nextIdx := (idx + 1) % focusableNames.size
      focusIndexRef.set nextIdx
      if h : nextIdx < focusableNames.size then
        events.registry.fireFocus (some focusableNames[nextIdx])
    | _ => pure ()

  -- Set initial focus to tabs
  SpiderM.liftIO <| events.registry.fireFocus (some "tabs-demo")

  -- Build the UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Navigation Demo ===" theme.heading1Style
      text' "Tab: cycle focus | Arrows: navigate | Enter: select | Ctrl+C: quit" theme.captionStyle

      -- Row 1: Tabs demo with content switching
      titledBlock' "Tabs Demo" .rounded theme do
        let tabResult ← tabs' #["Home", "Settings", "Help", "About"] 0 {
          focusName := "tabs-demo"
          activeStyle := { fg := .ansi .cyan, modifier := { bold := true } }
        }

        -- Tab content area
        emitDynamic do
          let idx ← tabResult.activeTab.sample
          let content := match idx with
            | 0 => "Welcome to the Home tab! This is the main dashboard."
            | 1 => "Settings: Configure your preferences here."
            | 2 => "Help: Press arrow keys to navigate tabs."
            | _ => "About: Reactive Navigation Demo v1.0"
          pure (RNode.text content theme.bodyStyle)

        -- Show current tab index
        row' (gap := 1) {} do
          text' "Active tab:" theme.captionStyle
          emitDynamic do
            let idx ← tabResult.activeTab.sample
            pure (RNode.text (toString idx) theme.primaryStyle)

      -- Row 2: Tree demo
      titledBlock' "File Tree" .rounded theme do
        -- Sample file tree structure
        let fileTree := TreeNode.branch "project" #[
          TreeNode.branch "src" #[
            TreeNode.branch "components" #[
              TreeNode.leaf "Button.lean",
              TreeNode.leaf "Input.lean",
              TreeNode.leaf "List.lean"
            ],
            TreeNode.leaf "Main.lean",
            TreeNode.leaf "Types.lean"
          ],
          TreeNode.branch "tests" #[
            TreeNode.leaf "ButtonTests.lean",
            TreeNode.leaf "InputTests.lean"
          ],
          TreeNode.leaf "lakefile.lean",
          TreeNode.leaf "README.md"
        ]

        let treeResult ← tree' fileTree {
          focusName := "file-tree"
          maxVisible := some 8
          selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
          branchStyle := { fg := .ansi .yellow, modifier := { bold := true } }
          expandedIcon := "▼ "
          collapsedIcon := "▶ "
          leafIcon := "  "
        }

        -- Show selected item
        row' (gap := 1) {} do
          text' "Selected:" theme.captionStyle
          emitDynamic do
            let node ← treeResult.selectedNode.sample
            let display := node.getD "(none)"
            pure (RNode.text display theme.primaryStyle)

        -- Show path
        row' (gap := 1) {} do
          text' "Path:" theme.captionStyle
          emitDynamic do
            let path ← treeResult.selectedPath.sample
            let pathStr := String.intercalate "/" (path.toList.map toString)
            pure (RNode.text s!"[{pathStr}]" theme.captionStyle)

      -- Status bar showing current focus
      let focusedInput ← useFocusedInputW
      emitDynamic do
        let focused ← focusedInput.sample
        let focusName := focused.getD "(none)"
        pure (RNode.text s!"Focused: {focusName}" theme.captionStyle)

  pure { render }
