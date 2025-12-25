-- FileExplorer: Tree and Scrollbar demo with Popup confirmation
-- Use arrow keys to navigate, Enter to expand/collapse, 'd' for delete popup

import Terminus

open Terminus

/-- Sample file tree structure -/
def sampleTree : List TreeNode := [
  .branch "src" [
    .branch "components" [
      .leaf "Button.tsx",
      .leaf "Input.tsx",
      .leaf "Modal.tsx",
      .leaf "Dropdown.tsx",
      .leaf "Table.tsx"
    ] true,
    .branch "utils" [
      .leaf "format.ts",
      .leaf "validate.ts",
      .leaf "api.ts"
    ] false,
    .branch "hooks" [
      .leaf "useAuth.ts",
      .leaf "useForm.ts",
      .leaf "useQuery.ts"
    ] true,
    .leaf "index.ts",
    .leaf "App.tsx"
  ] true,
  .branch "tests" [
    .leaf "Button.test.tsx",
    .leaf "Input.test.tsx",
    .leaf "utils.test.ts"
  ] false,
  .branch "public" [
    .leaf "index.html",
    .leaf "favicon.ico"
  ] true,
  .leaf "package.json",
  .leaf "tsconfig.json",
  .leaf "README.md"
]

structure FileExplorerState where
  tree : Tree := Tree.new sampleTree
  showPopup : Bool := false
  confirmPopup : ConfirmPopup := ConfirmPopup.new "Delete this file?"
  deriving Inhabited

def draw (frame : Frame) (state : FileExplorerState) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  -- Main layout
  let mainBlock := Block.double
    |>.withTitle "File Explorer"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  -- Split into tree and scrollbar
  let sections := hsplit inner [.fill, .fixed 1]

  -- Tree view
  if h : 0 < sections.length then
    let treeArea := sections[0]
    let treeWidget := state.tree
      |>.withSelectedStyle (Style.reversed.withFg Color.cyan)
      |>.withBranchStyle (Style.bold.withFg Color.yellow)
      |>.withLeafStyle (Style.fgColor Color.white)
      |>.withPrefixStyle (Style.fgColor Color.gray)
    f := f.render treeWidget treeArea

  -- Scrollbar
  if h : 1 < sections.length then
    let scrollArea := sections[1]
    let visibleLines := state.tree.visibleLines
    let scrollbar := Scrollbar.vertical
        state.tree.selected
        visibleLines.length
        (inner.height - 2)  -- Account for status bar
      |>.withThumbStyle (Style.fgColor Color.cyan)
      |>.withTrackStyle Style.dim
    f := f.render scrollbar scrollArea

  -- Status bar at bottom
  let statusY := area.y + area.height - 1
  let statusText := if state.showPopup then
    "Y: Confirm | N: Cancel"
  else
    "↑↓: Navigate | Enter: Toggle | d: Delete | q: Quit"
  f := f.writeString (area.x + 2) statusY statusText Style.dim

  -- Show selected item info
  match state.tree.getSelected with
  | some line =>
    let infoText := s!"Selected: {line.label}"
    let infoX := area.x + area.width - infoText.length - 2
    f := f.writeString infoX statusY infoText (Style.dim.withFg Color.cyan)
  | none => pure ()

  -- Render popup if showing
  if state.showPopup then
    f := f.render state.confirmPopup area

  f

def update (state : FileExplorerState) (event : Option Event) : FileExplorerState × Bool :=
  match event with
  | none => (state, false)
  | some (.key k) =>
    -- Handle popup mode
    if state.showPopup then
      match k.code with
      | .char 'y' | .char 'Y' | .enter =>
        if state.confirmPopup.selectedYes then
          -- Delete action (just close popup for demo)
          ({ state with showPopup := false }, false)
        else
          ({ state with showPopup := false }, false)
      | .char 'n' | .char 'N' | .escape =>
        ({ state with showPopup := false }, false)
      | .left | .right | .tab =>
        ({ state with confirmPopup := state.confirmPopup.toggle }, false)
      | _ => (state, false)
    else
      match k.code with
      | .char 'q' => (state, true)
      | .up =>
        ({ state with tree := state.tree.selectPrev }, false)
      | .down =>
        ({ state with tree := state.tree.selectNext }, false)
      | .enter | .char ' ' =>
        ({ state with tree := state.tree.toggleSelected }, false)
      | .char 'd' | .char 'D' =>
        -- Show delete confirmation popup
        match state.tree.getSelected with
        | some line =>
          let popup := ConfirmPopup.new s!"Delete '{line.label}'?"
            |>.withYesLabel "Delete"
            |>.withNoLabel "Cancel"
            |>.selectNo  -- Default to Cancel for safety
          ({ state with showPopup := true, confirmPopup := popup }, false)
        | none => (state, false)
      | .left =>
        -- Collapse current node or move to parent
        match state.tree.getSelected with
        | some line =>
          if !line.isLeaf && line.isExpanded then
            ({ state with tree := state.tree.toggleSelected }, false)
          else
            (state, false)
        | none => (state, false)
      | .right =>
        -- Expand current node
        match state.tree.getSelected with
        | some line =>
          if !line.isLeaf && !line.isExpanded then
            ({ state with tree := state.tree.toggleSelected }, false)
          else
            (state, false)
        | none => (state, false)
      | _ =>
        if k.isCtrlC || k.isCtrlQ then (state, true)
        else (state, false)
  | _ => (state, false)

def main : IO Unit := do
  let initialState : FileExplorerState := {}
  App.runApp initialState draw update
