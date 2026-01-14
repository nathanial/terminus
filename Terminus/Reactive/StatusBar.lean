/-
  Terminus Reactive - Status Bar Widget
  A bottom status bar with configurable sections (like vim/vscode status bars).
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Status Bar Alignment -/

/-- Alignment for status bar sections. -/
inductive StatusBarAlign where
  | left
  | center
  | right
  deriving Repr, BEq, Inhabited

/-! ## Status Bar Section Types -/

/-- A static status bar section with fixed content. -/
structure StatusBarSection where
  /-- The text content of the section. -/
  content : String
  /-- Style for the section (overrides bar style if set). -/
  style : Style := {}
  /-- Minimum width for the section (for consistent sizing). -/
  minWidth : Option Nat := none
  /-- Alignment of this section in the status bar. -/
  align : StatusBarAlign := .left
  deriving Inhabited

/-- A dynamic status bar section with reactive content. -/
structure StatusBarSectionDyn where
  /-- Dynamic text content of the section. -/
  content : Dynamic Spider String
  /-- Style for the section (overrides bar style if set). -/
  style : Style := {}
  /-- Minimum width for the section (for consistent sizing). -/
  minWidth : Option Nat := none
  /-- Alignment of this section in the status bar. -/
  align : StatusBarAlign := .left

/-! ## Status Bar Configuration -/

/-- Configuration for the status bar appearance. -/
structure StatusBarConfig where
  /-- Base style for the entire status bar. -/
  style : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Separator character between sections. -/
  separatorChar : String := " \u2502 "  -- " │ "
  /-- Style for the separator. -/
  separatorStyle : Style := { fg := .ansi .brightBlack }
  /-- Left padding (spaces) for the bar. -/
  leftPadding : Nat := 1
  /-- Right padding (spaces) for the bar. -/
  rightPadding : Nat := 1
  deriving Inhabited

/-! ## Helper Functions -/

/-- Pad a string to a minimum width. -/
private def padToWidth (s : String) (minWidth : Option Nat) : String :=
  match minWidth with
  | none => s
  | some w =>
    let len := s.length
    if len >= w then s
    else s ++ String.ofList (List.replicate (w - len) ' ')

/-- Create padding spaces. -/
private def padSpaces (n : Nat) : String :=
  String.ofList (List.replicate n ' ')

/-- Merge section style with base style (section overrides non-default values). -/
private def mergeStyles (base sectionStyle : Style) : Style :=
  { fg := if sectionStyle.fg == .default then base.fg else sectionStyle.fg
    bg := if sectionStyle.bg == .default then base.bg else sectionStyle.bg
    modifier := Modifier.merge base.modifier sectionStyle.modifier }

/-- Render a single static section as an RNode. -/
private def renderSection (sect : StatusBarSection) (baseStyle : Style) : RNode :=
  let content := padToWidth sect.content sect.minWidth
  let style := mergeStyles baseStyle sect.style
  RNode.text content style

/-- Render a separator as an RNode. -/
private def renderSeparator (config : StatusBarConfig) : RNode :=
  let style := mergeStyles config.style config.separatorStyle
  RNode.text config.separatorChar style

/-- Partition sections by alignment. -/
private def partitionByAlign (sections : Array StatusBarSection)
    : Array StatusBarSection × Array StatusBarSection × Array StatusBarSection :=
  let left := sections.filter (·.align == .left)
  let center := sections.filter (·.align == .center)
  let right := sections.filter (·.align == .right)
  (left, center, right)

/-- Build an array of RNodes from sections with separators between them. -/
private def buildSectionNodes (sections : Array StatusBarSection)
    (config : StatusBarConfig) : Array RNode := Id.run do
  let mut nodes : Array RNode := #[]
  for i in [:sections.size] do
    if i > 0 then
      nodes := nodes.push (renderSeparator config)
    nodes := nodes.push (renderSection sections[i]! config.style)
  nodes

/-! ## Static Status Bar -/

/-- Create a static status bar with fixed sections.

    Sections are arranged based on their alignment:
    - Left-aligned sections appear at the start
    - Right-aligned sections appear at the end
    - Center-aligned sections appear in the middle

    Example:
    ```
    statusBar' #[
      { content := "NORMAL", style := { bg := .ansi .green } },
      { content := "main.lean" },
      { content := "Ln 42, Col 8", align := .right },
      { content := "UTF-8", align := .right }
    ] {}
    ```
-/
def statusBar' (sections : Array StatusBarSection)
    (config : StatusBarConfig := {}) : WidgetM Unit := do
  let (leftSections, centerSections, rightSections) := partitionByAlign sections

  let mut nodes : Array RNode := #[]

  -- Left padding
  if config.leftPadding > 0 then
    nodes := nodes.push (RNode.text (padSpaces config.leftPadding) config.style)

  -- Left-aligned sections
  let leftNodes := buildSectionNodes leftSections config
  nodes := nodes ++ leftNodes

  -- Separator before center (if there are left and center sections)
  if !leftSections.isEmpty && !centerSections.isEmpty then
    nodes := nodes.push (renderSeparator config)

  -- Center-aligned sections
  let centerNodes := buildSectionNodes centerSections config
  nodes := nodes ++ centerNodes

  -- Flexible spacer between left/center and right
  -- Use a spacer node that will expand to fill available space
  if !rightSections.isEmpty then
    if !leftSections.isEmpty || !centerSections.isEmpty then
      nodes := nodes.push (renderSeparator config)
    -- Add a flexible space (will be handled by layout)
    nodes := nodes.push (RNode.spacer 1 1)
    if !leftSections.isEmpty || !centerSections.isEmpty then
      nodes := nodes.push (renderSeparator config)

  -- Right-aligned sections
  let rightNodes := buildSectionNodes rightSections config
  nodes := nodes ++ rightNodes

  -- Right padding
  if config.rightPadding > 0 then
    nodes := nodes.push (RNode.text (padSpaces config.rightPadding) config.style)

  emitStatic (RNode.row 0 {} nodes)

/-! ## Dynamic Status Bar -/

/-- Get element from list by index, with default. -/
private def listGetD (xs : List α) (i : Nat) (default : α) : α :=
  xs.getD i default

/-- Create a status bar with dynamic sections.

    Each section's content is a Dynamic that can change over time.
    The status bar re-renders whenever any section content changes.

    Example:
    ```
    let modeDyn ← holdDyn "NORMAL" modeEvent
    let fileDyn ← holdDyn "untitled" fileEvent
    statusBarDyn' #[
      { content := modeDyn, style := { bg := .ansi .green } },
      { content := fileDyn }
    ] {}
    ```
-/
def statusBarDyn' (sections : Array StatusBarSectionDyn)
    (config : StatusBarConfig := {}) : WidgetM Unit := do
  -- Collect all dynamic contents into a list
  let contentDyns := sections.map (·.content)
  let contentsList ← Reactive.Dynamic.sequence contentDyns.toList

  -- Pre-extract the static metadata from sections (style, minWidth, align)
  let sectionMeta := sections.map fun s => (s.style, s.minWidth, s.align)

  let node ← contentsList.map' fun contents =>
    Id.run do
      -- Rebuild static sections from current content values
      let mut staticSections : Array StatusBarSection := #[]
      let contentArray := contents.toArray
      for i in [:sectionMeta.size] do
        if h : i < sectionMeta.size then
          let (style, minWidth, align) := sectionMeta[i]
          let content := contentArray.getD i ""
          staticSections := staticSections.push {
            content := content
            style := style
            minWidth := minWidth
            align := align
          }

      let (leftSections, centerSections, rightSections) := partitionByAlign staticSections

      let mut nodes : Array RNode := #[]

      -- Left padding
      if config.leftPadding > 0 then
        nodes := nodes.push (RNode.text (padSpaces config.leftPadding) config.style)

      -- Left-aligned sections
      let leftNodes := buildSectionNodes leftSections config
      nodes := nodes ++ leftNodes

      -- Separator before center (if there are left and center sections)
      if !leftSections.isEmpty && !centerSections.isEmpty then
        nodes := nodes.push (renderSeparator config)

      -- Center-aligned sections
      let centerNodes := buildSectionNodes centerSections config
      nodes := nodes ++ centerNodes

      -- Flexible spacer between left/center and right
      if !rightSections.isEmpty then
        if !leftSections.isEmpty || !centerSections.isEmpty then
          nodes := nodes.push (renderSeparator config)
        nodes := nodes.push (RNode.spacer 1 1)
        if !leftSections.isEmpty || !centerSections.isEmpty then
          nodes := nodes.push (renderSeparator config)

      -- Right-aligned sections
      let rightNodes := buildSectionNodes rightSections config
      nodes := nodes ++ rightNodes

      -- Right padding
      if config.rightPadding > 0 then
        nodes := nodes.push (RNode.text (padSpaces config.rightPadding) config.style)

      return RNode.row 0 {} nodes
  emit node

/-! ## Simple Two-Part Status Bar -/

/-- Create a simple two-part status bar (left and right).

    This is a convenience function for the common pattern of having
    content on the left and content on the right separated by flexible space.

    Example:
    ```
    simpleStatusBar' "main.lean - Modified" "Ln 42, Col 8" {}
    ```
-/
def simpleStatusBar' (left : String) (right : String)
    (config : StatusBarConfig := {}) : WidgetM Unit := do
  let mut nodes : Array RNode := #[]

  -- Left padding
  if config.leftPadding > 0 then
    nodes := nodes.push (RNode.text (padSpaces config.leftPadding) config.style)

  -- Left content
  nodes := nodes.push (RNode.text left config.style)

  -- Flexible spacer
  nodes := nodes.push (RNode.spacer 1 1)

  -- Right content
  nodes := nodes.push (RNode.text right config.style)

  -- Right padding
  if config.rightPadding > 0 then
    nodes := nodes.push (RNode.text (padSpaces config.rightPadding) config.style)

  emitStatic (RNode.row 0 {} nodes)

/-- Create a simple status bar with dynamic left/right content.

    Example:
    ```
    let leftDyn ← holdDyn "Ready" statusEvent
    let rightDyn ← holdDyn "12:00 PM" timeEvent
    simpleStatusBarDyn' leftDyn rightDyn {}
    ```
-/
def simpleStatusBarDyn' (left : Dynamic Spider String) (right : Dynamic Spider String)
    (config : StatusBarConfig := {}) : WidgetM Unit := do
  let node ← left.zipWith' (fun l r =>
    Id.run do
      let mut nodes : Array RNode := #[]

      -- Left padding
      if config.leftPadding > 0 then
        nodes := nodes.push (RNode.text (padSpaces config.leftPadding) config.style)

      -- Left content
      nodes := nodes.push (RNode.text l config.style)

      -- Flexible spacer
      nodes := nodes.push (RNode.spacer 1 1)

      -- Right content
      nodes := nodes.push (RNode.text r config.style)

      -- Right padding
      if config.rightPadding > 0 then
        nodes := nodes.push (RNode.text (padSpaces config.rightPadding) config.style)

      return RNode.row 0 {} nodes
  ) right
  emit node

/-! ## Themed Status Bar Helpers -/

/-- Create a mode indicator section (like vim's NORMAL/INSERT/VISUAL). -/
def modeSection (mode : String) (style : Style := { bg := .ansi .green, fg := .ansi .black })
    : StatusBarSection :=
  { content := mode, style := style, minWidth := some 8, align := .left }

/-- Create a dynamic mode indicator section. -/
def modeSectionDyn (mode : Dynamic Spider String)
    (style : Style := { bg := .ansi .green, fg := .ansi .black }) : StatusBarSectionDyn :=
  { content := mode, style := style, minWidth := some 8, align := .left }

/-- Create a file path section. -/
def fileSection (path : String) (modified : Bool := false) : StatusBarSection :=
  let indicator := if modified then " [+]" else ""
  { content := path ++ indicator, align := .left }

/-- Create a cursor position section (line and column). -/
def positionSection (line col : Nat) : StatusBarSection :=
  { content := s!"Ln {line}, Col {col}", align := .right }

/-- Create an encoding section (e.g., UTF-8). -/
def encodingSection (encoding : String := "UTF-8") : StatusBarSection :=
  { content := encoding, align := .right }

/-- Create a language/filetype section. -/
def languageSection (language : String) : StatusBarSection :=
  { content := language, align := .right }

/-- Create a time section. -/
def timeSection (time : String) : StatusBarSection :=
  { content := time, align := .right }

/-- Create a dynamic time section. -/
def timeSectionDyn (time : Dynamic Spider String) : StatusBarSectionDyn :=
  { content := time, align := .right }

/-! ## Pre-configured Status Bar Styles -/

/-- Vim-style status bar configuration (blue background). -/
def vimStatusBarConfig : StatusBarConfig := {
  style := { bg := .ansi .blue, fg := .ansi .white }
  separatorChar := " \u2502 "
  separatorStyle := { fg := .ansi .brightBlue }
  leftPadding := 1
  rightPadding := 1
}

/-- VS Code-style status bar configuration (darker background). -/
def vscodeStatusBarConfig : StatusBarConfig := {
  style := { bg := .ansi .brightBlack, fg := .ansi .white }
  separatorChar := "  "
  separatorStyle := {}
  leftPadding := 2
  rightPadding := 2
}

/-- Minimal status bar configuration (no separators). -/
def minimalStatusBarConfig : StatusBarConfig := {
  style := { fg := .ansi .brightBlack }
  separatorChar := "  "
  separatorStyle := {}
  leftPadding := 0
  rightPadding := 0
}

end Terminus.Reactive
