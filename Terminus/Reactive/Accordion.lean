/-
  Terminus Reactive - Accordion Component
  Collapsible sections with headers and keyboard navigation.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Accordion Section -/

/-- A section in the accordion with title, content, and initial state. -/
structure AccordionSection where
  /-- Title displayed in the header. -/
  title : String
  /-- Widget content to render when expanded. -/
  content : WidgetM Unit
  /-- Whether this section starts expanded. -/
  initiallyOpen : Bool := false
  deriving Inhabited

/-! ## Accordion Configuration -/

/-- Configuration for accordion appearance and behavior. -/
structure AccordionConfig where
  /-- Allow multiple sections to be open simultaneously. -/
  allowMultiple : Bool := true
  /-- Icon shown for expanded sections. -/
  expandedIcon : String := "▼ "
  /-- Icon shown for collapsed sections. -/
  collapsedIcon : String := "▶ "
  /-- Style for section headers. -/
  headerStyle : Style := { modifier := { bold := true } }
  /-- Style for focused section header. -/
  focusedHeaderStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Indentation for content (in characters). -/
  contentIndent : Nat := 2
  /-- Gap between sections (in lines). -/
  gap : Nat := 0
  /-- Focus name for this accordion. Empty = auto-generated. -/
  focusName : String := ""
  /-- Whether accordion responds to keys without focus. -/
  globalKeys : Bool := false
  deriving Inhabited

/-! ## Accordion Result -/

/-- Result returned by accordion' containing reactive values and events. -/
structure AccordionResult where
  /-- Dynamic array of which sections are open. -/
  openSections : Reactive.Dynamic Spider (Array Bool)
  /-- Currently focused section index (none if no focus). -/
  focusedSection : Reactive.Dynamic Spider (Option Nat)
  /-- Event fired when a section is toggled (carries the section index). -/
  onToggle : Reactive.Event Spider Nat

/-! ## Accordion State -/

/-- Internal state for accordion navigation. -/
structure AccordionState where
  /-- Which sections are currently open. -/
  openStates : Array Bool := #[]
  /-- Currently focused section index. -/
  focusedIndex : Nat := 0
  deriving Repr, Inhabited, BEq

namespace AccordionState

/-- Move focus to previous section. -/
def focusUp (s : AccordionState) (sectionCount : Nat) : AccordionState :=
  if sectionCount == 0 || s.focusedIndex == 0 then s
  else { s with focusedIndex := s.focusedIndex - 1 }

/-- Move focus to next section. -/
def focusDown (s : AccordionState) (sectionCount : Nat) : AccordionState :=
  if sectionCount == 0 || s.focusedIndex >= sectionCount - 1 then s
  else { s with focusedIndex := s.focusedIndex + 1 }

/-- Jump to first section. -/
def focusFirst (s : AccordionState) : AccordionState :=
  { s with focusedIndex := 0 }

/-- Jump to last section. -/
def focusLast (s : AccordionState) (sectionCount : Nat) : AccordionState :=
  if sectionCount == 0 then s
  else { s with focusedIndex := sectionCount - 1 }

/-- Toggle a section's open state. -/
def toggleSection (s : AccordionState) (idx : Nat) (allowMultiple : Bool) : AccordionState :=
  if h : idx < s.openStates.size then
    let currentlyOpen := s.openStates[idx]
    if allowMultiple then
      -- Simply toggle the target section
      { s with openStates := s.openStates.set idx !currentlyOpen }
    else
      -- Single expand mode: close all others, toggle target
      let newStates := s.openStates.mapIdx fun i isOpen =>
        if i == idx then !currentlyOpen else false
      { s with openStates := newStates }
  else s

/-- Open a section (without toggling). -/
def openSection (s : AccordionState) (idx : Nat) (allowMultiple : Bool) : AccordionState :=
  if h : idx < s.openStates.size then
    if allowMultiple then
      { s with openStates := s.openStates.set idx true }
    else
      -- Close all others, open target
      let newStates := s.openStates.mapIdx fun i _ => i == idx
      { s with openStates := newStates }
  else s

/-- Close a section. -/
def closeSection (s : AccordionState) (idx : Nat) : AccordionState :=
  if h : idx < s.openStates.size then
    { s with openStates := s.openStates.set idx false }
  else s

end AccordionState

/-! ## Accordion Rendering Helper -/

/-- Render the accordion view given current state and content nodes. -/
private def renderAccordion (sections : Array AccordionSection) (config : AccordionConfig)
    (state : AccordionState) (contentNodes : List RNode) : RNode := Id.run do
  if sections.isEmpty then
    return RNode.text "(no sections)" config.headerStyle
  else
    let mut rows : Array RNode := #[]

    for h : i in [:sections.size] do
      let sec := sections[i]
      let isFocused := i == state.focusedIndex
      let isOpen := if hi : i < state.openStates.size then
        state.openStates[i]
      else false

      -- Add gap between sections (except before first)
      if i > 0 && config.gap > 0 then
        for _ in [:config.gap] do
          rows := rows.push RNode.empty

      -- Render header
      let icon := if isOpen then config.expandedIcon else config.collapsedIcon
      let headerText := icon ++ sec.title
      let headerStyle := if isFocused then config.focusedHeaderStyle else config.headerStyle
      rows := rows.push (RNode.text headerText headerStyle)

      -- Render content if open
      if isOpen then
        match contentNodes[i]? with
        | some contentNode =>
          -- Apply content indent
          if config.contentIndent > 0 then
            let indentStr := String.ofList (List.replicate config.contentIndent ' ')
            let indentedContent := RNode.row 0 {} #[RNode.text indentStr {}, contentNode]
            rows := rows.push indentedContent
          else
            rows := rows.push contentNode
        | none => pure ()

    return RNode.column 0 {} rows

/-! ## Accordion Widget -/

/-- Internal operation type for accordion state transitions. -/
private inductive AccordionOp
  | focusUp (sectionCount : Nat)
  | focusDown (sectionCount : Nat)
  | focusFirst
  | focusLast (sectionCount : Nat)
  | toggle (allowMultiple : Bool)
  | collapse
  | expand (allowMultiple : Bool)
  deriving Repr, Inhabited

/-- Apply an accordion operation to state, returning new state and optional toggle index. -/
private def AccordionOp.apply (op : AccordionOp) (state : AccordionState)
    : AccordionState × Option Nat :=
  match op with
  | .focusUp sectionCount =>
    let newState := state.focusUp sectionCount
    (newState, none)
  | .focusDown sectionCount =>
    let newState := state.focusDown sectionCount
    (newState, none)
  | .focusFirst =>
    let newState := state.focusFirst
    (newState, none)
  | .focusLast sectionCount =>
    let newState := state.focusLast sectionCount
    (newState, none)
  | .toggle allowMultiple =>
    let newState := state.toggleSection state.focusedIndex allowMultiple
    (newState, some state.focusedIndex)
  | .collapse =>
    if h : state.focusedIndex < state.openStates.size then
      if state.openStates[state.focusedIndex] then
        let newState := state.closeSection state.focusedIndex
        (newState, some state.focusedIndex)
      else (state, none)
    else (state, none)
  | .expand allowMultiple =>
    if h : state.focusedIndex < state.openStates.size then
      if !state.openStates[state.focusedIndex] then
        let newState := state.openSection state.focusedIndex allowMultiple
        (newState, some state.focusedIndex)
      else (state, none)
    else (state, none)

/-- Create an accordion widget with collapsible sections.

    The widget handles:
    - Up/Down (j/k) for navigating between section headers
    - Enter/Space to toggle the focused section
    - Left to collapse focused section
    - Right to expand focused section
    - Home/End to jump to first/last section

    Example:
    ```
    let sections := #[
      { title := "Section 1", content := text' "Content 1" {}, initiallyOpen := true },
      { title := "Section 2", content := text' "Content 2" {}, initiallyOpen := false },
      { title := "Section 3", content := text' "Content 3" {}, initiallyOpen := false }
    ]
    let accordion ← accordion' sections {}
    -- Use accordion.openSections to get current open states
    -- Use accordion.onToggle to handle toggle events
    ```
-/
def accordion' (sections : Array AccordionSection) (config : AccordionConfig := {})
    : WidgetM AccordionResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "accordion" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine the accordion's focus name
  let accordionName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW accordionName config.globalKeys

  -- Initialize state
  let initialOpenStates := sections.map (·.initiallyOpen)
  let initialState : AccordionState := {
    openStates := initialOpenStates
    focusedIndex := 0
  }
  let sectionCount := sections.size

  -- Map key events to state operations
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    if sections.isEmpty then none
    else
      match kd.event.code with
      | .up | .char 'k' => some (AccordionOp.focusUp sectionCount)
      | .down | .char 'j' => some (AccordionOp.focusDown sectionCount)
      | .home => some AccordionOp.focusFirst
      | .end => some (AccordionOp.focusLast sectionCount)
      | .enter | .space => some (AccordionOp.toggle config.allowMultiple)
      | .left => some AccordionOp.collapse
      | .right => some (AccordionOp.expand config.allowMultiple)
      | _ => none) keyEvents

  -- Fold state operations with toggle tracking
  -- We track (state, lastToggle) where lastToggle is the index that was toggled (if any)
  let stateWithToggleDyn ← foldDyn
    (fun (op : AccordionOp) (state : AccordionState × Option Nat) =>
      let (newState, toggleIdx) := op.apply state.1
      (newState, toggleIdx))
    (initialState, none)
    stateOps

  -- Derive the state dynamic
  let stateDyn ← stateWithToggleDyn.map' (·.1)

  -- Derive output dynamics from state
  let openSectionsDyn ← stateDyn.map' (·.openStates)
  let focusedSectionDyn ← stateDyn.map' fun (state : AccordionState) =>
    if sections.isEmpty then none else some state.focusedIndex

  -- Derive toggle event from state updates (filter for when toggleIdx is some)
  let toggleEvent ← Event.mapMaybeM (fun (_, toggleIdx) => toggleIdx)
    stateWithToggleDyn.updated

  -- Build content renders for each section (done once at setup)
  let mut contentRenders : Array ComponentRender := #[]
  for sec in sections do
    let (_, render) ← runWidgetChildren sec.content
    let childrenList ← Reactive.Dynamic.sequence render.toList
    let secRender ← childrenList.map' fun kids =>
      if kids.isEmpty then RNode.empty
      else RNode.column 0 {} kids.toArray
    contentRenders := contentRenders.push secRender

  -- Sequence the content renders into a single dynamic
  let contentsDyn ← Reactive.Dynamic.sequence contentRenders.toList

  -- Render the accordion
  let node ← stateDyn.zipWith' (fun state contentNodes =>
    renderAccordion sections config state contentNodes
  ) contentsDyn

  emit node

  pure {
    openSections := openSectionsDyn
    focusedSection := focusedSectionDyn
    onToggle := toggleEvent
  }

/-- Create an accordion with simple string content for each section.

    Example:
    ```
    let sections := #[
      ("Getting Started", "Welcome to the app..."),
      ("Features", "Key features include..."),
      ("FAQ", "Frequently asked questions...")
    ]
    let accordion ← simpleAccordion' sections {}
    ```
-/
def simpleAccordion' (sections : Array (String × String)) (config : AccordionConfig := {})
    : WidgetM AccordionResult := do
  let accordionSections := sections.map fun (title, content) => {
    title := title
    content := text' content {}
    initiallyOpen := false : AccordionSection
  }
  accordion' accordionSections config

/-- Create an accordion where the first section is initially open.

    Example:
    ```
    let sections := #[
      { title := "Open by default", content := text' "This is visible" {}, initiallyOpen := true },
      { title := "Closed", content := text' "Click to expand" {} }
    ]
    let accordion ← accordion' sections {}
    ```
-/
def singleExpandAccordion' (sections : Array AccordionSection) (config : AccordionConfig := {})
    : WidgetM AccordionResult :=
  accordion' sections { config with allowMultiple := false }

end Terminus.Reactive
