/-
  Terminus Reactive - Table Widget
  Data table with row selection and scrolling.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Table Cell and Row -/

/-- A single table cell. -/
structure TableCell' where
  /-- Cell content. -/
  content : String
  /-- Custom style for this cell. -/
  style : Style := {}
  deriving Repr, Inhabited

namespace TableCell'

/-- Create a simple cell. -/
def new (s : String) : TableCell' := { content := s }

/-- Create a styled cell. -/
def styled (s : String) (style : Style) : TableCell' := { content := s, style }

end TableCell'

/-- A table row. -/
structure TableRow' where
  /-- Cells in this row. -/
  cells : Array TableCell'
  deriving Repr, Inhabited

namespace TableRow'

/-- Create a row from strings. -/
def new (cells : Array String) : TableRow' := {
  cells := cells.map TableCell'.new
}

/-- Create a row from cells. -/
def fromCells (cells : Array TableCell') : TableRow' := { cells }

end TableRow'

/-! ## Column Configuration -/

/-- Column width specification. -/
inductive ColumnWidth' where
  /-- Fixed width in characters. -/
  | fixed (width : Nat)
  /-- Percentage of available width. -/
  | percent (pct : Nat)
  /-- Proportional ratio (relative to other ratio columns). -/
  | ratio (n : Nat)
  /-- Fill remaining space. -/
  | fill
  deriving Repr, BEq, Inhabited

/-- Column definition. -/
structure TableColumn' where
  /-- Column header text. -/
  header : String
  /-- Width specification. -/
  width : ColumnWidth' := .fill
  /-- Header style. -/
  headerStyle : Style := {}
  /-- Cell alignment (not yet implemented, reserved). -/
  align : String := "left"
  deriving Repr, Inhabited

/-! ## Table Configuration -/

/-- Configuration for table appearance and behavior. -/
structure TableConfig where
  /-- Style for header row. -/
  headerStyle : Style := { modifier := { bold := true } }
  /-- Style for selected row. -/
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Style for normal rows. -/
  normalStyle : Style := {}
  /-- Style for alternating rows (if enabled). -/
  alternateStyle : Style := { bg := .ansi .brightBlack }
  /-- Whether to use alternating row colors. -/
  useAlternateColors : Bool := false
  /-- Spacing between columns. -/
  columnSpacing : Nat := 1
  /-- Whether to show the header row. -/
  showHeader : Bool := true
  /-- Maximum visible rows (none = show all). -/
  maxHeight : Option Nat := none
  /-- Focus name for keyboard input. -/
  focusName : String := "table"
  /-- Whether to use global key handling (ignores focus). -/
  globalKeys : Bool := false
  deriving Repr, Inhabited

/-! ## Table State -/

/-- Internal table state. -/
structure TableState where
  /-- Currently selected row index (if any). -/
  selectedIndex : Option Nat := none
  /-- Scroll offset for large tables. -/
  scrollOffset : Nat := 0
  deriving Repr, Inhabited

namespace TableState

/-- Navigate to next row. -/
def navigateNext (state : TableState) (rowCount : Nat) (maxVisible : Option Nat) : TableState :=
  if rowCount == 0 then state
  else
    let current := state.selectedIndex.getD 0
    let next := if current + 1 >= rowCount then current else current + 1
    let newOffset := match maxVisible with
      | some max =>
        if next >= state.scrollOffset + max then
          min next (rowCount - min max rowCount)
        else state.scrollOffset
      | none => state.scrollOffset
    { selectedIndex := some next, scrollOffset := newOffset }

/-- Navigate to previous row. -/
def navigatePrev (state : TableState) (rowCount : Nat) : TableState :=
  if rowCount == 0 then state
  else
    let current := state.selectedIndex.getD 0
    let prev := if current == 0 then 0 else current - 1
    let newOffset := if prev < state.scrollOffset then prev else state.scrollOffset
    { selectedIndex := some prev, scrollOffset := newOffset }

/-- Clamp selection to valid range. -/
def clampSelection (state : TableState) (rowCount : Nat) : TableState :=
  if rowCount == 0 then { state with selectedIndex := none, scrollOffset := 0 }
  else match state.selectedIndex with
    | none => state
    | some idx =>
      let clamped := min idx (rowCount - 1)
      { state with
        selectedIndex := some clamped
        scrollOffset := min state.scrollOffset clamped }

end TableState

/-! ## Table Result -/

/-- Result returned by table widget. -/
structure TableResult where
  /-- Currently selected row index. -/
  selectedIndex : Reactive.Dynamic Spider (Option Nat)
  /-- Currently selected row data. -/
  selectedRow : Reactive.Dynamic Spider (Option TableRow')
  /-- Event fired when a row is selected (Enter pressed). -/
  onSelect : Reactive.Event Spider (Nat × TableRow')

/-! ## Table Widget -/

/-- Compute column widths based on specifications. -/
private def computeColumnWidths (widths : Array ColumnWidth') (numCols totalWidth spacing : Nat) : Array Nat := Id.run do
  if numCols == 0 then return #[]

  let totalSpacing := if numCols > 1 then spacing * (numCols - 1) else 0
  let available := if totalWidth > totalSpacing then totalWidth - totalSpacing else 0

  -- If no width specs, distribute evenly
  if widths.isEmpty then
    let colWidth := available / numCols
    return Array.replicate numCols colWidth

  let mut baseSizes : Array Nat := #[]
  let mut weights : Array Nat := #[]
  let mut fixedTotal : Nat := 0

  for i in [:numCols] do
    let w := widths.getD i .fill
    match w with
    | .fixed size =>
      baseSizes := baseSizes.push (min size available)
      weights := weights.push 0
      fixedTotal := fixedTotal + min size available
    | .percent pct =>
      let desired := (available * (min pct 100)) / 100
      baseSizes := baseSizes.push desired
      weights := weights.push 0
      fixedTotal := fixedTotal + desired
    | .ratio n =>
      baseSizes := baseSizes.push 0
      weights := weights.push n
    | .fill =>
      baseSizes := baseSizes.push 0
      weights := weights.push 1

  -- Distribute remaining space
  let remaining := if available > fixedTotal then available - fixedTotal else 0
  let weightSum := weights.foldl (· + ·) 0

  let mut result : Array Nat := #[]
  for i in [:numCols] do
    let base := baseSizes.getD i 0
    let w := weights.getD i 0
    let size := if w == 0 || weightSum == 0 then base else (remaining * w) / weightSum
    result := result.push size

  return result

/-- Render a table row. -/
private def renderTableRow (row : TableRow') (colWidths : Array Nat) (rowStyle : Style) (spacing : Nat) : RNode := Id.run do
  let mut parts : Array RNode := #[]

  for i in [:colWidths.size] do
    let width := colWidths.getD i 0
    let cell := row.cells.getD i (TableCell'.new "")
    let cellStyle := Style.merge rowStyle cell.style

    -- Truncate or pad content to width
    let content := cell.content
    let displayed := if content.length > width then
      content.take (width - 1) ++ "…"
    else
      content ++ String.mk (List.replicate (width - content.length) ' ')

    parts := parts.push (RNode.text displayed cellStyle)

    -- Add spacing between columns (except after last)
    if i + 1 < colWidths.size && spacing > 0 then
      parts := parts.push (RNode.text (String.mk (List.replicate spacing ' ')) {})

  return RNode.row 0 {} parts

/-- Create a table widget with static rows.

    The table supports keyboard navigation:
    - Up/Down arrows: Navigate rows
    - Enter: Select current row

    Example:
    ```
    let columns := #[
      { header := "Name", width := .fixed 20 },
      { header := "Age", width := .fixed 5 },
      { header := "Email", width := .fill }
    ]
    let rows := #[
      TableRow'.new #["Alice", "30", "alice@example.com"],
      TableRow'.new #["Bob", "25", "bob@example.com"]
    ]
    let table ← table' "data-table" columns rows {}
    ```
-/
def table' (name : String) (columns : Array TableColumn') (rows : Array TableRow')
    (config : TableConfig := {}) : WidgetM TableResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW name (isInput := true) (nameOverride := name)

  -- Get focused state
  let focusedInput ← useFocusedInputW

  -- State
  let stateRef ← SpiderM.liftIO (IO.mkRef (TableState.mk (if rows.isEmpty then none else some 0) 0))

  -- Events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Nat × TableRow')
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Option Nat)
  let (rowEvent, fireRow) ← newTriggerEvent (t := Spider) (a := Option TableRow')

  -- Initial dynamics
  let initialIdx := if rows.isEmpty then none else some 0
  let indexDyn ← holdDyn initialIdx indexEvent
  let rowDyn ← holdDyn (rows[0]?) rowEvent

  -- Subscribe to key events
  let events ← getEventsW
  let inputName := if name.isEmpty then widgetName else name

  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    let currentFocus ← focusedInput.sample
    let isFocused := currentFocus == some inputName

    if !config.globalKeys && !isFocused then return

    let state ← stateRef.get

    match kd.event.code with
    | .down | .char 'j' =>
      let newState := state.navigateNext rows.size config.maxHeight
      stateRef.set newState
      fireIndex newState.selectedIndex
      fireRow (newState.selectedIndex.bind fun i => rows[i]?)

    | .up | .char 'k' =>
      let newState := state.navigatePrev rows.size
      stateRef.set newState
      fireIndex newState.selectedIndex
      fireRow (newState.selectedIndex.bind fun i => rows[i]?)

    | .enter =>
      match state.selectedIndex with
      | some idx =>
        match rows[idx]? with
        | some row => fireSelect (idx, row)
        | none => pure ()
      | none => pure ()

    | _ => pure ()

  -- Render
  emit do
    let state ← stateRef.get

    if columns.isEmpty then
      pure RNode.empty
    else
      -- Calculate column widths (assume reasonable total width)
      let totalWidth := 80  -- Default assumption, could be made configurable
      let widths := columns.map (·.width)
      let colWidths := computeColumnWidths widths columns.size totalWidth config.columnSpacing

      let mut nodes : Array RNode := #[]

      -- Render header if enabled
      if config.showHeader then
        let headerRow := TableRow'.new (columns.map (·.header))
        nodes := nodes.push (renderTableRow headerRow colWidths config.headerStyle config.columnSpacing)

      -- Determine visible range
      let visibleRows := match config.maxHeight with
        | some max => min max rows.size
        | none => rows.size

      let startIdx := state.scrollOffset
      let endIdx := min (startIdx + visibleRows) rows.size

      -- Render visible rows
      for i in [startIdx : endIdx] do
        match rows[i]? with
        | some row =>
          let isSelected := state.selectedIndex == some i
          let isAlternate := config.useAlternateColors && i % 2 == 1

          let rowStyle := if isSelected then config.selectedStyle
                          else if isAlternate then config.alternateStyle
                          else config.normalStyle

          nodes := nodes.push (renderTableRow row colWidths rowStyle config.columnSpacing)
        | none => pure ()

      pure (RNode.column 0 {} nodes)

  pure {
    selectedIndex := indexDyn
    selectedRow := rowDyn
    onSelect := selectEvent
  }

/-- Create a table widget with dynamic rows.

    Similar to `table'` but the row data is a Dynamic that can change.
-/
def dynTable' (name : String) (columns : Array TableColumn')
    (rows : Reactive.Dynamic Spider (Array TableRow')) (config : TableConfig := {})
    : WidgetM TableResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW name (isInput := true) (nameOverride := name)

  -- Get focused state
  let focusedInput ← useFocusedInputW

  -- State
  let stateRef ← SpiderM.liftIO (IO.mkRef (TableState.mk none 0))

  -- Events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Nat × TableRow')
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Option Nat)
  let (rowEvent, fireRow) ← newTriggerEvent (t := Spider) (a := Option TableRow')

  -- Initial dynamics
  let indexDyn ← holdDyn none indexEvent
  let rowDyn ← holdDyn none rowEvent

  -- Subscribe to row changes to clamp selection
  let _unsub1 ← SpiderM.liftIO <| rows.updated.subscribe fun newRows => do
    let state ← stateRef.get
    let clamped := state.clampSelection newRows.size
    stateRef.set clamped
    fireIndex clamped.selectedIndex
    fireRow (clamped.selectedIndex.bind fun i => newRows[i]?)

  -- Subscribe to key events
  let events ← getEventsW
  let inputName := if name.isEmpty then widgetName else name

  let _unsub2 ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    let currentFocus ← focusedInput.sample
    let isFocused := currentFocus == some inputName

    if !config.globalKeys && !isFocused then return

    let currentRows ← rows.sample
    let state ← stateRef.get

    match kd.event.code with
    | .down | .char 'j' =>
      let newState := state.navigateNext currentRows.size config.maxHeight
      stateRef.set newState
      fireIndex newState.selectedIndex
      fireRow (newState.selectedIndex.bind fun i => currentRows[i]?)

    | .up | .char 'k' =>
      let newState := state.navigatePrev currentRows.size
      stateRef.set newState
      fireIndex newState.selectedIndex
      fireRow (newState.selectedIndex.bind fun i => currentRows[i]?)

    | .enter =>
      match state.selectedIndex with
      | some idx =>
        match currentRows[idx]? with
        | some row => fireSelect (idx, row)
        | none => pure ()
      | none => pure ()

    | _ => pure ()

  -- Render
  emitDynamic do
    let state ← stateRef.get
    let currentRows ← rows.sample

    if columns.isEmpty then
      pure RNode.empty
    else
      let totalWidth := 80
      let widths := columns.map (·.width)
      let colWidths := computeColumnWidths widths columns.size totalWidth config.columnSpacing

      let mut nodes : Array RNode := #[]

      if config.showHeader then
        let headerRow := TableRow'.new (columns.map (·.header))
        nodes := nodes.push (renderTableRow headerRow colWidths config.headerStyle config.columnSpacing)

      let visibleRows := match config.maxHeight with
        | some max => min max currentRows.size
        | none => currentRows.size

      let startIdx := state.scrollOffset
      let endIdx := min (startIdx + visibleRows) currentRows.size

      for i in [startIdx : endIdx] do
        match currentRows[i]? with
        | some row =>
          let isSelected := state.selectedIndex == some i
          let isAlternate := config.useAlternateColors && i % 2 == 1

          let rowStyle := if isSelected then config.selectedStyle
                          else if isAlternate then config.alternateStyle
                          else config.normalStyle

          nodes := nodes.push (renderTableRow row colWidths rowStyle config.columnSpacing)
        | none => pure ()

      pure (RNode.column 0 {} nodes)

  pure {
    selectedIndex := indexDyn
    selectedRow := rowDyn
    onSelect := selectEvent
  }

/-! ## Convenience Functions -/

/-- Create a simple table from string arrays. -/
def simpleTable' (name : String) (headers : Array String) (data : Array (Array String))
    (config : TableConfig := {}) : WidgetM TableResult := do
  let columns := headers.map fun h => { header := h, width := .fill : TableColumn' }
  let rows := data.map TableRow'.new
  table' name columns rows config

end Terminus.Reactive
