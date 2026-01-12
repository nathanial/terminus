/-
  Terminus Reactive - DataGrid Widget
  Spreadsheet-like grid with editable cells.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## DataGrid Configuration -/

/-- Configuration for data grid behavior and appearance. -/
structure DataGridConfig where
  /-- Optional title for the grid block. -/
  title : Option String := none
  /-- Optional column headers (fallback to A, B, C...). -/
  columnHeaders : Option (Array String) := none
  /-- Show column headers row. -/
  showColumnHeaders : Bool := true
  /-- Show row headers (1-based). -/
  showRowHeaders : Bool := true
  /-- Width of row header column. -/
  rowHeaderWidth : Nat := 4
  /-- Width of each cell. -/
  cellWidth : Nat := 8
  /-- Maximum visible rows (none = show all). -/
  maxVisibleRows : Option Nat := none
  /-- Maximum visible columns (none = show all). -/
  maxVisibleCols : Option Nat := none
  /-- Border type for the grid. -/
  borderType : BorderType := .rounded
  /-- Border style. -/
  borderStyle : Style := {}
  /-- Header style for column labels. -/
  headerStyle : Style := { modifier := { bold := true } }
  /-- Style for row header labels. -/
  rowHeaderStyle : Style := { fg := .ansi .brightBlack }
  /-- Default cell style. -/
  cellStyle : Style := {}
  /-- Style for selected cell. -/
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Style for cell while editing. -/
  editingStyle : Style := { bg := .ansi .cyan, fg := .ansi .black }
  /-- Focus name for keyboard routing. -/
  focusName : String := ""
  /-- Capture keys globally (ignores focus). -/
  globalKeys : Bool := false
  /-- Enable inline cell editing. -/
  editable : Bool := true
  deriving Repr, Inhabited

/-! ## DataGrid State -/

/-- Internal data grid state. -/
structure DataGridState where
  selectedRow : Nat := 0
  selectedCol : Nat := 0
  scrollRow : Nat := 0
  scrollCol : Nat := 0
  editing : Bool := false
  editBuffer : String := ""
  deriving Repr, Inhabited

/-! ## DataGrid Result -/

/-- Result returned by dataGrid'. -/
structure DataGridResult where
  /-- Current data as a Dynamic. -/
  data : Reactive.Dynamic Spider (Array (Array String))
  /-- Current selected cell position. -/
  selectedPos : Reactive.Dynamic Spider (Nat × Nat)
  /-- Whether the grid is in editing mode. -/
  editing : Reactive.Dynamic Spider Bool
  /-- Event fired when Enter/Space is pressed on a cell. -/
  onSelect : Reactive.Event Spider (Nat × Nat)
  /-- Event fired when a cell is edited (row, col, value). -/
  onEdit : Reactive.Event Spider (Nat × Nat × String)
  /-- Programmatically set a cell value. -/
  setCell : Nat → Nat → String → IO Unit

/-! ## Helpers -/

private def colCount (data : Array (Array String)) : Nat :=
  data.foldl (fun acc row => max acc row.size) 0

private def visibleCount (maxOpt : Option Nat) (total : Nat) : Nat :=
  match maxOpt with
  | some n =>
    if n == 0 then total else min n total
  | none => total

private def padRight (s : String) (width : Nat) : String :=
  let trimmed := if s.length > width then s.take width else s
  let pad := if trimmed.length < width then width - trimmed.length else 0
  trimmed ++ String.ofList (List.replicate pad ' ')

private def padLeft (s : String) (width : Nat) : String :=
  let trimmed :=
    if s.length > width then
      s.drop (s.length - width)
    else
      s
  let pad := if trimmed.length < width then width - trimmed.length else 0
  String.ofList (List.replicate pad ' ') ++ trimmed

private def colLabel (idx : Nat) : String :=
  if idx < 26 then
    String.singleton (Char.ofNat ('A'.toNat + idx))
  else
    toString (idx + 1)

private def getCell (data : Array (Array String)) (row col : Nat) : String :=
  if h : row < data.size then
    let rowData := data[row]
    if h2 : col < rowData.size then
      rowData[col]
    else
      ""
  else
    ""

private def updateCell (data : Array (Array String)) (row col : Nat) (value : String)
    : Array (Array String) := Id.run do
  if row >= data.size then
    return data
  let mut newRow := data[row]!
  if col < newRow.size then
    newRow := newRow.set! col value
  else
    for _ in [newRow.size:col] do
      newRow := newRow.push ""
    newRow := newRow.push value
  let mut newData := data
  newData := newData.set! row newRow
  return newData

private def clampSelection (state : DataGridState) (rowCount colCount : Nat) : DataGridState :=
  if rowCount == 0 || colCount == 0 then
    { state with selectedRow := 0, selectedCol := 0, editing := false, editBuffer := "" }
  else
    let row := min state.selectedRow (rowCount - 1)
    let col := min state.selectedCol (colCount - 1)
    { state with selectedRow := row, selectedCol := col }

private def adjustScroll (state : DataGridState)
    (rowCount colCount visibleRows visibleCols : Nat) : DataGridState :=
  if rowCount == 0 || colCount == 0 then
    { state with scrollRow := 0, scrollCol := 0 }
  else
    let scrollRow :=
      if rowCount <= visibleRows then 0
      else if state.selectedRow < state.scrollRow then state.selectedRow
      else if state.selectedRow >= state.scrollRow + visibleRows then
        state.selectedRow - visibleRows + 1
      else state.scrollRow
    let scrollCol :=
      if colCount <= visibleCols then 0
      else if state.selectedCol < state.scrollCol then state.selectedCol
      else if state.selectedCol >= state.scrollCol + visibleCols then
        state.selectedCol - visibleCols + 1
      else state.scrollCol
    let clampRow := if rowCount <= visibleRows then 0 else min scrollRow (rowCount - visibleRows)
    let clampCol := if colCount <= visibleCols then 0 else min scrollCol (colCount - visibleCols)
    { state with scrollRow := clampRow, scrollCol := clampCol }

/-! ## DataGrid Widget -/

/-- Create an editable data grid widget. -/
def dataGrid' (data : Array (Array String)) (config : DataGridConfig := {})
    : WidgetM DataGridResult := do
  -- Register for focus handling
  let widgetName ← registerComponentW "dataGrid" (isInput := true)
    (nameOverride := config.focusName)
  let gridName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW gridName config.globalKeys

  -- Compute initial state
  let initialRows := data.size
  let initialCols := colCount data
  let visibleRows := visibleCount config.maxVisibleRows initialRows
  let visibleCols := visibleCount config.maxVisibleCols initialCols
  let baseState : DataGridState := {}
  let initialState := adjustScroll (clampSelection baseState initialRows initialCols)
    initialRows initialCols visibleRows visibleCols

  let dataRef ← SpiderM.liftIO (IO.mkRef data)
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
  let (dataEvent, fireData) ← newTriggerEvent (t := Spider) (a := Array (Array String))
  let dataDyn ← holdDyn data dataEvent
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := DataGridState)
  let stateDyn ← holdDyn initialState stateEvent

  -- Events and dynamics
  let (posEvent, firePos) ← newTriggerEvent (t := Spider) (a := Nat × Nat)
  let posDyn ← holdDyn (initialState.selectedRow, initialState.selectedCol) posEvent
  let (editingEvent, fireEditing) ← newTriggerEvent (t := Spider) (a := Bool)
  let editingDyn ← holdDyn initialState.editing editingEvent
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Nat × Nat)
  let (editEvent, fireEdit) ← newTriggerEvent (t := Spider) (a := Nat × Nat × String)

  let env ← SpiderM.getEnv

  let updateState : DataGridState → IO Unit := fun newState => do
    stateRef.set newState
    fireState newState
    firePos (newState.selectedRow, newState.selectedCol)
    fireEditing newState.editing

  let setCellFn : Nat → Nat → String → IO Unit := fun row col value => do
    env.withFrame do
      let current ← dataRef.get
      let newData := updateCell current row col value
      dataRef.set newData
      fireData newData
      fireEdit (row, col, value)

  -- Handle key events
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
      let state ← stateRef.get
      let currentData ← dataRef.get
      let rows := currentData.size
      let cols := colCount currentData
      if rows == 0 || cols == 0 then
        pure ()
      else
        let visibleRows := visibleCount config.maxVisibleRows rows
        let visibleCols := visibleCount config.maxVisibleCols cols
        let ke := kd.event

        if state.editing then
          let buffer := state.editBuffer
          match ke.code with
          | .char c =>
            if c.val >= 32 then
              updateState { state with editBuffer := buffer ++ c.toString }
            else
              pure ()
          | .space =>
            updateState { state with editBuffer := buffer ++ " " }
          | .backspace =>
            updateState { state with editBuffer := buffer.dropRight 1 }
          | .enter =>
            let newData := updateCell currentData state.selectedRow state.selectedCol buffer
            dataRef.set newData
            fireData newData
            fireEdit (state.selectedRow, state.selectedCol, buffer)
            updateState { state with editing := false, editBuffer := "" }
          | .escape =>
            updateState { state with editing := false, editBuffer := "" }
          | _ => pure ()
        else
          let startEditWith : String → IO Unit := fun initial => do
            updateState { state with editing := true, editBuffer := initial }

          match ke.code with
          | .up =>
            let nextRow := if state.selectedRow > 0 then state.selectedRow - 1 else state.selectedRow
            let newState := adjustScroll { state with selectedRow := nextRow } rows cols visibleRows visibleCols
            updateState newState
          | .down =>
            let nextRow := if state.selectedRow + 1 < rows then state.selectedRow + 1 else state.selectedRow
            let newState := adjustScroll { state with selectedRow := nextRow } rows cols visibleRows visibleCols
            updateState newState
          | .left =>
            let nextCol := if state.selectedCol > 0 then state.selectedCol - 1 else state.selectedCol
            let newState := adjustScroll { state with selectedCol := nextCol } rows cols visibleRows visibleCols
            updateState newState
          | .right =>
            let nextCol := if state.selectedCol + 1 < cols then state.selectedCol + 1 else state.selectedCol
            let newState := adjustScroll { state with selectedCol := nextCol } rows cols visibleRows visibleCols
            updateState newState
          | .home =>
            let newState := adjustScroll { state with selectedCol := 0 } rows cols visibleRows visibleCols
            updateState newState
          | .end =>
            let newState := adjustScroll { state with selectedCol := cols - 1 } rows cols visibleRows visibleCols
            updateState newState
          | .pageUp =>
            let step := if visibleRows > 0 then visibleRows else 1
            let nextRow := if state.selectedRow >= step then state.selectedRow - step else 0
            let newState := adjustScroll { state with selectedRow := nextRow } rows cols visibleRows visibleCols
            updateState newState
          | .pageDown =>
            let step := if visibleRows > 0 then visibleRows else 1
            let nextRow := min (state.selectedRow + step) (rows - 1)
            let newState := adjustScroll { state with selectedRow := nextRow } rows cols visibleRows visibleCols
            updateState newState
          | .char c =>
            if c.val >= 32 && config.editable then
              startEditWith c.toString
            else
              pure ()
          | .backspace | .delete =>
            if config.editable then
              startEditWith ""
            else
              pure ()
          | .enter =>
            if config.editable then
              startEditWith (getCell currentData state.selectedRow state.selectedCol)
            else
              fireSelect (state.selectedRow, state.selectedCol)
          | .space =>
            fireSelect (state.selectedRow, state.selectedCol)
          | _ => pure ()

  let node ← stateDyn.zipWith' (fun state currentData =>
    Id.run do
      let rows := currentData.size
      let cols := colCount currentData

      if rows == 0 || cols == 0 then
        let emptyNode := RNode.text "(empty)" config.cellStyle
        if config.borderType == .none then
          return emptyNode
        else
          return RNode.block config.title config.borderType config.borderStyle none emptyNode
      else
        let visibleRows := visibleCount config.maxVisibleRows rows
        let visibleCols := visibleCount config.maxVisibleCols cols
        let rowHeaderWidth :=
          if config.showRowHeaders then
            max config.rowHeaderWidth (toString rows).length
          else 0
        let startRow := state.scrollRow
        let endRow := min (startRow + visibleRows) rows
        let startCol := state.scrollCol
        let endCol := min (startCol + visibleCols) cols

        let mut rowNodes : Array RNode := #[]

        if config.showColumnHeaders then
          let mut headerCells : Array RNode := #[]
          if config.showRowHeaders then
            headerCells := headerCells.push (RNode.text (padLeft "" rowHeaderWidth) config.rowHeaderStyle)
          for c in [startCol:endCol] do
            let label := match config.columnHeaders with
              | some headers => headers.getD c (colLabel c)
              | none => colLabel c
            headerCells := headerCells.push (RNode.text (padRight label config.cellWidth) config.headerStyle)
          rowNodes := rowNodes.push (RNode.row 1 {} headerCells)

        for r in [startRow:endRow] do
          let mut cells : Array RNode := #[]
          if config.showRowHeaders then
            let label := padLeft (toString (r + 1)) rowHeaderWidth
            cells := cells.push (RNode.text label config.rowHeaderStyle)

          for c in [startCol:endCol] do
            let isSelected := r == state.selectedRow && c == state.selectedCol
            let isEditing := isSelected && state.editing
            let content :=
              if isEditing then
                let buffer := state.editBuffer
                let display := if buffer.isEmpty then "|" else buffer ++ "|"
                padRight display config.cellWidth
              else
                padRight (getCell currentData r c) config.cellWidth
            let style := if isEditing then config.editingStyle
              else if isSelected then config.selectedStyle
              else config.cellStyle
            cells := cells.push (RNode.text content style)

          rowNodes := rowNodes.push (RNode.row 1 {} cells)

        let inner := RNode.column 0 {} rowNodes
        if config.borderType == .none then
          return inner
        else
          return RNode.block config.title config.borderType config.borderStyle none inner
  ) dataDyn
  emit node

  pure {
    data := dataDyn
    selectedPos := posDyn
    editing := editingDyn
    onSelect := selectEvent
    onEdit := editEvent
    setCell := setCellFn
  }

end Terminus.Reactive
