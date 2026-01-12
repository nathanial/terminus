/-
  Terminus Reactive - Grid Widget
  Fixed-size grid for game boards and cell-based displays.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive.Host

namespace Terminus.Reactive

/-! ## Grid Configuration -/

/-- Configuration for grid appearance. -/
structure GridConfig where
  /-- Characters per cell (width). Default 2 for visual balance. -/
  cellWidth : Nat := 2
  /-- Lines per cell (height). -/
  cellHeight : Nat := 1
  /-- Border type around the grid. -/
  borderType : BorderType := .none
  /-- Style for the border. -/
  borderStyle : Style := {}
  /-- Default style for cells. -/
  defaultStyle : Style := {}
  /-- Background character for empty cells. -/
  emptyChar : Char := ' '
  /-- Focus name for keyboard routing. -/
  focusName : String := ""
  deriving Inhabited

/-! ## Cell Types -/

/-- A cell in the grid with character and style. -/
structure GridCell where
  /-- Display characters (should match cellWidth). -/
  content : String
  /-- Style for this cell. -/
  style : Style := {}
  deriving Repr, Inhabited

namespace GridCell

/-- Create an empty cell. -/
def empty (width : Nat := 2) : GridCell :=
  { content := String.ofList (List.replicate width ' '), style := {} }

/-- Create a cell with a single character repeated. -/
def filled (c : Char) (width : Nat := 2) (style : Style := {}) : GridCell :=
  { content := String.ofList (List.replicate width c), style }

/-- Create a cell with content and style. -/
def mk' (content : String) (style : Style := {}) : GridCell :=
  { content, style }

end GridCell

/-! ## Grid Rendering Helper -/

private def renderGrid (width height : Nat) (config : GridConfig)
    (cellAt : Nat → Nat → GridCell) : RNode := Id.run do
  if width == 0 || height == 0 then
    return RNode.empty
  else
    let cellW := config.cellWidth
    let cellH := config.cellHeight

    -- Calculate total dimensions
    let borderOffset := if config.borderType != .none then 1 else 0
    let totalWidth := width * cellW + borderOffset * 2

    let mut rows : Array RNode := #[]

    -- Top border
    if config.borderType != .none then
      let chars := BorderChars.fromType config.borderType
      let topBorder := String.singleton chars.topLeft ++
        String.ofList (List.replicate (totalWidth - 2) chars.horizontal) ++
        String.singleton chars.topRight
      rows := rows.push (RNode.text topBorder config.borderStyle)

    -- Grid rows
    for y in [:height] do
      for _cellRow in [:cellH] do
        -- Cells in this row
        let mut cells : Array RNode := #[]
        if config.borderType != .none then
          let chars := BorderChars.fromType config.borderType
          cells := cells.push (RNode.text (String.singleton chars.vertical) config.borderStyle)

        for x in [:width] do
          let cell := cellAt x y
          cells := cells.push (RNode.text cell.content cell.style)

        -- Right border
        if config.borderType != .none then
          let chars := BorderChars.fromType config.borderType
          cells := cells.push (RNode.text (String.singleton chars.vertical) config.borderStyle)

        rows := rows.push (RNode.row 0 {} cells)

    -- Bottom border
    if config.borderType != .none then
      let chars := BorderChars.fromType config.borderType
      let bottomBorder := String.singleton chars.bottomLeft ++
        String.ofList (List.replicate (totalWidth - 2) chars.horizontal) ++
        String.singleton chars.bottomRight
      rows := rows.push (RNode.text bottomBorder config.borderStyle)

    return RNode.column 0 {} rows

/-! ## Grid Result -/

/-- Result returned by grid widget for cursor-enabled grids. -/
structure GridResult where
  /-- Current cursor position (x, y). -/
  cursorPos : Reactive.Dynamic Spider (Nat × Nat)
  /-- Event fired when cursor moves. -/
  onCursorMove : Reactive.Event Spider (Nat × Nat)
  /-- Event fired when Enter is pressed on a cell. -/
  onSelect : Reactive.Event Spider (Nat × Nat)

/-! ## Grid Widget -/

/-- Create a static grid widget.
    The renderCell function is called for each cell position. -/
def grid' (width height : Nat)
    (renderCell : Nat → Nat → GridCell)
    (config : GridConfig := {}) : WidgetM Unit := do
  emitStatic (renderGrid width height config renderCell)

/-- Create a grid with dynamic cell content.
    The grid re-renders when the cell renderer function changes. -/
def dynGrid' (width height : Nat)
    (cellContent : Reactive.Dynamic Spider (Nat → Nat → GridCell))
    (config : GridConfig := {}) : WidgetM Unit := do
  let node ← cellContent.map' (fun renderFn =>
    renderGrid width height config renderFn
  )
  emit node

/-- Create a grid with cursor navigation support.
    Returns a GridResult with cursor position and events. -/
def cursorGrid' (width height : Nat)
    (renderCell : Nat → Nat → Bool → GridCell)  -- x, y, isCursor -> cell
    (config : GridConfig := {}) : WidgetM GridResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "cursorGrid" (isInput := true)
    (nameOverride := config.focusName)

  -- Determine focus name before calling useFocusedKeyEventsW
  let gridName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW gridName

  -- Create trigger events
  let (moveEvent, fireMove) ← Reactive.newTriggerEvent (t := Spider) (a := Nat × Nat)
  let (selectEvent, fireSelect) ← Reactive.newTriggerEvent (t := Spider) (a := Nat × Nat)
  let (posEvent, firePos) ← Reactive.newTriggerEvent (t := Spider) (a := Nat × Nat)

  -- Track cursor position
  let initialPos := (0, 0)
  let posRef ← SpiderM.liftIO (IO.mkRef initialPos)

  -- Create dynamics
  let cursorPosDyn ← Reactive.holdDyn initialPos posEvent

  -- Subscribe to key events
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    if width == 0 || height == 0 then pure ()
    else
      let (cx, cy) ← posRef.get
      let ke := kd.event

      let newPos ← match ke.code with
        | .up | .char 'k' =>
          pure (cx, if cy > 0 then cy - 1 else cy)
        | .down | .char 'j' =>
          pure (cx, if cy < height - 1 then cy + 1 else cy)
        | .left | .char 'h' =>
          pure (if cx > 0 then cx - 1 else cx, cy)
        | .right | .char 'l' =>
          pure (if cx < width - 1 then cx + 1 else cx, cy)
        | .home =>
          pure (0, cy)
        | .end =>
          pure (width - 1, cy)
        | .enter | .space =>
          fireSelect (cx, cy)
          pure (cx, cy)
        | _ => pure (cx, cy)

      if newPos != (cx, cy) then
        posRef.set newPos
        firePos newPos
        fireMove newPos

  -- Emit render function
  let node ← cursorPosDyn.map' (fun (cursorX, cursorY) =>
    renderGrid width height config (fun x y =>
      renderCell x y (x == cursorX && y == cursorY)
    )
  )
  emit node

  pure {
    cursorPos := cursorPosDyn
    onCursorMove := moveEvent
    onSelect := selectEvent
  }

/-! ## Convenience Functions -/

/-- Create a simple character grid from a 2D array. -/
def charGrid' (cells : Array (Array (Char × Style)))
    (config : GridConfig := {}) : WidgetM Unit := do
  let height := cells.size
  let width := if height > 0 then cells[0]!.size else 0
  grid' width height (fun x y =>
    if h : y < cells.size then
      let row := cells[y]
      if h2 : x < row.size then
        let (c, style) := row[x]
        let content := String.ofList (List.replicate config.cellWidth c)
        { content, style }
      else
        GridCell.empty config.cellWidth
    else
      GridCell.empty config.cellWidth
  ) config

/-- Create a colored block grid (like Tetris). Each cell is either a color or empty. -/
def blockGrid' (cells : Array (Array (Option Color)))
    (emptyStyle : Style := {})
    (config : GridConfig := {}) : WidgetM Unit := do
  let height := cells.size
  let width := if height > 0 then cells[0]!.size else 0
  grid' width height (fun x y =>
    if h : y < cells.size then
      let row := cells[y]
      if h2 : x < row.size then
        match row[x] with
        | some color =>
          let content := "██"
          let style : Style := { fg := color }
          { content, style }
        | none =>
          let content := String.ofList (List.replicate config.cellWidth config.emptyChar)
          { content, style := emptyStyle }
      else
        GridCell.empty config.cellWidth
    else
      GridCell.empty config.cellWidth
  ) config

/-- Highlight a specific cell in a grid. -/
def highlightedGrid' (width height : Nat)
    (highlightPos : Reactive.Dynamic Spider (Nat × Nat))
    (renderCell : Nat → Nat → GridCell)
    (highlightStyle : Style := { bg := .ansi .cyan })
    (config : GridConfig := {}) : WidgetM Unit := do
  let node ← highlightPos.map' (fun (hx, hy) =>
    renderGrid width height config (fun x y =>
      let cell := renderCell x y
      let isHighlight := x == hx && y == hy
      let style := if isHighlight then
        { cell.style with bg := highlightStyle.bg }
      else
        cell.style
      { cell with style }
    )
  )
  emit node

end Terminus.Reactive
