/-
  Reactive Game Demo Widgets
  Showcases Grid and Animation widgets with a simple game-like demo.
-/
import Terminus.Reactive

open Terminus Terminus.Reactive
open Reactive Reactive.Host

/-- Simple color palette for the game demo. -/
def gameColors : Array Color := #[
  Color.ansi .red,
  Color.ansi .green,
  Color.ansi .blue,
  Color.ansi .yellow,
  Color.ansi .magenta,
  Color.ansi .cyan
]

/-- Get a color from the palette by index. -/
def getColor (idx : Nat) : Color :=
  if h : idx % gameColors.size < gameColors.size then
    gameColors[idx % gameColors.size]
  else
    Color.ansi .white

def reactiveGameApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Game state
  let gridWidth := 8
  let gridHeight := 6

  -- Cell state: each cell has a color index (0-5) or none (empty)
  let emptyRow : Array (Option Nat) := Array.mkEmpty gridWidth |>.append (List.replicate gridWidth none).toArray
  let initialCells : Array (Array (Option Nat)) := Array.mkEmpty gridHeight |>.append (List.replicate gridHeight emptyRow).toArray
  let cellsRef ← SpiderM.liftIO (IO.mkRef initialCells)

  -- Score tracking
  let scoreRef ← SpiderM.liftIO (IO.mkRef (0 : Nat))
  let (scoreEvent, fireScore) ← newTriggerEvent (t := Spider) (a := Nat)
  let scoreDyn ← holdDyn 0 scoreEvent

  -- Animation state
  let (flashEvent, fireFlash) ← newTriggerEvent (t := Spider) (a := Unit)
  let flashAnim ← useAnimation { durationMs := 200 } flashEvent

  -- Pulse for highlight effect
  let pulse ← usePulse 500

  -- Cycle for color cycling demo
  let colorCycle ← useCycle 3000

  -- Subscribe to key events for game controls
  let keyEvents ← useKeyEvent
  let events ← getEvents

  -- Set initial focus
  SpiderM.liftIO <| events.registry.fireFocus (some "game-grid")

  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    match kd.event.code with
    | .char 'r' =>
      -- Reset grid
      cellsRef.set initialCells
      scoreRef.set 0
      fireScore 0
    | .char 'f' =>
      -- Fill a random cell
      let cells ← cellsRef.get
      let x := (← IO.rand 0 (gridWidth - 1))
      let y := (← IO.rand 0 (gridHeight - 1))
      let colorIdx := (← IO.rand 0 (gameColors.size - 1))
      if h : y < cells.size then
        let row := cells[y]
        if h2 : x < row.size then
          let newRow := row.set x (some colorIdx)
          let newCells := cells.set y newRow
          cellsRef.set newCells
          let s ← scoreRef.get
          scoreRef.set (s + 10)
          fireScore (s + 10)
          fireFlash ()
    | _ => pure ()

  -- Build UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Game Demo ===" theme.heading1Style
      text' "R: reset | F: fill cell | Arrows: move cursor | Enter: select | Ctrl+C: quit" theme.captionStyle

      row' (gap := 2) {} do
        -- Left panel: Game grid with cursor
        column' (gap := 1) {} do
          titledBlock' "Game Grid" .rounded theme do
            let gridResult ← cursorGrid' gridWidth gridHeight (fun x y isCursor => do
              let cells ← cellsRef.get
              let cellVal := if h : y < cells.size then
                let row := cells[y]
                if h2 : x < row.size then row[x] else none
              else none

              match cellVal with
              | some colorIdx =>
                let baseColor := getColor colorIdx
                let style : Style := if isCursor then
                  { fg := baseColor, bg := Color.ansi .white }
                else
                  { fg := baseColor }
                pure { content := "██", style }
              | none =>
                let style : Style := if isCursor then
                  { fg := Color.ansi .brightBlack, bg := Color.ansi .blue }
                else
                  { fg := Color.ansi .brightBlack }
                pure { content := "· ", style }
            ) { borderType := .rounded, focusName := "game-grid" }

            -- Show cursor position
            row' (gap := 1) {} do
              text' "Cursor:" theme.captionStyle
              emitDynamic do
                let (cx, cy) ← gridResult.cursorPos.sample
                pure (RNode.text s!"({cx}, {cy})" theme.primaryStyle)

          -- Score display
          row' (gap := 1) {} do
            text' "Score:" theme.captionStyle
            emitDynamic do
              let s ← scoreDyn.sample
              pure (RNode.text (toString s) { fg := Color.ansi .green, modifier := { bold := true } })

        -- Right panel: Animation demos
        column' (gap := 1) {} do
          titledBlock' "Animations" .rounded theme do
            -- Flash animation indicator
            row' (gap := 1) {} do
              text' "Flash:" theme.captionStyle
              emitDynamic do
                let progress ← flashAnim.progress.sample
                let isRunning ← flashAnim.isRunning.sample
                if isRunning then
                  let intensity := (255.0 * (1.0 - progress)).toUInt8
                  pure (RNode.text "████" { fg := Color.rgb intensity intensity intensity })
                else
                  pure (RNode.text "    " {})

            -- Pulse indicator
            row' (gap := 1) {} do
              text' "Pulse:" theme.captionStyle
              emitDynamic do
                let on ← pulse.sample
                if on then
                  pure (RNode.text "●" { fg := Color.ansi .green })
                else
                  pure (RNode.text "○" { fg := Color.ansi .brightBlack })

            -- Color cycle indicator
            row' (gap := 1) {} do
              text' "Cycle:" theme.captionStyle
              emitDynamic do
                let progress ← colorCycle.sample
                let colorIdx := (progress * Float.ofNat gameColors.size).toUInt64.toNat % gameColors.size
                let color := getColor colorIdx
                pure (RNode.text "████" { fg := color })

          -- Progress bar demo
          titledBlock' "Progress" .rounded theme do
            emitDynamic do
              let progress ← colorCycle.sample
              let barWidth := 20
              let filledWidth := (progress * Float.ofNat barWidth).toUInt64.toNat
              let emptyWidth := barWidth - filledWidth
              let bar := String.ofList (List.replicate filledWidth '█') ++
                         String.ofList (List.replicate emptyWidth '░')
              let percent := (progress * 100.0).toUInt64.toNat
              pure (RNode.row 0 {} #[
                RNode.text bar { fg := Color.ansi .cyan },
                RNode.text s!" {percent}%" theme.captionStyle
              ])

      -- Footer with focus info
      let focusedInput ← useFocusedInputW
      emitDynamic do
        let focused ← focusedInput.sample
        let focusName := focused.getD "(none)"
        pure (RNode.text s!"Focused: {focusName}" theme.captionStyle)

  pure { render }
