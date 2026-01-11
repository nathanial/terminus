/-
  Reactive Data Demo Widgets
  Demonstrates Menu, Table, and Calendar widgets.
-/
import Terminus.Reactive

open Terminus.Reactive
open Reactive Reactive.Host

def reactiveDataApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- State for selected items
  let menuSelectionRef ← SpiderM.liftIO (IO.mkRef ("None" : String))
  let tableSelectionRef ← SpiderM.liftIO (IO.mkRef ("None" : String))
  let calendarDateRef ← SpiderM.liftIO (IO.mkRef ("2024-01-01" : String))

  -- Define focusable component names
  let focusableNames := #["demo-menu", "demo-table", "demo-calendar"]
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

  -- Set initial focus to menu
  SpiderM.liftIO <| events.registry.fireFocus (some "demo-menu")

  -- Build the UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Data Demo ===" theme.heading1Style
      text' "Tab: switch focus | Arrows: navigate | Enter: select | Esc: back" theme.captionStyle

      -- Main content row
      row' (gap := 2) {} do
        -- Left column: Menu
        column' (gap := 1) {} do
          titledBlock' "1. Menu Widget" .rounded theme do
            let menuItems := #[
              MenuItem'.new "File" |>.withSubmenu #[
                MenuItem'.new "New" |>.withShortcut "Ctrl+N",
                MenuItem'.new "Open" |>.withShortcut "Ctrl+O",
                MenuItem'.new "Save" |>.withShortcut "Ctrl+S",
                MenuItem'.separator,
                MenuItem'.new "Exit"
              ],
              MenuItem'.new "Edit" |>.withSubmenu #[
                MenuItem'.new "Cut" |>.withShortcut "Ctrl+X",
                MenuItem'.new "Copy" |>.withShortcut "Ctrl+C",
                MenuItem'.new "Paste" |>.withShortcut "Ctrl+V"
              ],
              MenuItem'.new "View" |>.withSubmenu #[
                MenuItem'.new "Zoom In",
                MenuItem'.new "Zoom Out",
                MenuItem'.disabled "Full Screen"
              ],
              MenuItem'.new "Help"
            ]

            let menuResult ← menu' "demo-menu" menuItems {}

            -- Subscribe to menu selection
            let _ ← SpiderM.liftIO <| menuResult.onSelect.subscribe fun (_, label) =>
              menuSelectionRef.set label

          -- Display selected item
          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let sel ← menuSelectionRef.get
              pure (RNode.text sel theme.bodyStyle)

        -- Middle column: Table
        column' (gap := 1) {} do
          titledBlock' "2. Table Widget" .rounded theme do
            let columns := #[
              { header := "Name", width := .fixed 12 : TableColumn' },
              { header := "Role", width := .fixed 10 : TableColumn' },
              { header := "Status", width := .fixed 8 : TableColumn' }
            ]

            let rows := #[
              TableRow'.new #["Alice", "Engineer", "Active"],
              TableRow'.new #["Bob", "Designer", "Away"],
              TableRow'.new #["Carol", "Manager", "Active"],
              TableRow'.new #["David", "Intern", "Active"],
              TableRow'.new #["Eve", "Analyst", "Busy"]
            ]

            let tableResult ← table' "demo-table" columns rows {
              useAlternateColors := true
              maxHeight := some 5
            }

            -- Subscribe to table selection
            let _ ← SpiderM.liftIO <| tableResult.onSelect.subscribe fun (idx, row) =>
              match row.cells[0]? with
              | some cell => tableSelectionRef.set cell.content
              | none => pure ()

          -- Display selected row
          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let sel ← tableSelectionRef.get
              pure (RNode.text sel theme.bodyStyle)

        -- Right column: Calendar
        column' (gap := 1) {} do
          titledBlock' "3. Calendar Widget" .rounded theme do
            let today := CalendarDate.new 2024 1 15
            let calResult ← calendar' "demo-calendar" 2024 1 (some 15) {
              today := some today
              weekStartsMonday := false
            }

            -- Subscribe to date selection
            let _ ← SpiderM.liftIO <| calResult.onSelect.subscribe fun date =>
              calendarDateRef.set s!"{date.year}-{date.month}-{date.day}"

          -- Display selected date
          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let date ← calendarDateRef.get
              pure (RNode.text date theme.bodyStyle)

      -- Footer with instructions
      text' "Press Ctrl+C to quit" theme.captionStyle

  pure { render }
