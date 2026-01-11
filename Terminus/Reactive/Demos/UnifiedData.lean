import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Data Tab Content -/

def dataContent (theme : Theme) (_events : TerminusEvents) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Menu, table, and calendar widgets." theme.captionStyle

    row' (gap := 2) {} do
      -- Menu
      column' (gap := 1) {} do
        titledBlock' "Menu Widget" .rounded theme do
          let menuItems := #[
            MenuItem'.new "File" |>.withSubmenu #[
              MenuItem'.new "New",
              MenuItem'.new "Open",
              MenuItem'.new "Save"
            ],
            MenuItem'.new "Edit" |>.withSubmenu #[
              MenuItem'.new "Cut",
              MenuItem'.new "Copy",
              MenuItem'.new "Paste"
            ],
            MenuItem'.new "Help"
          ]

          let menuResult ← menu' "demo-menu" menuItems {}

          -- FRP: Create selection Dynamic from menu events
          let selectionEvent ← Event.mapM (fun (_, label) => label) menuResult.onSelect
          let selectedDyn ← Reactive.holdDyn "None" selectionEvent

          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let sel ← selectedDyn.sample
              pure (RNode.text sel theme.bodyStyle)

      -- Table
      column' (gap := 1) {} do
        titledBlock' "Table Widget" .rounded theme do
          let columns := #[
            { header := "Name", width := .fixed 10 : TableColumn' },
            { header := "Role", width := .fixed 8 : TableColumn' }
          ]
          let rows := #[
            TableRow'.new #["Alice", "Engineer"],
            TableRow'.new #["Bob", "Designer"],
            TableRow'.new #["Carol", "Manager"]
          ]

          let tableResult ← table' "demo-table" columns rows {
            useAlternateColors := true
            maxHeight := some 4
          }

          row' (gap := 1) {} do
            text' "Row:" theme.captionStyle
            emitDynamic do
              let idx ← tableResult.selectedIndex.sample
              let display := match idx with
                | some i => toString i
                | none => "(none)"
              pure (RNode.text display theme.primaryStyle)

      -- Calendar
      column' (gap := 1) {} do
        titledBlock' "Calendar" .rounded theme do
          let today := CalendarDate.new 2024 1 15
          let calResult ← calendar' "demo-calendar" 2024 1 (some 15) {
            today := some today
          }

          -- FRP: Create date Dynamic from calendar events
          let dateEvent ← Event.mapM (fun date =>
            s!"{date.year}-{date.month}-{date.day}") calResult.onSelect
          let dateDyn ← Reactive.holdDyn "2024-01-15" dateEvent

          row' (gap := 1) {} do
            text' "Date:" theme.captionStyle
            emitDynamic do
              let date ← dateDyn.sample
              pure (RNode.text date theme.primaryStyle)

    spacer' 0 1

    row' (gap := 2) {} do
      -- Scrolling widgets
      column' (gap := 1) {} do
        titledBlock' "ScrollView" .rounded theme do
          text' "Tab to focus, arrows to scroll" theme.captionStyle
          let scroll ← scrollView' { maxVisible := 4, focusName := "demo-scroll" } do
            for i in [1:9] do
              text' s!"Item {i}" theme.bodyStyle

          emitDynamic do
            let state ← scroll.scrollState.sample
            pure (RNode.text s!"Offset: {state.offsetY}" theme.captionStyle)

        titledBlock' "Scrollbars" .rounded theme do
          text' "Vertical:" theme.captionStyle
          scrollbar' 2 10 4 { length := 6 }
          spacer' 0 1
          text' "Horizontal:" theme.captionStyle
          hScrollbar' 3 12 6 12

      -- Grid widgets
      column' (gap := 1) {} do
        titledBlock' "Grid Widgets" .rounded theme do
          text' "Static grid:" theme.captionStyle
          grid' 3 2 (fun x y => do
            pure { content := s!"{x}{y}", style := { fg := .ansi .cyan } }
          ) { borderType := .rounded }

          spacer' 0 1

          text' "Cursor grid (arrows):" theme.captionStyle
          let cursor ← cursorGrid' 4 3 (fun _ _ isCursor => do
            if isCursor then
              pure { content := "[]", style := { fg := .ansi .yellow } }
            else
              pure { content := " .", style := { fg := .ansi .brightBlack } }
          ) { focusName := "cursor-grid", borderType := .rounded }

          emitDynamic do
            let (cx, cy) ← cursor.cursorPos.sample
            pure (RNode.text s!"Cursor: {cx},{cy}" theme.captionStyle)

          spacer' 0 1

          text' "Char grid:" theme.captionStyle
          let cells : Array (Array (Char × Style)) := #[
            #[('A', { fg := .ansi .red }), ('B', { fg := .ansi .green })],
            #[('C', { fg := .ansi .yellow }), ('D', { fg := .ansi .cyan })]
          ]
          charGrid' cells { borderType := .rounded }

          spacer' 0 1

          titledBlock' "DataGrid" .rounded theme do
            text' "Enter to edit; arrows to move" theme.captionStyle
            let gridData := #[
              #["1", "Ada", "42"],
              #["2", "Linus", "7"],
              #["3", "Grace", "13"],
              #["4", "Edsger", "5"]
            ]
            let _ ← dataGrid' gridData {
              columnHeaders := some #["ID", "Name", "Score"]
              cellWidth := 8
              maxVisibleRows := some 4
              maxVisibleCols := some 3
              focusName := "data-grid"
            }
