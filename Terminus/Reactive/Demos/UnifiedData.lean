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
        titledBlock' "Menu Widget" .rounded theme none do
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
            let node ← selectedDyn.map' (fun sel =>
              RNode.text sel theme.bodyStyle
            )
            emit node

      -- Table
      column' (gap := 1) {} do
        titledBlock' "Table Widget" .rounded theme none do
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
            let node ← tableResult.selectedIndex.map' (fun idx =>
              let display := match idx with
                | some i => toString i
                | none => "(none)"
              RNode.text display theme.primaryStyle
            )
            emit node

      -- Calendar
      column' (gap := 1) {} do
        titledBlock' "Calendar" .rounded theme none do
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
            let node ← dateDyn.map' (fun date =>
              RNode.text date theme.primaryStyle
            )
            emit node

    spacer' 0 1

    row' (gap := 2) {} do
      -- Scrolling widgets
      column' (gap := 1) {} do
        titledBlock' "ScrollView" .rounded theme none do
          text' "Tab to focus, arrows to scroll" theme.captionStyle
          let scroll ← scrollView' { maxVisible := 4, focusName := "demo-scroll" } do
            for i in [1:9] do
              text' s!"Item {i}" theme.bodyStyle

          let node ← scroll.scrollState.map' (fun state =>
            RNode.text s!"Offset: {state.offsetY}" theme.captionStyle
          )
          emit node

        titledBlock' "Scrollbars" .rounded theme none do
          text' "Vertical:" theme.captionStyle
          scrollbar' 2 10 4 { length := 6 }
          spacer' 0 1
          text' "Horizontal:" theme.captionStyle
          hScrollbar' 3 12 6 12

      -- Grid widgets
      column' (gap := 1) {} do
        titledBlock' "Grid Widgets" .rounded theme none do
          text' "Static grid:" theme.captionStyle
          grid' 3 2 (fun x y =>
            { content := s!"{x}{y}", style := { fg := .ansi .cyan } }
          ) { borderType := .rounded }

          spacer' 0 1

          text' "Cursor grid (arrows):" theme.captionStyle
          let cursor ← cursorGrid' 4 3 (fun _ _ isCursor =>
            if isCursor then
              { content := "[]", style := { fg := .ansi .yellow } }
            else
              { content := " .", style := { fg := .ansi .brightBlack } }
          ) { focusName := "cursor-grid", borderType := .rounded }

          let node ← cursor.cursorPos.map' (fun (cx, cy) =>
            RNode.text s!"Cursor: {cx},{cy}" theme.captionStyle
          )
          emit node

          spacer' 0 1

          text' "Char grid:" theme.captionStyle
          let cells : Array (Array (Char × Style)) := #[
            #[('A', { fg := .ansi .red }), ('B', { fg := .ansi .green })],
            #[('C', { fg := .ansi .yellow }), ('D', { fg := .ansi .cyan })]
          ]
          charGrid' cells { borderType := .rounded }

          spacer' 0 1

          titledBlock' "DataGrid" .rounded theme none do
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

      -- VirtualList
      column' (gap := 1) {} do
        titledBlock' "VirtualList" .rounded theme none do
          text' "↑/↓ nav, PgUp/PgDn scroll (1000 items)" theme.captionStyle
          -- Generate 1000 items
          let items := (List.range 1000).toArray.map fun i => s!"Item #{i + 1}"
          let vlist ← virtualList' "vlist" items {
            visibleRows := 6
            scrollIndicator := true
          }
          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            let selStr ← Dynamic.map' vlist.selectedItem (·.getD "(none)")
            dynText' selStr theme.primaryStyle
          row' (gap := 1) {} do
            text' "Range:" theme.captionStyle
            let rangeStr ← Dynamic.map' vlist.visibleRange fun (start, endIdx) =>
              s!"{start}-{endIdx}"
            dynText' rangeStr theme.captionStyle
