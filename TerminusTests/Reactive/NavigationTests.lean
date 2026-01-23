-- TerminusTests.Reactive.NavigationTests: Tests for Menu, Table, and Calendar widgets

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.NavigationTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Navigation Widget Tests"

-- ============================================================================
-- MenuItem' Tests
-- ============================================================================

test "MenuItem'.new creates basic item" := do
  let item := MenuItem'.new "File"
  item.label ≡ "File"
  item.enabled ≡ true
  item.isSeparator ≡ false
  item.submenu.isEmpty ≡ true

test "MenuItem'.separator creates separator" := do
  let sep := MenuItem'.separator
  sep.isSeparator ≡ true

test "MenuItem'.disabled creates disabled item" := do
  let item := MenuItem'.disabled "Unavailable"
  item.label ≡ "Unavailable"
  item.enabled ≡ false

test "MenuItem'.withShortcut adds shortcut" := do
  let item := MenuItem'.new "Save" |>.withShortcut "Ctrl+S"
  item.shortcut ≡ some "Ctrl+S"

test "MenuItem'.withSubmenu adds submenu" := do
  let submenu := #[MenuItem'.new "New", MenuItem'.new "Open"]
  let item := MenuItem'.new "File" |>.withSubmenu submenu
  item.hasSubmenu ≡ true
  item.submenu.size ≡ 2

-- ============================================================================
-- Menu State Tests
-- ============================================================================

test "MenuState navigates to next enabled item" := do
  let items := #[
    MenuItem'.new "First",
    MenuItem'.separator,  -- Should skip
    MenuItem'.new "Second"
  ]
  let state := MenuState.mk 0 #[]
  let next := state.navigateNext items
  next.selectedIndex ≡ 2  -- Skips separator

test "MenuState navigates to previous enabled item" := do
  let items := #[
    MenuItem'.new "First",
    MenuItem'.separator,
    MenuItem'.new "Second"
  ]
  let state := MenuState.mk 2 #[]
  let prev := state.navigatePrev items
  prev.selectedIndex ≡ 0  -- Skips separator

test "MenuState.openSubmenu tracks path" := do
  let state := MenuState.mk 1 #[]
  let opened := state.openSubmenu
  opened.openPath ≡ #[1]
  opened.selectedIndex ≡ 0

test "MenuState.closeSubmenu restores selection" := do
  let state := MenuState.mk 0 #[2]
  let closed := state.closeSubmenu
  closed.openPath ≡ #[]
  closed.selectedIndex ≡ 2

-- ============================================================================
-- Menu Widget Tests
-- ============================================================================

test "menu' renders items" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #[MenuItem'.new "File", MenuItem'.new "Edit", MenuItem'.new "View"]

    let (_result, render) ← (runWidget do
      menu' "test-menu" items {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "File") "expected File")
    SpiderM.liftIO (ensure (rnodeContainsText node "Edit") "expected Edit")
    SpiderM.liftIO (ensure (rnodeContainsText node "View") "expected View")

test "menu' renders shortcuts" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #[
      MenuItem'.new "Save" |>.withShortcut "Ctrl+S",
      MenuItem'.new "Quit" |>.withShortcut "Ctrl+Q"
    ]

    let (_result, render) ← (runWidget do
      menu' "test-menu" items {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Ctrl+S") "expected Ctrl+S shortcut")
    SpiderM.liftIO (ensure (rnodeContainsText node "Ctrl+Q") "expected Ctrl+Q shortcut")

-- ============================================================================
-- TableCell' and TableRow' Tests
-- ============================================================================

test "TableCell'.new creates basic cell" := do
  let cell := TableCell'.new "Hello"
  cell.content ≡ "Hello"

test "TableCell'.styled creates styled cell" := do
  let style : Style := { fg := .ansi .red }
  let cell := TableCell'.styled "Error" style
  cell.content ≡ "Error"

test "TableRow'.new creates row from strings" := do
  let row := TableRow'.new #["A", "B", "C"]
  row.cells.size ≡ 3
  row.cells[0]!.content ≡ "A"
  row.cells[1]!.content ≡ "B"
  row.cells[2]!.content ≡ "C"

-- ============================================================================
-- Table State Tests
-- ============================================================================

test "TableState.navigateNext moves to next row" := do
  let state := TableState.mk (some 0) 0
  let next := state.navigateNext 5 none
  next.selectedIndex ≡ some 1

test "TableState.navigatePrev moves to previous row" := do
  let state := TableState.mk (some 2) 0
  let prev := state.navigatePrev 5
  prev.selectedIndex ≡ some 1

test "TableState.navigateNext stays at end" := do
  let state := TableState.mk (some 4) 0
  let next := state.navigateNext 5 none
  next.selectedIndex ≡ some 4

test "TableState.navigatePrev stays at start" := do
  let state := TableState.mk (some 0) 0
  let prev := state.navigatePrev 5
  prev.selectedIndex ≡ some 0

test "TableState.clampSelection clamps to valid range" := do
  let state := TableState.mk (some 10) 0
  let clamped := state.clampSelection 5
  clamped.selectedIndex ≡ some 4

test "TableState.clampSelection handles empty" := do
  let state := TableState.mk (some 3) 0
  let clamped := state.clampSelection 0
  clamped.selectedIndex ≡ none

-- ============================================================================
-- Table Widget Tests
-- ============================================================================

test "table' renders header" := do
  runSpider do
    let (events, _) ← createInputs
    let columns := #[
      { header := "Name", width := .fill : TableColumn' },
      { header := "Age", width := .fixed 5 : TableColumn' }
    ]
    let rows := #[TableRow'.new #["Alice", "30"]]

    let (_result, render) ← (runWidget do
      table' "test-table" columns rows {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Name") "expected Name header")
    SpiderM.liftIO (ensure (rnodeContainsText node "Age") "expected Age header")

test "table' renders rows" := do
  runSpider do
    let (events, _) ← createInputs
    let columns := #[{ header := "Item", width := .fill : TableColumn' }]
    let rows := #[
      TableRow'.new #["First"],
      TableRow'.new #["Second"]
    ]

    let (_result, render) ← (runWidget do
      table' "test-table" columns rows {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "First") "expected First row")
    SpiderM.liftIO (ensure (rnodeContainsText node "Second") "expected Second row")

test "simpleTable' creates table from arrays" := do
  runSpider do
    let (events, _) ← createInputs
    let headers := #["Col1", "Col2"]
    let data := #[#["A", "B"], #["C", "D"]]

    let (_result, render) ← (runWidget do
      simpleTable' "test-table" headers data {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Col1") "expected Col1 header")
    SpiderM.liftIO (ensure (rnodeContainsText node "A") "expected A cell")

-- ============================================================================
-- CalendarDate Tests
-- ============================================================================

test "CalendarDate.isLeapYear correctly identifies leap years" := do
  CalendarDate.isLeapYear 2000 ≡ true   -- Divisible by 400
  CalendarDate.isLeapYear 2004 ≡ true   -- Divisible by 4, not by 100
  CalendarDate.isLeapYear 1900 ≡ false  -- Divisible by 100, not by 400
  CalendarDate.isLeapYear 2001 ≡ false  -- Not divisible by 4

test "CalendarDate.daysInMonth returns correct days" := do
  CalendarDate.daysInMonth 2024 1 ≡ 31   -- January
  CalendarDate.daysInMonth 2024 2 ≡ 29   -- February (leap year)
  CalendarDate.daysInMonth 2023 2 ≡ 28   -- February (non-leap)
  CalendarDate.daysInMonth 2024 4 ≡ 30   -- April

test "CalendarDate.new normalizes day" := do
  let d := CalendarDate.new 2024 2 31  -- Feb doesn't have 31 days
  d.day ≡ 29  -- Clamped to 29 (leap year)

test "CalendarDate.prevMonth moves to previous month" := do
  let d := CalendarDate.new 2024 3 15
  let prev := d.prevMonth
  prev.month ≡ 2
  prev.year ≡ 2024

test "CalendarDate.prevMonth wraps to previous year" := do
  let d := CalendarDate.new 2024 1 15
  let prev := d.prevMonth
  prev.month ≡ 12
  prev.year ≡ 2023

test "CalendarDate.nextMonth moves to next month" := do
  let d := CalendarDate.new 2024 3 15
  let next := d.nextMonth
  next.month ≡ 4
  next.year ≡ 2024

test "CalendarDate.nextMonth wraps to next year" := do
  let d := CalendarDate.new 2024 12 15
  let next := d.nextMonth
  next.month ≡ 1
  next.year ≡ 2025

-- ============================================================================
-- CalendarState Tests
-- ============================================================================

test "CalendarState.prevDay moves to previous day" := do
  let state := CalendarState.mk 2024 3 15
  let prev := state.prevDay
  prev.selectedDay ≡ 14

test "CalendarState.prevDay wraps to previous month" := do
  let state := CalendarState.mk 2024 3 1
  let prev := state.prevDay
  prev.month ≡ 2
  prev.selectedDay ≡ 29  -- Feb 2024 has 29 days (leap year)

test "CalendarState.nextDay moves to next day" := do
  let state := CalendarState.mk 2024 3 15
  let next := state.nextDay
  next.selectedDay ≡ 16

test "CalendarState.nextDay wraps to next month" := do
  let state := CalendarState.mk 2024 3 31
  let next := state.nextDay
  next.month ≡ 4
  next.selectedDay ≡ 1

test "CalendarState.prevWeek moves up a week" := do
  let state := CalendarState.mk 2024 3 15
  let prev := state.prevWeek
  prev.selectedDay ≡ 8

test "CalendarState.nextWeek moves down a week" := do
  let state := CalendarState.mk 2024 3 15
  let next := state.nextWeek
  next.selectedDay ≡ 22

-- ============================================================================
-- Calendar Widget Tests
-- ============================================================================

test "calendar' renders month header" := do
  runSpider do
    let (events, _) ← createInputs

    let (_result, render) ← (runWidget do
      calendar' "test-cal" 2024 3 (some 15) {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "March") "expected March")
    SpiderM.liftIO (ensure (rnodeContainsText node "2024") "expected 2024")

test "calendar' renders day headers" := do
  runSpider do
    let (events, _) ← createInputs

    let (_result, render) ← (runWidget do
      calendar' "test-cal" 2024 1 none {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Su") "expected Su")
    SpiderM.liftIO (ensure (rnodeContainsText node "Mo") "expected Mo")
    SpiderM.liftIO (ensure (rnodeContainsText node "Fr") "expected Fr")

test "calendar' renders day numbers" := do
  runSpider do
    let (events, _) ← createInputs

    let (_result, render) ← (runWidget do
      calendar' "test-cal" 2024 1 none {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "15") "expected day 15")
    SpiderM.liftIO (ensure (rnodeContainsText node "31") "expected day 31")

test "calendar' returns correct initial date" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, _render) ← (runWidget do
      calendar' "test-cal" 2024 6 (some 20) {}
    ).run events

    let date ← SpiderM.liftIO result.selectedDate.sample
    SpiderM.liftIO (date.year ≡ 2024)
    SpiderM.liftIO (date.month ≡ 6)
    SpiderM.liftIO (date.day ≡ 20)



end TerminusTests.Reactive.NavigationTests
