/-
  Terminus Reactive - Calendar Widget
  Monthly calendar view with date selection.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Date Types -/

/-- Simple date representation. -/
structure CalendarDate where
  year : Nat
  month : Nat  -- 1-12
  day : Nat    -- 1-31
  deriving Repr, BEq, Inhabited

namespace CalendarDate

/-- Month names. -/
def monthNames : Array String := #[
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
]

/-- Check if a year is a leap year. -/
def isLeapYear (year : Nat) : Bool :=
  (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)

/-- Get the number of days in a month. -/
def daysInMonth (year month : Nat) : Nat :=
  match month with
  | 1 => 31  -- January
  | 2 => if isLeapYear year then 29 else 28  -- February
  | 3 => 31  -- March
  | 4 => 30  -- April
  | 5 => 31  -- May
  | 6 => 30  -- June
  | 7 => 31  -- July
  | 8 => 31  -- August
  | 9 => 30  -- September
  | 10 => 31 -- October
  | 11 => 30 -- November
  | 12 => 31 -- December
  | _ => 30

/-- Calculate day of week for the first of a month (0 = Sunday, 6 = Saturday)
    Using Zeller's congruence. -/
def firstDayOfMonth (year month : Nat) : Nat :=
  let (y, m) := if month <= 2 then (year - 1, month + 12) else (year, month)
  let q := 1
  let k := y % 100
  let j := y / 100
  let h := (q + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 + 5 * j) % 7
  (h + 6) % 7

/-- Create a date. -/
def new (year month day : Nat) : CalendarDate := {
  year
  month := max 1 (min month 12)
  day := max 1 (min day (daysInMonth year (max 1 (min month 12))))
}

/-- Get month name. -/
def monthName (d : CalendarDate) : String :=
  monthNames.getD ((d.month - 1) % 12) "?"

/-- Get number of days in this date's month. -/
def numDaysInMonth (d : CalendarDate) : Nat :=
  daysInMonth d.year d.month

/-- Get day of week for the first of this month. -/
def firstDay (d : CalendarDate) : Nat :=
  firstDayOfMonth d.year d.month

/-- Move to previous month. -/
def prevMonth (d : CalendarDate) : CalendarDate :=
  if d.month == 1 then { year := d.year - 1, month := 12, day := d.day }
  else { d with month := d.month - 1 }

/-- Move to next month. -/
def nextMonth (d : CalendarDate) : CalendarDate :=
  if d.month == 12 then { year := d.year + 1, month := 1, day := d.day }
  else { d with month := d.month + 1 }

/-- Normalize date to valid day in month. -/
def normalize (d : CalendarDate) : CalendarDate :=
  let maxDay := daysInMonth d.year d.month
  { d with day := max 1 (min d.day maxDay) }

end CalendarDate

/-! ## Calendar Configuration -/

/-- Configuration for calendar appearance and behavior. -/
structure CalendarConfig where
  /-- Style for month/year header. -/
  headerStyle : Style := { modifier := { bold := true } }
  /-- Style for day-of-week headers. -/
  dayHeaderStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for normal days. -/
  normalStyle : Style := {}
  /-- Style for selected day. -/
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  /-- Style for today (if shown). -/
  todayStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Style for weekend days. -/
  weekendStyle : Style := { fg := .ansi .brightBlack }
  /-- Optional today's date for highlighting. -/
  today : Option CalendarDate := none
  /-- Whether to start week on Monday (false = Sunday). -/
  weekStartsMonday : Bool := false
  /-- Focus name for keyboard input. -/
  focusName : String := "calendar"
  /-- Whether to use global key handling (ignores focus). -/
  globalKeys : Bool := false
  deriving Repr, Inhabited

/-! ## Calendar State -/

/-- Internal calendar state. -/
structure CalendarState where
  /-- Currently displayed year. -/
  year : Nat
  /-- Currently displayed month (1-12). -/
  month : Nat
  /-- Selected day (1-31 or 0 for none). -/
  selectedDay : Nat
  deriving Repr, Inhabited

namespace CalendarState

/-- Move to previous day. -/
def prevDay (state : CalendarState) : CalendarState :=
  if state.selectedDay <= 1 then
    let prev := CalendarDate.new state.year state.month 1 |>.prevMonth
    { year := prev.year, month := prev.month, selectedDay := prev.numDaysInMonth }
  else
    { state with selectedDay := state.selectedDay - 1 }

/-- Move to next day. -/
def nextDay (state : CalendarState) : CalendarState :=
  let maxDays := CalendarDate.daysInMonth state.year state.month
  if state.selectedDay >= maxDays then
    let next := CalendarDate.new state.year state.month 1 |>.nextMonth
    { year := next.year, month := next.month, selectedDay := 1 }
  else
    { state with selectedDay := state.selectedDay + 1 }

/-- Move to previous week. -/
def prevWeek (state : CalendarState) : CalendarState :=
  if state.selectedDay <= 7 then
    let prev := CalendarDate.new state.year state.month 1 |>.prevMonth
    let newDay := prev.numDaysInMonth - (7 - state.selectedDay)
    { year := prev.year, month := prev.month, selectedDay := max 1 newDay }
  else
    { state with selectedDay := state.selectedDay - 7 }

/-- Move to next week. -/
def nextWeek (state : CalendarState) : CalendarState :=
  let maxDays := CalendarDate.daysInMonth state.year state.month
  if state.selectedDay + 7 > maxDays then
    let next := CalendarDate.new state.year state.month 1 |>.nextMonth
    let overflow := state.selectedDay + 7 - maxDays
    { year := next.year, month := next.month, selectedDay := min overflow (next.numDaysInMonth) }
  else
    { state with selectedDay := state.selectedDay + 7 }

/-- Move to previous month. -/
def prevMonthState (state : CalendarState) : CalendarState :=
  let d := CalendarDate.new state.year state.month state.selectedDay |>.prevMonth
  let maxDay := d.numDaysInMonth
  { year := d.year, month := d.month, selectedDay := min state.selectedDay maxDay }

/-- Move to next month. -/
def nextMonthState (state : CalendarState) : CalendarState :=
  let d := CalendarDate.new state.year state.month state.selectedDay |>.nextMonth
  let maxDay := d.numDaysInMonth
  { year := d.year, month := d.month, selectedDay := min state.selectedDay maxDay }

/-- Get current date. -/
def toDate (state : CalendarState) : CalendarDate :=
  CalendarDate.new state.year state.month state.selectedDay

end CalendarState

/-! ## Calendar Result -/

/-- Result returned by calendar widget. -/
structure CalendarResult where
  /-- Currently selected date. -/
  selectedDate : Reactive.Dynamic Spider CalendarDate
  /-- Currently displayed month. -/
  currentMonth : Reactive.Dynamic Spider (Nat × Nat)  -- (year, month)
  /-- Event fired when a date is selected (Enter pressed). -/
  onSelect : Reactive.Event Spider CalendarDate
  /-- Event fired when month changes. -/
  onMonthChange : Reactive.Event Spider (Nat × Nat)

/-! ## Calendar Widget -/

/-- Day header strings. -/
private def dayHeadersSunday : String := "Su Mo Tu We Th Fr Sa"
private def dayHeadersMonday : String := "Mo Tu We Th Fr Sa Su"

/-- Create a calendar widget.

    The calendar supports keyboard navigation:
    - Left/Right arrows: Previous/next day
    - Up/Down arrows: Previous/next week
    - Page Up/Down: Previous/next month
    - Enter: Select current date

    Example:
    ```
    let cal ← calendar' "date-picker" 2025 1 (some 15) {}
    -- Handle date selection
    let _ ← cal.onSelect.subscribe fun date => do
      IO.println s!"Selected: {date.year}-{date.month}-{date.day}"
    ```
-/
def calendar' (name : String) (year : Nat) (month : Nat) (initialDay : Option Nat := none)
    (config : CalendarConfig := {}) : WidgetM CalendarResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW name (isInput := true) (nameOverride := name)

  -- Get focused state
  let focusedInput ← useFocusedInputW

  -- State
  let normalizedMonth := max 1 (min month 12)
  let maxDays := CalendarDate.daysInMonth year normalizedMonth
  let initialSelected := match initialDay with
    | some d => max 1 (min d maxDays)
    | none => 1

  let initialState := CalendarState.mk year normalizedMonth initialSelected


  -- Events

  let events ← getEventsW
  let inputName := if name.isEmpty then widgetName else name

  -- Derived dynamics
  let isFocusedDyn ← focusedInput.map' (· == some inputName)
  let shouldProcessKeys ← if config.globalKeys then
      Dynamic.pureM true
    else
      pure isFocusedDyn

  let keyEvents ← Event.gateM shouldProcessKeys.current events.keyEvent

  -- Map key events to state transformations
  let stateOps ← Event.mapMaybeM (fun kd =>
    match kd.event.code with
    | .left | .char 'h' => some CalendarState.prevDay
    | .right | .char 'l' => some CalendarState.nextDay
    | .up | .char 'k' => some CalendarState.prevWeek
    | .down | .char 'j' => some CalendarState.nextWeek
    | .pageUp => some CalendarState.prevMonthState
    | .pageDown => some CalendarState.nextMonthState
    | _ => none) keyEvents

  -- Fold state operations
  let stateDyn ← foldDyn id initialState stateOps

  -- Enter key event for selection
  let enterE ← Event.filterM (fun kd => kd.event.code == .enter) keyEvents

  let selectedDate ← stateDyn.map' CalendarState.toDate
  let currentMonth ← stateDyn.map' fun s => (s.year, s.month)

  -- Derive select event (samples current state when Enter is pressed)
  let selectEvent ← Event.attachWithM (fun s _ => s.toDate) stateDyn.current enterE

  let node ← stateDyn.map' fun state =>
    Id.run do
      let firstDay := CalendarDate.firstDayOfMonth state.year state.month
      let numDays := CalendarDate.daysInMonth state.year state.month

      let mut nodes : Array RNode := #[]

      let monthName := CalendarDate.monthNames.getD ((state.month - 1) % 12) "?"
      let header := s!"{monthName} {state.year}"
      nodes := nodes.push (RNode.text header config.headerStyle)

      let dayHeaders := if config.weekStartsMonday then dayHeadersMonday else dayHeadersSunday
      nodes := nodes.push (RNode.text dayHeaders config.dayHeaderStyle)

      let adjustedFirstDay := if config.weekStartsMonday then
        (firstDay + 6) % 7
      else firstDay

      let mut day := 1
      for row in [:6] do
        if day > numDays then break

        let mut rowParts : Array RNode := #[]
        for col in [:7] do
          let cellPos := row * 7 + col

          if cellPos < adjustedFirstDay || day > numDays then
            -- Empty cell
            rowParts := rowParts.push (RNode.text "   " {})
          else
            -- Day cell
            let isSelected := day == state.selectedDay
            let isWeekend := if config.weekStartsMonday then col >= 5 else col == 0 || col == 6
            let isToday := match config.today with
              | some t => t.year == state.year && t.month == state.month && t.day == day
              | none => false

            let style := if isSelected then config.selectedStyle
                         else if isToday then config.todayStyle
                         else if isWeekend then config.weekendStyle
                         else config.normalStyle

            -- Format day (right-aligned in 2 chars + space)
            let dayStr := if day < 10 then s!" {day} " else s!"{day} "
            rowParts := rowParts.push (RNode.text dayStr style)

            day := day + 1

        nodes := nodes.push (RNode.row 0 {} rowParts)

      return RNode.column 0 {} nodes
  emit node

  pure {
    selectedDate := selectedDate
    currentMonth := currentMonth
    onSelect := selectEvent
    onMonthChange := currentMonth.updated
  }

/-! ## Convenience Functions -/

/-- Create a calendar for the current month (uses a default date). -/
def currentMonthCalendar' (name : String) (today : CalendarDate) (config : CalendarConfig := {})
    : WidgetM CalendarResult :=
  calendar' name today.year today.month (some today.day) { config with today := some today }

/-- Create a date picker calendar. -/
def datePicker' (name : String) (initialDate : CalendarDate) (config : CalendarConfig := {})
    : WidgetM CalendarResult :=
  calendar' name initialDate.year initialDate.month (some initialDate.day) config

end Terminus.Reactive
