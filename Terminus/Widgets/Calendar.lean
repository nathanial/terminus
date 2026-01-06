-- Terminus.Widgets.Calendar: Monthly calendar view with date selection

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Month names -/
private def monthNames : Array String := #[
  "January", "February", "March", "April", "May", "June",
  "July", "August", "September", "October", "November", "December"
]

/-- Check if a year is a leap year -/
private def isLeapYear (year : Nat) : Bool :=
  (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0)

/-- Get the number of days in a month -/
private def daysInMonth (year month : Nat) : Nat :=
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
    Using Zeller's congruence -/
private def firstDayOfMonth (year month : Nat) : Nat :=
  -- Adjust month and year for Zeller's formula (Jan/Feb are 13/14 of prev year)
  let (y, m) := if month <= 2 then (year - 1, month + 12) else (year, month)
  let q := 1  -- First day of month
  let k := y % 100
  let j := y / 100
  -- Zeller's congruence for Gregorian calendar
  let h := (q + (13 * (m + 1)) / 5 + k + k / 4 + j / 4 + 5 * j) % 7
  -- Convert from Zeller (0=Sat, 1=Sun, ...) to our format (0=Sun, 6=Sat)
  (h + 6) % 7

/-- Calendar widget -/
structure Calendar where
  year : Nat := 2025
  month : Nat := 1           -- 1-12
  selectedDay : Option Nat := none
  headerStyle : Style := Style.bold
  dayHeaderStyle : Style := Style.dim
  normalStyle : Style := Style.default
  selectedStyle : Style := Style.reversed
  todayStyle : Style := Style.bold.withFg Color.cyan
  weekendStyle : Style := Style.dim
  outsideStyle : Style := Style.dim
  block : Option Block := none
  deriving Repr, Inhabited

namespace Calendar

def new (year month : Nat) : Calendar := { year, month := Nat.max 1 (Nat.min month 12) }

def withYear (c : Calendar) (y : Nat) : Calendar := { c with year := y }
def withMonth (c : Calendar) (m : Nat) : Calendar := { c with month := Nat.max 1 (Nat.min m 12) }
def withSelectedDay (c : Calendar) (d : Nat) : Calendar := { c with selectedDay := some d }
def clearSelection (c : Calendar) : Calendar := { c with selectedDay := none }

def withHeaderStyle (c : Calendar) (s : Style) : Calendar := { c with headerStyle := s }
def withDayHeaderStyle (c : Calendar) (s : Style) : Calendar := { c with dayHeaderStyle := s }
def withNormalStyle (c : Calendar) (s : Style) : Calendar := { c with normalStyle := s }
def withSelectedStyle (c : Calendar) (s : Style) : Calendar := { c with selectedStyle := s }
def withTodayStyle (c : Calendar) (s : Style) : Calendar := { c with todayStyle := s }
def withWeekendStyle (c : Calendar) (s : Style) : Calendar := { c with weekendStyle := s }
def withBlock (c : Calendar) (b : Block) : Calendar := { c with block := some b }

/-- Get month name -/
def monthName (c : Calendar) : String :=
  monthNames.getD ((c.month - 1) % 12) "?"

/-- Get number of days in current month -/
def numDays (c : Calendar) : Nat :=
  daysInMonth c.year c.month

/-- Get the day of week for day 1 of current month -/
def firstDay (c : Calendar) : Nat :=
  firstDayOfMonth c.year c.month

/-- Move to previous month -/
def prevMonth (c : Calendar) : Calendar :=
  if c.month == 1 then { c with year := c.year - 1, month := 12 }
  else { c with month := c.month - 1 }

/-- Move to next month -/
def nextMonth (c : Calendar) : Calendar :=
  if c.month == 12 then { c with year := c.year + 1, month := 1 }
  else { c with month := c.month + 1 }

/-- Select previous day -/
def prevDay (c : Calendar) : Calendar :=
  match c.selectedDay with
  | none => { c with selectedDay := some 1 }
  | some 1 =>
    let prev := c.prevMonth
    { prev with selectedDay := some prev.numDays }
  | some d => { c with selectedDay := some (d - 1) }

/-- Select next day -/
def nextDay (c : Calendar) : Calendar :=
  match c.selectedDay with
  | none => { c with selectedDay := some 1 }
  | some d =>
    if d >= c.numDays then
      let next := c.nextMonth
      { next with selectedDay := some 1 }
    else { c with selectedDay := some (d + 1) }

/-- Move selection up (previous week) -/
def prevWeek (c : Calendar) : Calendar :=
  match c.selectedDay with
  | none => { c with selectedDay := some 1 }
  | some d =>
    if d <= 7 then
      let prev := c.prevMonth
      let newDay := prev.numDays - (7 - d)
      { prev with selectedDay := some newDay }
    else { c with selectedDay := some (d - 7) }

/-- Move selection down (next week) -/
def nextWeek (c : Calendar) : Calendar :=
  match c.selectedDay with
  | none => { c with selectedDay := some 1 }
  | some d =>
    if d + 7 > c.numDays then
      let next := c.nextMonth
      let newDay := d + 7 - c.numDays
      { next with selectedDay := some newDay }
    else { c with selectedDay := some (d + 7) }

end Calendar

instance : Widget Calendar where
  render c area buf := Id.run do
    -- Render block if present
    let mut result := match c.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get content area
    let contentArea := match c.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty || contentArea.height < 3 then return result

    -- Calendar layout:
    -- Row 0: Month Year header (centered)
    -- Row 1: Day headers (Su Mo Tu We Th Fr Sa)
    -- Rows 2-7: Day numbers (up to 6 weeks)

    let cellWidth := 3  -- Each cell is 3 chars wide (2 digits + space)
    let calendarWidth := 7 * cellWidth - 1  -- 20 chars total
    let startX := contentArea.x + (contentArea.width - calendarWidth) / 2

    -- Row 0: Header
    let header := s!"{c.monthName} {c.year}"
    let headerX := contentArea.x + (contentArea.width - header.length) / 2
    let headerChars := header.toList
    for i in [:headerChars.length] do
      match headerChars[i]? with
      | some ch => result := result.setStyled (headerX + i) contentArea.y ch c.headerStyle
      | none => pure ()

    -- Row 1: Day headers
    if contentArea.height > 1 then
      let dayHeaders := "Su Mo Tu We Th Fr Sa"
      let y := contentArea.y + 1
      let dayHeaderChars := dayHeaders.toList
      for i in [:dayHeaderChars.length] do
        match dayHeaderChars[i]? with
        | some ch => result := result.setStyled (startX + i) y ch c.dayHeaderStyle
        | none => pure ()

    -- Rows 2+: Days
    let firstDay := c.firstDay
    let numDays := c.numDays

    let mut day := 1
    let mut row := 0

    while day <= numDays && row < 6 do
      let y := contentArea.y + 2 + row
      if y >= contentArea.y + contentArea.height then break

      for col in [:7] do
        let cellStartDay := row * 7 + col
        if cellStartDay >= firstDay && day <= numDays then
          let x := startX + col * cellWidth

          -- Determine style
          let isSelected := c.selectedDay == some day
          let isWeekend := col == 0 || col == 6

          let style := if isSelected then c.selectedStyle
                       else if isWeekend then c.weekendStyle
                       else c.normalStyle

          -- Format day number (right-aligned in 2 chars)
          let dayStr := if day < 10 then s!" {day}" else s!"{day}"
          let dayChars := dayStr.toList
          for j in [:dayChars.length] do
            if x + j < contentArea.x + contentArea.width then
              match dayChars[j]? with
              | some ch => result := result.setStyled (x + j) y ch style
              | none => pure ()

          day := day + 1

      row := row + 1

    result

end Terminus
