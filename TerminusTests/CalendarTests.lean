-- TerminusTests.CalendarTests: Tests for Calendar widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Calendar

namespace TerminusTests.CalendarTests

open Terminus
open Crucible

testSuite "Calendar Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "Calendar.new creates with year and month" := do
  let cal := Calendar.new 2025 6
  cal.year ≡ 2025
  cal.month ≡ 6

test "Calendar.new clamps month to valid range" := do
  let cal := Calendar.new 2025 15
  cal.month ≡ 12

test "Calendar.withSelectedDay sets selection" := do
  let cal := Calendar.new 2025 1 |>.withSelectedDay 15
  cal.selectedDay ≡ some 15

test "Calendar.nextDay advances selection" := do
  let cal := Calendar.new 2025 1 |>.withSelectedDay 10 |>.nextDay
  cal.selectedDay ≡ some 11

test "Calendar.prevDay moves selection back" := do
  let cal := Calendar.new 2025 1 |>.withSelectedDay 10 |>.prevDay
  cal.selectedDay ≡ some 9

test "Calendar.nextMonth advances month" := do
  let cal := Calendar.new 2025 6 |>.nextMonth
  cal.month ≡ 7

test "Calendar.prevMonth moves month back" := do
  let cal := Calendar.new 2025 6 |>.prevMonth
  cal.month ≡ 5

test "Calendar.monthName returns correct name" := do
  let cal := Calendar.new 2025 3
  cal.monthName ≡ "March"

test "Calendar.numDays returns days in month" := do
  let cal := Calendar.new 2025 2  -- February 2025 (not leap year)
  cal.numDays ≡ 28

test "Calendar renders without crash" := do
  let cal := Calendar.new 2025 12 |>.withSelectedDay 25
  let buf := renderWidget cal 25 10
  buf.width ≡ 25

#generate_tests

end TerminusTests.CalendarTests
