-- TerminusTests.LoggerTests: Tests for Logger widget

import Crucible
import Terminus.Core.Buffer
import Terminus.Core.Rect
import Terminus.Widgets.Widget
import Terminus.Widgets.Logger

namespace TerminusTests.LoggerTests

open Terminus
open Crucible

testSuite "Logger Widget Tests"

def renderWidget [Widget α] (widget : α) (width height : Nat) : Buffer :=
  let buf := Buffer.new width height
  let area : Rect := { x := 0, y := 0, width, height }
  Widget.render widget area buf

test "LogLevel.severity returns correct ordering" := do
  LogLevel.trace.severity ≡ 0
  LogLevel.debug.severity ≡ 1
  LogLevel.info.severity ≡ 2
  LogLevel.warn.severity ≡ 3
  LogLevel.error.severity ≡ 4

test "LogLevel.label returns display name" := do
  LogLevel.info.label ≡ "INFO"
  LogLevel.error.label ≡ "ERROR"

test "LogLevel.tag returns bracketed name" := do
  LogLevel.warn.tag ≡ "[WARN]"

test "LogEntry.new creates entry" := do
  let entry := LogEntry.new .info "Test message" (some "12:00:00")
  entry.level ≡ .info
  entry.timestamp ≡ some "12:00:00"
  entry.message ≡ "Test message"

test "Logger.new creates empty logger" := do
  let logger := Logger.new
  logger.entries.length ≡ 0
  logger.follow ≡ true

test "Logger.add appends entry" := do
  let logger := Logger.new
    |>.add (LogEntry.new .info "First" (some "12:00"))
    |>.add (LogEntry.new .warn "Second" (some "12:01"))
  logger.entries.length ≡ 2

test "Logger.extend adds multiple entries" := do
  let entries := [
    LogEntry.new .info "A" (some "12:00"),
    LogEntry.new .debug "B" (some "12:01")
  ]
  let logger := Logger.new |>.extend entries
  logger.entries.length ≡ 2

test "Logger.clear removes all entries" := do
  let logger := Logger.new
    |>.add (LogEntry.new .info "Test")
    |>.clear
  logger.entries.length ≡ 0
  logger.scroll ≡ 0

test "Logger.scrollDown increments scroll" := do
  let logger := Logger.new |>.scrollDown 5
  logger.scroll ≡ 5
  logger.follow ≡ false  -- Scrolling disables follow

test "Logger.scrollUp decrements scroll" := do
  let logger := Logger.new |>.scrollDown 10 |>.scrollUp 3
  logger.scroll ≡ 7

test "Logger.home resets scroll to 0" := do
  let logger := Logger.new |>.scrollDown 20 |>.home
  logger.scroll ≡ 0

test "Logger.end_ sets scroll to end" := do
  let entries := List.replicate 50 (LogEntry.new .info "Test")
  let logger := Logger.new |>.extend entries |>.end_
  logger.follow ≡ true  -- end_ re-enables follow

test "Logger.toggleFollow toggles follow mode" := do
  let logger := Logger.new |>.toggleFollow
  logger.follow ≡ false
  let logger2 := logger.toggleFollow
  logger2.follow ≡ true

test "LogLevelFilter.allows with minLevel" := do
  let filter := { minLevel := some LogLevel.warn : LogLevelFilter }
  filter.allows .error ≡ true
  filter.allows .warn ≡ true
  filter.allows .info ≡ false

test "LogLevelFilter.allows with allowedLevels" := do
  let filter := { allowedLevels := some [LogLevel.info, LogLevel.error] : LogLevelFilter }
  filter.allows .info ≡ true
  filter.allows .error ≡ true
  filter.allows .warn ≡ false

test "Logger.withMinLevel sets filter" := do
  let logger := Logger.new |>.withMinLevel LogLevel.warn
  logger.filter.minLevel ≡ some LogLevel.warn

test "Logger renders without crash" := do
  let logger := Logger.new
    |>.add (LogEntry.new .info "Application started" (some "12:00:00"))
    |>.add (LogEntry.new .warn "Low memory warning" (some "12:00:01"))
    |>.add (LogEntry.new .error "Connection failed" (some "12:00:02"))
  let buf := renderWidget logger 60 5
  buf.width ≡ 60

#generate_tests

end TerminusTests.LoggerTests
