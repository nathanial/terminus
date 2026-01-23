-- TerminusTests.Reactive.FeedbackTests: Tests for Spinner, Logger, and Notification widgets

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.FeedbackTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Feedback Widget Tests"

-- ============================================================================
-- Spinner Tests
-- ============================================================================

test "SpinnerStyle.frames returns correct frame arrays" := do
  -- Test dots style
  let dotsFrames := SpinnerStyle.dots.frames
  dotsFrames.size ≡ 10

  -- Test ascii style
  let asciiFrames := SpinnerStyle.ascii.frames
  asciiFrames.size ≡ 4

  -- Test blocks style
  let blocksFrames := SpinnerStyle.blocks.frames
  blocksFrames.size ≡ 4

test "SpinnerStyle.frameAt wraps correctly" := do
  -- Test that frame index wraps around
  let style := SpinnerStyle.ascii  -- Has 4 frames: | / - \
  style.frameAt 0 ≡ "|"
  style.frameAt 1 ≡ "/"
  style.frameAt 2 ≡ "-"
  style.frameAt 3 ≡ "\\"
  style.frameAt 4 ≡ "|"   -- Wraps back to 0
  style.frameAt 5 ≡ "/"   -- Wraps to 1

test "spinner' renders current frame" := do
  runSpider do
    let (events, _) ← createInputs
    let (neverEvent, _) ← Reactive.newTriggerEvent (t := Spider) (a := Nat)
    let frameIndexDyn ← Reactive.holdDyn 0 neverEvent

    let (_result, render) ← (runWidget do
      spinner' (some "Loading") frameIndexDyn { style := .ascii }
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Should contain the first ASCII frame "|" and the label
    SpiderM.liftIO (ensure (rnodeContainsText node "|") "expected | frame")
    SpiderM.liftIO (ensure (rnodeContainsText node "Loading") "expected label")

test "animatedSpinner' renders with label" := do
  runSpider do
    let (events, _) ← createInputs

    let (_result, render) ← (runWidget do
      animatedSpinner' (some "Processing...") 80 { style := .dots }
    ).run events

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "Processing...") "expected label")

-- ============================================================================
-- Logger Tests
-- ============================================================================

test "LogLevel.tag returns correct tags" := do
  Terminus.Reactive.LogLevel.debug.tag ≡ "[DEBUG]"
  Terminus.Reactive.LogLevel.info.tag ≡ "[INFO]"
  Terminus.Reactive.LogLevel.warn.tag ≡ "[WARN]"
  Terminus.Reactive.LogLevel.error.tag ≡ "[ERROR]"

test "LogEntry.new creates entries correctly" := do
  let entry := Terminus.Reactive.LogEntry.new .info "Test message" (some "12:00:00")
  entry.level ≡ .info
  entry.message ≡ "Test message"
  entry.timestamp ≡ some "12:00:00"

test "LogEntry convenience constructors work" := do
  let debug := Terminus.Reactive.LogEntry.debug "Debug msg"
  debug.level ≡ .debug
  debug.message ≡ "Debug msg"

  let error := Terminus.Reactive.LogEntry.error "Error msg"
  error.level ≡ .error

test "logger' starts with empty entries" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, render) ← (runWidget do
      logger' {}
    ).run events

    let entries ← SpiderM.liftIO result.entries.sample
    SpiderM.liftIO (entries.size ≡ 0)

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "no log entries") "expected empty message")

test "logger' log function adds entries" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, _render) ← (runWidget do
      logger' {}
    ).run events

    -- Log some messages
    SpiderM.liftIO <| result.log .info "First message"
    SpiderM.liftIO <| result.log .error "Second message"

    let entries ← SpiderM.liftIO result.entries.sample
    SpiderM.liftIO (entries.size ≡ 2)

test "logger' respects maxLines limit" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, _render) ← (runWidget do
      logger' { maxLines := 3 }
    ).run events

    -- Log more than maxLines
    SpiderM.liftIO <| result.log .info "Message 1"
    SpiderM.liftIO <| result.log .info "Message 2"
    SpiderM.liftIO <| result.log .info "Message 3"
    SpiderM.liftIO <| result.log .info "Message 4"

    let entries ← SpiderM.liftIO result.entries.sample
    -- Should be truncated to maxLines
    SpiderM.liftIO (entries.size ≡ 3)

test "logger' clear function works" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, _render) ← (runWidget do
      logger' {}
    ).run events

    -- Add entries
    SpiderM.liftIO <| result.log .info "Test"
    let entries1 ← SpiderM.liftIO result.entries.sample
    SpiderM.liftIO (entries1.size ≡ 1)

    -- Clear
    SpiderM.liftIO result.clear
    let entries2 ← SpiderM.liftIO result.entries.sample
    SpiderM.liftIO (entries2.size ≡ 0)

test "logger' renders entries with level tags" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, render) ← (runWidget do
      logger' { showLevel := true }
    ).run events

    SpiderM.liftIO <| result.log .error "Something bad happened"

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "[ERROR]") "expected error tag")
    SpiderM.liftIO (ensure (rnodeContainsText node "Something bad happened") "expected message")

-- ============================================================================
-- Notification Tests
-- ============================================================================

test "NotificationLevel.symbol returns correct symbols" := do
  NotificationLevel.info.symbol ≡ "i"
  NotificationLevel.success.symbol ≡ "+"
  NotificationLevel.warning.symbol ≡ "!"
  NotificationLevel.error.symbol ≡ "x"

test "notifications' starts empty" := do
  runSpider do
    let (events, _) ← createInputs

    let (_result, render) ← (runWidget do
      notifications' {}
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Empty notifications should render as RNode.empty or nothing visible
    SpiderM.liftIO (ensure (!rnodeContainsText node "[i]" && !rnodeContainsText node "[+]") "expected empty")

test "notifications' show function adds notification" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, render) ← (runWidget do
      notifications' { durationMs := 0 }  -- No auto-dismiss
    ).run events

    -- Show a notification
    SpiderM.liftIO <| result.«show» .success "Operation completed"

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "[+]") "expected success symbol")
    SpiderM.liftIO (ensure (rnodeContainsText node "Operation completed") "expected message")

test "notifications' respects maxVisible" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, render) ← (runWidget do
      notifications' { durationMs := 0, maxVisible := 2 }
    ).run events

    -- Show more than maxVisible
    SpiderM.liftIO <| result.«show» .info "Message 1"
    SpiderM.liftIO <| result.«show» .info "Message 2"
    SpiderM.liftIO <| result.«show» .info "Message 3"

    let node ← SpiderM.liftIO render.sample
    -- Should only show last 2 (Message 2 and Message 3)
    SpiderM.liftIO (ensure (!rnodeContainsText node "Message 1") "should not have Message 1")
    SpiderM.liftIO (ensure (rnodeContainsText node "Message 2") "should have Message 2")
    SpiderM.liftIO (ensure (rnodeContainsText node "Message 3") "should have Message 3")

test "notifications' dismiss removes oldest" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, render) ← (runWidget do
      notifications' { durationMs := 0, maxVisible := 5 }
    ).run events

    SpiderM.liftIO <| result.«show» .info "First"
    SpiderM.liftIO <| result.«show» .info "Second"

    -- Dismiss oldest
    SpiderM.liftIO result.dismiss

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (!rnodeContainsText node "First") "First should be dismissed")
    SpiderM.liftIO (ensure (rnodeContainsText node "Second") "Second should remain")

test "notifications' dismissAll clears all" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, render) ← (runWidget do
      notifications' { durationMs := 0 }
    ).run events

    SpiderM.liftIO <| result.«show» .info "A"
    SpiderM.liftIO <| result.«show» .info "B"

    -- Dismiss all
    SpiderM.liftIO result.dismissAll

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (!rnodeContainsText node "[i]") "all should be dismissed")

test "notifications' shows different level styles" := do
  runSpider do
    let (events, _) ← createInputs

    let (result, render) ← (runWidget do
      notifications' { durationMs := 0, maxVisible := 4 }
    ).run events

    SpiderM.liftIO <| result.«show» .info "Info msg"
    SpiderM.liftIO <| result.«show» .success "Success msg"
    SpiderM.liftIO <| result.«show» .warning "Warning msg"
    SpiderM.liftIO <| result.«show» .error "Error msg"

    let node ← SpiderM.liftIO render.sample
    SpiderM.liftIO (ensure (rnodeContainsText node "[i]") "should have info symbol")
    SpiderM.liftIO (ensure (rnodeContainsText node "[+]") "should have success symbol")
    SpiderM.liftIO (ensure (rnodeContainsText node "[!]") "should have warning symbol")
    SpiderM.liftIO (ensure (rnodeContainsText node "[x]") "should have error symbol")



end TerminusTests.Reactive.FeedbackTests
