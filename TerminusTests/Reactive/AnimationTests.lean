-- TerminusTests.Reactive.AnimationTests: Animation utility tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.AnimationTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Animation Tests"

-- ============================================================================
-- AnimPhase Tests
-- ============================================================================

test "AnimPhase.idle isIdle returns true" := do
  let phase := AnimPhase.idle
  ensure phase.isIdle "idle should be idle"

test "AnimPhase.phase isIdle returns false" := do
  let phase := AnimPhase.phase "test" 0.5
  ensure (!phase.isIdle) "phase should not be idle"

test "AnimPhase.isPhase checks name" := do
  let phase := AnimPhase.phase "flash" 0.5
  ensure (phase.isPhase "flash") "should match flash"
  ensure (!phase.isPhase "other") "should not match other"

test "AnimPhase.getProgress returns progress" := do
  let phase := AnimPhase.phase "test" 0.75
  ensure (phase.getProgress == 0.75) "should return 0.75"

test "AnimPhase.getName returns name" := do
  let phase := AnimPhase.phase "fade" 0.5
  ensure (phase.getName == "fade") "should return fade"

test "AnimPhase.idle getProgress returns 0" := do
  let phase := AnimPhase.idle
  ensure (phase.getProgress == 0.0) "idle progress should be 0"

test "AnimPhase.idle getName returns empty" := do
  let phase := AnimPhase.idle
  ensure (phase.getName == "") "idle name should be empty"

-- ============================================================================
-- Easing Function Tests
-- ============================================================================

test "Easing.linear is identity" := do
  ensure (Easing.linear 0.0 == 0.0) "linear(0) should be 0"
  ensure (Easing.linear 0.5 == 0.5) "linear(0.5) should be 0.5"
  ensure (Easing.linear 1.0 == 1.0) "linear(1) should be 1"

test "Easing.easeInQuad starts slow" := do
  let mid := Easing.easeInQuad 0.5
  ensure (mid < 0.5) "easeInQuad(0.5) should be less than 0.5"
  ensure (Easing.easeInQuad 0.0 == 0.0) "easeInQuad(0) should be 0"
  ensure (Easing.easeInQuad 1.0 == 1.0) "easeInQuad(1) should be 1"

test "Easing.easeOutQuad ends slow" := do
  let mid := Easing.easeOutQuad 0.5
  ensure (mid > 0.5) "easeOutQuad(0.5) should be greater than 0.5"
  ensure (Easing.easeOutQuad 0.0 == 0.0) "easeOutQuad(0) should be 0"
  ensure (Easing.easeOutQuad 1.0 == 1.0) "easeOutQuad(1) should be 1"

test "Easing.easeInOutQuad symmetric" := do
  ensure (Easing.easeInOutQuad 0.0 == 0.0) "easeInOutQuad(0) should be 0"
  ensure (Easing.easeInOutQuad 0.5 == 0.5) "easeInOutQuad(0.5) should be 0.5"
  ensure (Easing.easeInOutQuad 1.0 == 1.0) "easeInOutQuad(1) should be 1"

test "Easing.easeInCubic starts slower than quad" := do
  let cubicMid := Easing.easeInCubic 0.5
  let quadMid := Easing.easeInQuad 0.5
  ensure (cubicMid < quadMid) "cubic should be slower than quad at midpoint"

test "Easing.easeOutCubic bounds correct" := do
  ensure (Easing.easeOutCubic 0.0 == 0.0) "easeOutCubic(0) should be 0"
  ensure (Easing.easeOutCubic 1.0 == 1.0) "easeOutCubic(1) should be 1"

test "Easing.easeInOutCubic bounds correct" := do
  ensure (Easing.easeInOutCubic 0.0 == 0.0) "easeInOutCubic(0) should be 0"
  ensure (Easing.easeInOutCubic 1.0 == 1.0) "easeInOutCubic(1) should be 1"

test "Easing.bounce ends at 1" := do
  ensure (Easing.bounce 1.0 == 1.0) "bounce(1) should be 1"
  ensure (Easing.bounce 0.0 == 0.0) "bounce(0) should be 0"

-- ============================================================================
-- Animation Config Tests
-- ============================================================================

test "AnimationConfig default values" := do
  let config : AnimationConfig := { durationMs := 1000 }
  ensure (config.durationMs == 1000) "duration should be 1000"
  ensure (config.loop == false) "loop should default to false"
  ensure (config.easing 0.5 == 0.5) "default easing should be linear"

test "AnimationConfig with custom easing" := do
  let config : AnimationConfig := {
    durationMs := 500
    easing := Easing.easeInQuad
  }
  ensure (config.easing 0.5 < 0.5) "easing should be quadratic"

test "AnimationConfig with loop" := do
  let config : AnimationConfig := { durationMs := 100, loop := true }
  ensure config.loop "loop should be true"

-- ============================================================================
-- WidgetM Animation Tests
-- ============================================================================

test "useAnimationW works in WidgetM" := do
  runSpider do
    let (events, _) ← createInputs
    let (animResult, _render) ← (runWidget do
      let (trigger, _) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let anim ← useAnimationW { durationMs := 100 } trigger
      emitStatic RNode.empty
      pure anim
    ).run events

    let progress ← SpiderM.liftIO animResult.progress.sample
    SpiderM.liftIO (ensure (progress == 0.0) "should start at 0")

test "usePulseW works in WidgetM" := do
  runSpider do
    let (events, _) ← createInputs
    let (pulseDyn, _render) ← (runWidget do
      let p ← usePulseW 500
      emitStatic RNode.empty
      pure p
    ).run events

    let pulse ← SpiderM.liftIO pulseDyn.sample
    SpiderM.liftIO (ensure (pulse == false) "should start false")

test "useCycleW works in WidgetM" := do
  runSpider do
    let (events, _) ← createInputs
    let (cycleDyn, _render) ← (runWidget do
      let c ← useCycleW 1000
      emitStatic RNode.empty
      pure c
    ).run events

    let cycle ← SpiderM.liftIO cycleDyn.sample
    SpiderM.liftIO (ensure (cycle >= 0.0 && cycle <= 1.0) "cycle in range")

test "useAnimationPhasesW works in WidgetM" := do
  runSpider do
    let (events, _) ← createInputs
    let (phaseDyn, _render) ← (runWidget do
      let (trigger, _) ← Reactive.newTriggerEvent (t := Spider) (a := String)
      let p ← useAnimationPhasesW #[("test", 100)] trigger
      emitStatic RNode.empty
      pure p
    ).run events

    let phase ← SpiderM.liftIO phaseDyn.sample
    SpiderM.liftIO (ensure phase.isIdle "should start idle")

test "useBlinkingCursorW works in WidgetM" := do
  runSpider do
    let (events, _) ← createInputs
    let (blinkDyn, _render) ← (runWidget do
      let b ← useBlinkingCursorW
      emitStatic RNode.empty
      pure b
    ).run events

    let blink ← SpiderM.liftIO blinkDyn.sample
    SpiderM.liftIO (ensure (blink == false) "should start false")

test "useCountdownW works in WidgetM" := do
  runSpider do
    let (events, _) ← createInputs
    let ((remainingDyn, runningDyn), _render) ← (runWidget do
      let (trigger, _) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let result ← useCountdownW 1000 trigger
      emitStatic RNode.empty
      pure result
    ).run events

    let remaining ← SpiderM.liftIO remainingDyn.sample
    let running ← SpiderM.liftIO runningDyn.sample
    SpiderM.liftIO (ensure (remaining == 1000) "should be 1000")
    SpiderM.liftIO (ensure (!running) "should not be running")

test "useInterpolationW works in WidgetM" := do
  runSpider do
    let (events, _) ← createInputs
    let (interpResult, _render) ← (runWidget do
      let (trigger, _) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let result ← useInterpolationW 100 0.0 100.0 trigger
      emitStatic RNode.empty
      pure result
    ).run events

    let value ← SpiderM.liftIO interpResult.value.sample
    SpiderM.liftIO (ensure (value == 0.0) "should start at 0.0")

test "AnimationResult has correct structure" := do
  runSpider do
    let (events, _) ← createInputs
    let (animResult, _render) ← (runWidget do
      let (trigger, _) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let anim ← useAnimationW { durationMs := 100 } trigger
      emitStatic RNode.empty
      pure anim
    ).run events

    -- Test all fields exist and have correct initial values
    let progress ← SpiderM.liftIO animResult.progress.sample
    let running ← SpiderM.liftIO animResult.isRunning.sample
    SpiderM.liftIO (ensure (progress == 0.0) "progress should be 0")
    SpiderM.liftIO (ensure (!running) "should not be running")

test "InterpolatedResult has correct structure" := do
  runSpider do
    let (events, _) ← createInputs
    let (interpResult, _render) ← (runWidget do
      let (trigger, _) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
      let result ← useInterpolationW 100 10.0 50.0 trigger
      emitStatic RNode.empty
      pure result
    ).run events

    let value ← SpiderM.liftIO interpResult.value.sample
    let running ← SpiderM.liftIO interpResult.isRunning.sample
    SpiderM.liftIO (ensure (value == 10.0) "value should be fromVal")
    SpiderM.liftIO (ensure (!running) "should not be running")



end TerminusTests.Reactive.AnimationTests
