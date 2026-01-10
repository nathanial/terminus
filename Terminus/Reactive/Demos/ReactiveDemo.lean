/-
  Reactive Demo Widgets
  Shared widget tree for the reactive demo (used by tests and the executable).
-/
import Terminus.Reactive

open Terminus.Reactive
open Reactive Reactive.Host

def reactiveDemoApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Get events and log function
  let events ← getEvents
  let log := events.debugLog

  -- Get key events
  let keyEvents ← useKeyEvent

  -- DEBUG: Add a direct subscription to verify events are received
  let testRef ← SpiderM.liftIO (IO.mkRef 0)
  let _testUnsub ← SpiderM.liftIO <| keyEvents.subscribe fun _kd => do
    let n ← testRef.get
    log s!"[DIRECT-SUB] Direct subscription received key! count={n+1}"
    testRef.set (n + 1)

  -- Count key presses (with debug logging via IO)
  let countRef ← SpiderM.liftIO (IO.mkRef 0)
  let keyCount ← Reactive.foldDyn (fun _ n =>
    -- Note: We can't easily log here since foldDyn callback is pure
    -- But we track via the ref above
    n + 1
  ) 0 keyEvents

  -- Also subscribe to log when foldDyn would fire
  let _logUnsub ← SpiderM.liftIO <| keyEvents.subscribe fun _kd => do
    let n ← countRef.get
    log s!"[FOLDDYN-SUB] foldDyn should have updated, count would be {n+1}"
    countRef.set (n + 1)

  -- Track last key pressed
  let keyStrings ← Event.mapM (fun kd =>
    match kd.event.code with
    | .char c => s!"'{c}'"
    | .enter => "Enter"
    | .escape => "Escape"
    | .backspace => "Backspace"
    | .tab => "Tab"
    | .space => "Space"
    | .up => "Up"
    | .down => "Down"
    | .left => "Left"
    | .right => "Right"
    | .f n => s!"F{n}"
    | _ => "other"
  ) keyEvents
  let lastKey ← Reactive.holdDyn "none" keyStrings

  -- Get elapsed time for animation
  let elapsedMs ← useElapsedMs

  -- Create a cycling progress value (0.0 to 1.0 over 3 seconds, then resets)
  let tickEvents ← useTick
  let progress ← Reactive.foldDyn (fun td _ =>
    -- Cycle over 3000ms (3 seconds)
    let cycleMs := td.elapsedMs % 3000
    cycleMs.toFloat / 3000.0
  ) 0.0 tickEvents

  -- Build the UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Title
      text' "=== Reactive Demo ===" (theme.heading1Style)
      text' "Press keys to see reactive updates. Ctrl+C to quit." (theme.captionStyle)
      text' "" {}

      -- Animated progress bar
      text' "Animated Progress:" (theme.bodyStyle)
      -- Log progress value every render
      emitDynamic do
        let p ← progress.sample
        log s!"[RENDER] progress = {p}"
        pure RNode.empty
      dynProgressBar' progress {
        width := 30
        filledStyle := { fg := .ansi .green }
        emptyStyle := { fg := .ansi .brightBlack }
        percentageStyle := theme.captionStyle
      }
      text' "" {}

      -- Elapsed time
      row' (gap := 1) (style := {}) do
        text' "Elapsed:" (theme.bodyStyle)
        emitDynamic do
          let ms ← elapsedMs.sample
          let seconds := ms / 1000
          let minutes := seconds / 60
          let secs := seconds % 60
          pure (RNode.text s!"{minutes}:{String.mk (if secs < 10 then ['0'] else [])}{secs}" theme.primaryStyle)

      -- Key counter
      row' (gap := 1) (style := {}) do
        text' "Keys pressed:" (theme.bodyStyle)
        emitDynamic do
          let count ← keyCount.sample
          log s!"[RENDER] sampling keyCount = {count}"
          pure (RNode.text (toString count) theme.primaryStyle)

      -- Last key
      row' (gap := 1) (style := {}) do
        text' "Last key:" (theme.bodyStyle)
        emitDynamic do
          let key ← lastKey.sample
          pure (RNode.text key theme.primaryStyle)

      -- Instructions
      text' "" {}
      text' "Try pressing:" (theme.captionStyle)
      text' "  - Any letter or number" (theme.captionStyle)
      text' "  - Arrow keys" (theme.captionStyle)
      text' "  - Function keys (F1-F12)" (theme.captionStyle)

  pure { render }
