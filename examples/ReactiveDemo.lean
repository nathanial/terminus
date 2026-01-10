/-
  Reactive Demo
  Demonstrates the Terminus.Reactive widget system.
-/
import Terminus.Reactive

open Terminus.Reactive
open Reactive Reactive.Host

def main : IO Unit := runReactiveApp do
  let theme := Theme.dark

  -- Get key events
  let keyEvents ← useKeyEvent

  -- Count key presses
  let keyCount ← Reactive.foldDyn (fun _ n => n + 1) 0 keyEvents

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

  -- Build the UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Title
      text' "=== Reactive Demo ===" (theme.heading1Style)
      text' "Press keys to see reactive updates. Ctrl+C to quit." (theme.captionStyle)
      text' "" {}

      -- Key counter
      row' (gap := 1) (style := {}) do
        text' "Keys pressed:" (theme.bodyStyle)
        emitDynamic do
          let count ← keyCount.sample
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
