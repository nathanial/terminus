import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Basics Tab Content -/

def basicsContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Demonstrates progress bars, animations, and time tracking." theme.captionStyle

    row' (gap := 3) {} do
      -- Progress bars
      column' (gap := 1) {} do
        titledBlock' "Progress Bars" .rounded theme none do
          -- Animated progress
          let tickEvents ← useTickW
          let progress ← Reactive.foldDyn (fun td _ =>
            let cycleMs := td.elapsedMs % 3000
            cycleMs.toFloat / 3000.0
          ) 0.0 tickEvents

          text' "Cycling (3s):" theme.captionStyle
          dynProgressBar' progress {
            width := 25
            filledStyle := { fg := .ansi .green }
            emptyStyle := { fg := .ansi .brightBlack }
            showPercentage := true
          }

          spacer' 0 1

          text' "Static gauges:" theme.captionStyle
          gauge' 0.65 { width := 25, filledStyle := { fg := .ansi .cyan } }
          gauge' 0.40 { width := 25, filledStyle := { fg := .ansi .yellow } }
          lineGauge' 0.75 { width := 25, label := some "Memory" }

      -- Animations
      column' (gap := 1) {} do
        titledBlock' "Animations" .rounded theme none do
          let pulse ← usePulse 500
          let colorCycle ← useCycle 2000

          row' (gap := 1) {} do
            text' "Pulse:" theme.captionStyle
            let node ← pulse.map' (fun on =>
              if on then
                RNode.text "●" { fg := .ansi .green }
              else
                RNode.text "○" { fg := .ansi .brightBlack }
            )
            emit node

          row' (gap := 1) {} do
            text' "Cycle:" theme.captionStyle
            let node ← colorCycle.map' (fun idx =>
              let colors := #[
                Color.ansi .red, Color.ansi .green,
                Color.ansi .yellow, Color.ansi .blue,
                Color.ansi .magenta, Color.ansi .cyan
              ]
              let idxNat := idx.toUInt64.toNat
              let safeIdx := idxNat % colors.size
              let color := colors[safeIdx]!
              RNode.text "████" { fg := color }
            )
            emit node

      -- Time tracking
      column' (gap := 1) {} do
        titledBlock' "Time & Keys" .rounded theme none do
          let elapsedMs ← useElapsedMsW
          let keyEvents ← useKeyEventW
          let keyCount ← Reactive.foldDyn (fun _ n => n + 1) 0 keyEvents

          let keyStrings ← Event.mapM (fun (kd : KeyData) => s!"{reprStr kd.event.code}") keyEvents
          let lastKey ← Reactive.holdDyn "(none)" keyStrings

          row' (gap := 1) {} do
            text' "Uptime:" theme.captionStyle
            let node ← elapsedMs.map' (fun ms =>
              let secs := ms / 1000
              RNode.text s!"{secs}s" theme.primaryStyle
            )
            emit node

          row' (gap := 1) {} do
            text' "Keys:" theme.captionStyle
            let node ← keyCount.map' (fun count =>
              RNode.text s!"{count}" theme.primaryStyle
            )
            emit node

          row' (gap := 1) {} do
            text' "Last:" theme.captionStyle
            let node ← lastKey.map' (fun key =>
              RNode.text key theme.primaryStyle
            )
            emit node

    spacer' 0 1

    row' (gap := 2) {} do
      -- Paragraphs
      column' (gap := 1) {} do
        titledBlock' "Paragraph" .rounded theme none do
          paragraph' "This is a paragraph widget that supports automatic word wrapping. It handles long text gracefully by wrapping it to the next line." {
            maxWidth := some 30
            alignment := .left
            style := theme.bodyStyle
          }

      -- Dividers
      column' (gap := 1) {} do
        titledBlock' "Dividers" .rounded theme none do
          text' "Horizontal:" theme.captionStyle
          text' "━━━━━━━━━━━━━━━━━━━━" theme.bodyStyle
          spacer' 0 1
          text' "Vertical:" theme.captionStyle
          text' "┃" theme.bodyStyle
          text' "┃" theme.bodyStyle
          text' "┃" theme.bodyStyle

      -- Clear
      column' (gap := 1) {} do
        titledBlock' "Clear" .rounded theme none do
          text' "Clears a styled region:" theme.captionStyle
          clear' { width := 14, height := 3, fillChar := '.', style := { fg := .ansi .cyan } }
