import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Media Tab Content -/

private def nibblePngPath : System.FilePath := "examples/nibble.png"

def mediaContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Canvas drawing, big text, and images. Press N to switch." theme.captionStyle

    -- Track media mode
    let keyEvents ← useKeyEventW
    let modeEvent ← Event.filterM (fun kd => kd.event.code == .char 'n' || kd.event.code == .char 'N') keyEvents
    let voidModeEvent ← Event.voidM modeEvent
    let modeDyn ← Reactive.foldDyn (fun _ mode => (mode + 1) % 3) 0 voidModeEvent

    let isCanvas ← Dynamic.map' modeDyn (· == 0)
    let isBigText ← Dynamic.map' modeDyn (· == 1)
    let isImage ← Dynamic.map' modeDyn (· == 2)

    let node ← modeDyn.map' (fun mode =>
      let label := match mode with
        | 0 => "Canvas"
        | 1 => "BigText"
        | _ => "Image"
      RNode.text s!"Current: {label}" theme.primaryStyle
    )
    emit node

    spacer' 0 1

    -- Canvas
    when' isCanvas do
      titledBlock' "Canvas Drawing" .rounded theme none do
        let grid := BrailleGrid.new 25 6
          |>.drawRect 2 2 15 15 { fg := .ansi .cyan }
          |>.drawLine 0 0 48 22 { fg := .ansi .green }
          |>.drawCircle 25 11 8 { fg := .ansi .yellow }
        staticCanvas' grid
        text' "Shapes: rect, line, circle" theme.captionStyle

    -- BigText
    when' isBigText do
      titledBlock' "BigText Rendering" .rounded theme none do
        bigText' "LEAN" { font := .block, style := { fg := .ansi .cyan } }
        spacer' 0 1
        bigText' "TERMINUS" { font := .small, style := { fg := .ansi .green } }

    -- Image
    when' isImage do
      titledBlock' "Image Widget" .rounded theme none do
        image' nibblePngPath {
          width := 30
          height := 10
          protocol := .iterm2
          altText := "[nibble.png - requires iTerm2/WezTerm]"
        }
        text' "Supports iTerm2, WezTerm, kitty" theme.captionStyle
