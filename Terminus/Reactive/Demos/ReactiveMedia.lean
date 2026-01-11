/-
  Terminus Reactive - Media Demo
  Demonstrates Canvas, BigText, and Image widgets.
-/
import Terminus.Reactive

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

namespace Terminus.Reactive.Demos.ReactiveMedia

/-- Demo state for tracking which mode we're in -/
inductive DemoMode where
  | canvas
  | bigText
  | image
  deriving Repr, Inhabited, BEq

def DemoMode.label : DemoMode → String
  | .canvas => "Canvas"
  | .bigText => "BigText"
  | .image => "Image"

def DemoMode.next : DemoMode → DemoMode
  | .canvas => .bigText
  | .bigText => .image
  | .image => .canvas

/-- Path to the demo image -/
private def nibblePngPath : System.FilePath := "examples/nibble.png"

/-- Build the complete demo app -/
def app : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Track current demo mode
  let keyEvents ← useKeyEvent
  let modeEvent ← Event.filterM (fun kd =>
    kd.event.code == .tab || kd.event.code == .char 'n') keyEvents
  let voidModeEvent ← Event.voidM modeEvent

  let modeDyn ← Reactive.foldDyn (fun _ mode => mode.next) DemoMode.canvas voidModeEvent

  -- Create condition dynamics for each mode
  let isCanvasMode ← Dynamic.map' modeDyn (· == .canvas)
  let isBigTextMode ← Dynamic.map' modeDyn (· == .bigText)
  let isImageMode ← Dynamic.map' modeDyn (· == .image)

  -- Build the render function
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      heading1' "Terminus Reactive - Media Widgets Demo" theme

      -- Instructions
      row' (gap := 2) (style := {}) do
        text' "Press Tab or 'n' to switch demos" theme.captionStyle
        text' "Press Ctrl+C to exit" theme.captionStyle

      -- Show current mode
      emitDynamic do
        let mode ← modeDyn.sample
        pure (RNode.text s!"Current: {mode.label}" theme.primaryStyle)

      spacer' 0 1

      -- Canvas demo panel
      when' isCanvasMode do
        titledPanel' "Canvas Drawing" .rounded theme do
          column' (gap := 1) (style := {}) do
            text' "Braille-based sub-pixel drawing (2x4 dots per cell):" theme.captionStyle

            let grid := BrailleGrid.new 30 8
              |>.drawRect 2 2 20 20 { fg := .ansi .cyan }
              |>.drawLine 0 0 58 30 { fg := .ansi .green }
              |>.drawLine 58 0 0 30 { fg := .ansi .green }
              |>.drawCircle 30 15 10 { fg := .ansi .yellow }
              |>.fillRect 45 5 10 10 { fg := .ansi .magenta }
            staticCanvas' grid

            text' "Shapes: cyan rect, green X, yellow circle, magenta fill" theme.captionStyle

      -- BigText demo panel
      when' isBigTextMode do
        titledPanel' "BigText Rendering" .rounded theme do
          column' (gap := 1) (style := {}) do
            text' "Block font (8x8):" theme.captionStyle
            bigText' "HELLO" { font := .block, style := { fg := .ansi .cyan } }

            spacer' 0 1

            text' "Slant font (8x8 italic):" theme.captionStyle
            bigText' "LEAN4" { font := .slant, style := { fg := .ansi .green } }

            spacer' 0 1

            text' "Small font (4x4):" theme.captionStyle
            bigText' "TERMINUS" { font := .small, style := { fg := .ansi .yellow } }

      -- Image demo panel
      when' isImageMode do
        titledPanel' "Image Widget" .rounded theme do
          column' (gap := 1) (style := {}) do
            text' "Terminal image using iTerm2 protocol:" theme.captionStyle

            spacer' 0 1

            image' nibblePngPath {
              width := 40
              height := 15
              protocol := .iterm2
              altText := "[nibble.png - requires iTerm2/WezTerm]"
            }

            spacer' 0 1

            text' "Supported: iTerm2, WezTerm, Konsole, kitty" theme.captionStyle

  pure { render }

/-- Main entry point -/
def main : IO Unit := runReactiveApp app

end Terminus.Reactive.Demos.ReactiveMedia
