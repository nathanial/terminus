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

/-- Create the canvas demo panel -/
def canvasDemo : WidgetM Unit := do
  let theme := Theme.dark

  -- Create a canvas and draw some shapes
  titledPanel' "Canvas Drawing" theme do
    column' (gap := 1) (style := {}) do
      text' "Braille-based sub-pixel drawing:" theme.captionStyle

      -- Create a static canvas with pre-drawn shapes
      let grid := BrailleGrid.new 30 8
        |>.drawRect 2 2 20 20 { fg := .ansi .cyan }
        |>.drawLine 0 0 58 30 { fg := .ansi .green }
        |>.drawLine 58 0 0 30 { fg := .ansi .green }
        |>.drawCircle 30 15 10 { fg := .ansi .yellow }
        |>.fillRect 45 5 10 10 { fg := .ansi .magenta }
      staticCanvas' grid

      text' "Shapes: rectangle, X lines, circle, filled rect" theme.captionStyle

/-- Create the big text demo panel -/
def bigTextDemo : WidgetM Unit := do
  let theme := Theme.dark

  titledPanel' "BigText Rendering" theme do
    column' (gap := 1) (style := {}) do
      -- Block font
      text' "Block font (8x8):" theme.captionStyle
      bigText' "HELLO" { font := .block, style := { fg := .ansi .cyan } }

      spacer' 0 1

      -- Slant font
      text' "Slant font (8x8 italic):" theme.captionStyle
      bigText' "LEAN4" { font := .slant, style := { fg := .ansi .green } }

      spacer' 0 1

      -- Small font
      text' "Small font (4x4):" theme.captionStyle
      bigText' "TERMINUS" { font := .small, style := { fg := .ansi .yellow } }

      spacer' 0 1

      -- Custom characters
      text' "Custom chars:" theme.captionStyle
      bigText' "OK" { onChar := '#', offChar := some '.', style := { fg := .ansi .magenta } }

/-- Create the image demo panel -/
def imageDemo : WidgetM Unit := do
  let theme := Theme.dark

  titledPanel' "Image Widget" theme do
    column' (gap := 1) (style := {}) do
      text' "Images use terminal protocols (iTerm2, Sixel)" theme.captionStyle
      text' "Fallback alt-text shown when protocol not supported:" theme.captionStyle

      spacer' 0 1

      -- Show placeholder since we don't have an actual image
      imagePlaceholder' "sample.png" { width := 40, height := 10 }

      spacer' 0 1

      text' "Supported protocols:" theme.captionStyle
      text' "  - iTerm2 inline images" theme.bodyStyle
      text' "  - Sixel graphics" theme.bodyStyle

/-- Build the complete demo app -/
def app : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Track current demo mode
  let keyEvents ← useKeyEvent
  let modeEvent := keyEvents.filter (fun kd =>
    kd.event.code == .tab || kd.event.code == .char 'n')
    |>.map (fun _ => ())

  let modeDyn ← Reactive.foldDyn (fun _ mode => mode.next) DemoMode.canvas modeEvent

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

      -- Demo content based on mode
      emitDynamic do
        let mode ← modeDyn.sample
        -- We return empty here and use a switch below
        pure RNode.empty

    -- Render the appropriate demo based on mode
    emitDynamic do
      let mode ← modeDyn.sample
      match mode with
      | .canvas =>
        -- We need to render the canvas demo
        -- For now, just return a static representation
        let grid := BrailleGrid.new 30 8
          |>.drawRect 2 2 20 20 { fg := .ansi .cyan }
          |>.drawLine 0 0 58 30 { fg := .ansi .green }
          |>.drawLine 58 0 0 30 { fg := .ansi .green }
          |>.drawCircle 30 15 10 { fg := .ansi .yellow }
        let rows := grid.toRNodes
        pure (RNode.block (some "Canvas Drawing") .rounded { fg := .ansi .brightBlack }
          (RNode.column 0 {} rows))
      | .bigText =>
        let textRows := renderBigTextLine "DEMO" { font := .block, style := { fg := .ansi .cyan } }
        pure (RNode.block (some "BigText Rendering") .rounded { fg := .ansi .brightBlack }
          (RNode.column 0 {} textRows))
      | .image =>
        pure (RNode.block (some "Image Widget") .rounded { fg := .ansi .brightBlack }
          (RNode.text "[Image placeholder - use iTerm2/WezTerm for real images]" Style.dim))

  pure { render }

/-- Main entry point -/
def main : IO Unit := runReactiveApp app

end Terminus.Reactive.Demos.ReactiveMedia
