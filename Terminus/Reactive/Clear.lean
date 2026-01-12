/-
  Terminus Reactive - Clear Widget
  Utility widget for clearing/filling areas.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Clear Configuration -/

/-- Configuration for clear widget. -/
structure ClearConfig where
  /-- Style to fill with (including background color). -/
  style : Style := {}
  /-- Character to fill with. -/
  fillChar : Char := ' '
  /-- Width of area to clear. -/
  width : Nat := 1
  /-- Height of area to clear. -/
  height : Nat := 1
  deriving Repr, Inhabited

/-! ## Clear Widget -/

/-- Create a clear widget that fills an area with styled empty space.

    Useful for clearing areas before redrawing or creating backgrounds.

    Example:
    ```
    -- Clear a 10x5 area with blue background
    clear' { width := 10, height := 5, style := { bg := .ansi .blue } }
    ```
-/
def clear' (config : ClearConfig := {}) : WidgetM Unit := do
  let fillStr := String.ofList (List.replicate config.width config.fillChar)
  let lines := (List.range config.height).map fun _ =>
    RNode.text fillStr config.style
  emitStatic (RNode.column 0 {} lines.toArray)

/-- Create a horizontal spacer.

    Example:
    ```
    hSpace' 10  -- Creates 10 spaces
    ```
-/
def hSpace' (width : Nat) (style : Style := {}) : WidgetM Unit := do
  emitStatic (RNode.text (String.ofList (List.replicate width ' ')) style)

/-- Create a vertical spacer.

    Example:
    ```
    vSpace' 3  -- Creates 3 empty lines
    ```
-/
def vSpace' (height : Nat) (style : Style := {}) : WidgetM Unit := do
  let lines := (List.range height).map fun _ =>
    RNode.text " " style
  emitStatic (RNode.column 0 {} lines.toArray)

/-- Create a filled rectangle.

    Example:
    ```
    filledRect' 20 5 '█' { fg := .ansi .blue }
    ```
-/
def filledRect' (width height : Nat) (char : Char := ' ') (style : Style := {}) : WidgetM Unit := do
  let line := String.ofList (List.replicate width char)
  let lines := (List.range height).map fun _ =>
    RNode.text line style
  emitStatic (RNode.column 0 {} lines.toArray)

/-- Create a background layer.

    This creates a full-size background that can be placed behind other content.

    Example:
    ```
    layer' do
      background' { bg := .ansi .blue }
      -- Other content on top
      text' "Hello"
    ```
-/
def background' (style : Style) : WidgetM Unit := do
  -- Just emit a styled empty space - the layout system handles sizing
  emitStatic (RNode.text " " style)

/-! ## Separator Widgets -/

/-- Create a horizontal separator line.

    Example:
    ```
    hSeparator' 40 '─' { fg := .ansi .brightBlack }
    ```
-/
def hSeparator' (width : Nat) (char : Char := '─') (style : Style := {}) : WidgetM Unit := do
  emitStatic (RNode.text (String.ofList (List.replicate width char)) style)

/-- Create a vertical separator line.

    Example:
    ```
    vSeparator' 10 '│' { fg := .ansi .brightBlack }
    ```
-/
def vSeparator' (height : Nat) (char : Char := '│') (style : Style := {}) : WidgetM Unit := do
  let charStr := String.singleton char
  let lines := (List.range height).map fun _ =>
    RNode.text charStr style
  emitStatic (RNode.column 0 {} lines.toArray)

end Terminus.Reactive
