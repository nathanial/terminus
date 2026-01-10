/-
  Terminus Reactive - Core Types
  Types for FRP-based terminal widgets.
-/
import Terminus
import Trellis
import Reactive

-- Note: We open Reactive for access to exported functions (Reactive.holdDyn, Reactive.newTriggerEvent, etc.)
-- but still use fully qualified Reactive.Event, Reactive.Dynamic for clarity vs Terminus.Event.

namespace Terminus.Reactive

open Reactive Reactive.Host

/-! ## Render Node Tree

RNode is the core widget tree type for reactive terminal UIs.
Similar to Afferent.Arbor.Widget but designed for terminal cell output.
-/

/-- Unique identifier for render nodes (used for layout indexing). -/
structure NodeId where
  id : Nat
  deriving Repr, BEq, Hashable, Inhabited

/-- Render node tree - describes the visual structure of terminal UI.
    This is rendered to a Buffer after layout computation. -/
inductive RNode where
  /-- Text content with styling. -/
  | text (content : String) (style : Style)
  /-- Block container with optional title and border. -/
  | block (title : Option String) (borderType : BorderType) (borderStyle : Style)
      (child : RNode)
  /-- Row container (horizontal flex layout). -/
  | row (gap : Nat) (style : Trellis.BoxConstraints) (children : Array RNode)
  /-- Column container (vertical flex layout). -/
  | column (gap : Nat) (style : Trellis.BoxConstraints) (children : Array RNode)
  /-- Fixed-size spacer. -/
  | spacer (width : Nat) (height : Nat)
  /-- Empty placeholder (zero size). -/
  | empty
  deriving Repr, Inhabited

namespace RNode

/-- Create a text node with default style. -/
def plain (content : String) : RNode := .text content {}

/-- Create a styled text node. -/
def styled (content : String) (fg : Color := .default) (bg : Color := .default)
    (modifier : Modifier := {}) : RNode :=
  .text content { fg, bg, modifier }

/-- Create a spacer node. -/
def space (w h : Nat := 1) : RNode := .spacer w h

end RNode

/-! ## Theme

Theme defines consistent styling for terminal UI components.
-/

/-- Terminal UI theme with colors and styles for components. -/
structure Theme where
  /-- Background color for the terminal. -/
  background : Color := .default
  /-- Foreground text color. -/
  foreground : Color := .default
  /-- Primary accent color (buttons, links). -/
  primary : Color := .ansi .cyan
  /-- Secondary accent color. -/
  secondary : Color := .ansi .blue
  /-- Success/positive color. -/
  success : Color := .ansi .green
  /-- Warning color. -/
  warning : Color := .ansi .yellow
  /-- Error/danger color. -/
  error : Color := .ansi .red
  /-- Muted/caption color. -/
  muted : Color := .ansi .brightBlack
  /-- Border color. -/
  border : Color := .ansi .brightBlack
  deriving Repr, Inhabited

namespace Theme

/-- Default dark theme. -/
def dark : Theme := {}

/-- Light theme variant. -/
def light : Theme := {
  background := .ansi .white
  foreground := .ansi .black
  muted := .ansi .brightBlack
  border := .ansi .brightBlack
}

/-- Get style for heading level 1. -/
def heading1Style (t : Theme) : Style :=
  { fg := t.foreground, modifier := { bold := true } }

/-- Get style for heading level 2. -/
def heading2Style (t : Theme) : Style :=
  { fg := t.foreground, modifier := { bold := true } }

/-- Get style for heading level 3. -/
def heading3Style (t : Theme) : Style :=
  { fg := t.foreground }

/-- Get style for body text. -/
def bodyStyle (t : Theme) : Style :=
  { fg := t.foreground }

/-- Get style for caption/muted text. -/
def captionStyle (t : Theme) : Style :=
  { fg := t.muted }

/-- Get style for primary elements. -/
def primaryStyle (t : Theme) : Style :=
  { fg := t.primary }

/-- Get style for borders. -/
def borderStyle (t : Theme) : Style :=
  { fg := t.border }

end Theme

/-! ## Event Data Types

Wrappers for terminal events with optional context.
-/

/-- Key event with optional focus context. -/
structure KeyData where
  /-- The keyboard event. -/
  event : KeyEvent
  /-- Currently focused widget name (for input routing). -/
  focusedWidget : Option String := none
  deriving Repr, Inhabited

/-- Mouse event with position context. -/
structure MouseData where
  /-- The mouse event. -/
  event : MouseEvent
  deriving Repr, Inhabited

/-- Resize event data. -/
structure ResizeData where
  /-- New terminal width in cells. -/
  width : Nat
  /-- New terminal height in cells. -/
  height : Nat
  deriving Repr, Inhabited

/-- Tick event data (fired each frame). -/
structure TickData where
  /-- Frame number since app start. -/
  frame : Nat
  /-- Elapsed time in milliseconds since app start. -/
  elapsedMs : Nat
  deriving Repr, Inhabited

/-! ## Component Registry

Registry for auto-generating unique widget names and tracking focusable components.
-/

/-- Registry for component names and focus management. -/
structure ComponentRegistry where
  private mk ::
  /-- Counter for generating unique IDs. -/
  idCounter : IO.Ref Nat
  /-- Names of focusable input widgets. -/
  inputNames : IO.Ref (Array String)
  /-- Names of all interactive widgets. -/
  interactiveNames : IO.Ref (Array String)
  /-- Currently focused input (by auto-generated name). -/
  focusedInput : Reactive.Dynamic Spider (Option String)
  /-- Trigger to change focus. -/
  fireFocus : Option String → IO Unit

/-- Create a new component registry. -/
def ComponentRegistry.create : SpiderM ComponentRegistry := do
  let idCounter ← SpiderM.liftIO <| IO.mkRef 0
  let inputNames ← SpiderM.liftIO <| IO.mkRef #[]
  let interactiveNames ← SpiderM.liftIO <| IO.mkRef #[]
  let (focusEvent, fireFocus) ← Reactive.newTriggerEvent (t := Spider) (a := Option String)
  let focusedInput ← Reactive.holdDyn none focusEvent
  pure { idCounter, inputNames, interactiveNames, focusedInput, fireFocus }

/-- Register a component and get an auto-generated name.
    - `namePrefix`: Component type prefix (e.g., "button", "text-input")
    - `isInput`: Whether this is a focusable input widget
    - `isInteractive`: Whether this widget responds to clicks -/
def ComponentRegistry.register (reg : ComponentRegistry) (namePrefix : String)
    (isInput : Bool := false) (isInteractive : Bool := true) : IO String := do
  let id ← reg.idCounter.modifyGet fun n => (n, n + 1)
  let name := s!"{namePrefix}-{id}"
  if isInput then
    reg.inputNames.modify (·.push name)
  if isInteractive then
    reg.interactiveNames.modify (·.push name)
  pure name

/-! ## Reactive Events Infrastructure

Event streams and triggers for connecting terminal input to the reactive network.
-/

/-- Global reactive event streams that widgets subscribe to. -/
structure TerminusEvents where
  /-- Keyboard events. -/
  keyEvent : Reactive.Event Spider KeyData
  /-- Mouse events. -/
  mouseEvent : Reactive.Event Spider MouseData
  /-- Terminal resize events. -/
  resizeEvent : Reactive.Event Spider ResizeData
  /-- Tick events (fired each frame for animations). -/
  tickEvent : Reactive.Event Spider TickData
  /-- Component registry for focus management. -/
  registry : ComponentRegistry
  /-- Debug log function (writes to log file if debug enabled). -/
  debugLog : String → IO Unit := fun _ => pure ()

/-- Trigger functions to fire from the application loop when terminal events occur. -/
structure TerminusInputs where
  /-- Fire when a key is pressed. -/
  fireKey : KeyData → IO Unit
  /-- Fire when a mouse event occurs. -/
  fireMouse : MouseData → IO Unit
  /-- Fire when terminal is resized. -/
  fireResize : ResizeData → IO Unit
  /-- Fire on each frame (for animations). -/
  fireTick : TickData → IO Unit

/-- Create the reactive input infrastructure.
    Returns both the event streams (for subscriptions) and triggers (for firing).
    @param debugLog Optional debug logging function -/
def createInputs (debugLog : String → IO Unit := fun _ => pure ()) : SpiderM (TerminusEvents × TerminusInputs) := do
  let (keyEvent, fireKey) ← Reactive.newTriggerEvent (t := Spider) (a := KeyData)
  let (mouseEvent, fireMouse) ← Reactive.newTriggerEvent (t := Spider) (a := MouseData)
  let (resizeEvent, fireResize) ← Reactive.newTriggerEvent (t := Spider) (a := ResizeData)
  let (tickEvent, fireTick) ← Reactive.newTriggerEvent (t := Spider) (a := TickData)
  let registry ← ComponentRegistry.create

  let events : TerminusEvents := {
    keyEvent := keyEvent
    mouseEvent := mouseEvent
    resizeEvent := resizeEvent
    tickEvent := tickEvent
    registry := registry
    debugLog := debugLog
  }
  let inputs : TerminusInputs := {
    fireKey := fireKey
    fireMouse := fireMouse
    fireResize := fireResize
    fireTick := fireTick
  }
  pure (events, inputs)

end Terminus.Reactive
