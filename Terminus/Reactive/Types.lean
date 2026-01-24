/-
  Terminus Reactive - Core Types
  Types for FRP-based terminal widgets.
-/
import Terminus
import Reactive
import Std.Data.HashMap

-- Note: We open Reactive for access to exported functions (Reactive.holdDyn, Reactive.newTriggerEvent, etc.)
-- but still use fully qualified Reactive.Event, Reactive.Dynamic for clarity vs Terminus.Event.

namespace Terminus.Reactive

open Reactive Reactive.Host

/-! ## Text Layout Types -/

/-- Text alignment options -/
inductive Alignment where
  | left
  | center
  | right
  deriving Repr, BEq, Inhabited

/-- Wrap mode for long lines -/
inductive WrapMode where
  | noWrap    -- Truncate at boundary
  | wrap      -- Wrap at word boundaries
  | charWrap  -- Wrap at character boundaries
  deriving Repr, BEq, Inhabited

/-! ## Border Types -/

/-- Border style variants -/
inductive BorderType where
  | none
  | single
  | double
  | rounded
  | thick
  deriving Repr, BEq, Inhabited

/-- Border character set -/
structure BorderChars where
  topLeft : Char
  topRight : Char
  bottomLeft : Char
  bottomRight : Char
  horizontal : Char
  vertical : Char
  deriving Repr, Inhabited

namespace BorderChars

def single : BorderChars := {
  topLeft := '┌'
  topRight := '┐'
  bottomLeft := '└'
  bottomRight := '┘'
  horizontal := '─'
  vertical := '│'
}

def double : BorderChars := {
  topLeft := '╔'
  topRight := '╗'
  bottomLeft := '╚'
  bottomRight := '╝'
  horizontal := '═'
  vertical := '║'
}

def rounded : BorderChars := {
  topLeft := '╭'
  topRight := '╮'
  bottomLeft := '╰'
  bottomRight := '╯'
  horizontal := '─'
  vertical := '│'
}

def thick : BorderChars := {
  topLeft := '┏'
  topRight := '┓'
  bottomLeft := '┗'
  bottomRight := '┛'
  horizontal := '━'
  vertical := '┃'
}

def fromType : BorderType → BorderChars
  | .none => single -- Will not be drawn anyway
  | .single => single
  | .double => double
  | .rounded => rounded
  | .thick => thick

end BorderChars

/-! ## Layout Style

Simple constraints for reactive terminal layout.
-/

/-- Style constraints for reactive layout containers. -/
structure RStyle where
  /-- Minimum width in cells. -/
  minWidth : Option Nat := none
  /-- Minimum height in cells. -/
  minHeight : Option Nat := none
  /-- Padding on all sides in cells. -/
  padding : Nat := 0
  deriving Repr, Inhabited, BEq

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
inductive RNode : Type where
  /-- Text content with styling. -/
  | text (content : String) (style : Style)
  /-- Block container with optional title, border, and background fill. -/
  | block (title : Option String) (borderType : BorderType) (borderStyle : Style)
      (fillStyle : Option Style) (child : RNode)
  /-- Row container (horizontal flex layout). -/
  | row (gap : Nat) (style : RStyle) (children : Array RNode)
  /-- Column container (vertical flex layout). -/
  | column (gap : Nat) (style : RStyle) (children : Array RNode)
  /-- Fixed-size spacer. -/
  | spacer (width : Nat) (height : Nat)
  /-- Empty placeholder (zero size). -/
  | empty
  /-- Clips child content to this node's computed bounds. -/
  | clipped (child : RNode)
  /-- Shifts child content by offset (for scrolling). -/
  | scrolled (offsetX offsetY : Nat) (child : RNode)
  /-- Dock a footer at the bottom with fixed height. -/
  | dockBottom (footerHeight : Nat) (content : RNode) (footer : RNode)
  /-- Overlay content centered on top of base content.
      - base: underlying content that renders first
      - content: overlay content centered on screen
      - backdropStyle: optional style for backdrop (fills screen behind overlay) -/
  | overlay (base : RNode) (content : RNode) (backdropStyle : Option Style)
  /-- Image node using terminal image protocol. -/
  | image (source : ImageSource) (protocol : ImageProtocol) (width : Nat) (height : Nat)
      (preserveAspect : Bool) (altText : String)
  deriving Repr, Inhabited, BEq

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
  /-- Counter for generating unique group IDs (e.g., panels). -/
  groupCounter : IO.Ref Nat
  /-- Names of focusable input widgets. -/
  inputNames : IO.Ref (Array String)
  /-- Names of all interactive widgets. -/
  interactiveNames : IO.Ref (Array String)
  /-- Mapping from widget name to focus group ancestry (nearest first). -/
  groupMembers : IO.Ref (Std.HashMap String (List String))
  /-- Currently focused input (by auto-generated name). -/
  focusedInput : Reactive.Dynamic Spider (Option String)
  /-- Focus group ancestry for the currently focused input. -/
  focusedGroups : Reactive.Dynamic Spider (List String)
  /-- Trigger to change focus. -/
  fireFocus : Option String → IO Unit

/-- Create a new component registry. -/
def ComponentRegistry.create : SpiderM ComponentRegistry := do
  let idCounter ← SpiderM.liftIO <| IO.mkRef 0
  let groupCounter ← SpiderM.liftIO <| IO.mkRef 0
  let inputNames ← SpiderM.liftIO <| IO.mkRef #[]
  let interactiveNames ← SpiderM.liftIO <| IO.mkRef #[]
  let groupMembers ← SpiderM.liftIO <| IO.mkRef ({} : Std.HashMap String (List String))
  let (focusEvent, fireFocusName) ← Reactive.newTriggerEvent (t := Spider) (a := Option String)
  let (focusGroupsEvent, fireFocusGroups) ← Reactive.newTriggerEvent (t := Spider) (a := List String)
  let focusedInput ← Reactive.holdDyn none focusEvent
  let focusedGroups ← Reactive.holdDyn [] focusGroupsEvent
  let fireFocus := fun name => do
    fireFocusName name
    let groups ← match name with
      | some n => do
          let members ← groupMembers.get
          pure (members.getD n [])
      | none => pure []
    fireFocusGroups groups
  pure {
    idCounter,
    groupCounter,
    inputNames,
    interactiveNames,
    groupMembers,
    focusedInput,
    focusedGroups,
    fireFocus
  }

/-- Register a component and get a name (auto-generated unless `nameOverride` is provided).
    - `namePrefix`: Component type prefix (e.g., "button", "text-input")
    - `isInput`: Whether this is a focusable input widget
    - `isInteractive`: Whether this widget responds to clicks -/
def ComponentRegistry.register (reg : ComponentRegistry) (namePrefix : String)
    (isInput : Bool := false) (isInteractive : Bool := true)
    (nameOverride : String := "") : IO String := do
  let id ← reg.idCounter.modifyGet fun n => (n, n + 1)
  let name :=
    if nameOverride.isEmpty then
      s!"{namePrefix}-{id}"
    else
      nameOverride
  if isInput then
    reg.inputNames.modify (·.push name)
  if isInteractive then
    reg.interactiveNames.modify (·.push name)
  pure name

/-- Associate a widget name with its focus group ancestry. -/
def ComponentRegistry.registerGroups (reg : ComponentRegistry) (name : String)
    (groups : List String) : IO Unit := do
  if !groups.isEmpty then
    reg.groupMembers.modify (fun members => members.insert name groups)

/-- Allocate a new focus group ID. -/
def ComponentRegistry.newGroup (reg : ComponentRegistry) (groupPrefix : String := "panel")
    : IO String := do
  let id ← reg.groupCounter.modifyGet fun n => (n, n + 1)
  pure s!"{groupPrefix}-{id}"

private def removeFirst (names : Array String) (name : String) : Array String := Id.run do
  let mut out : Array String := #[]
  let mut removed := false
  for entry in names do
    if !removed && entry == name then
      removed := true
    else
      out := out.push entry
  out

/-- Unregister a component name from the registry. -/
def ComponentRegistry.unregister (reg : ComponentRegistry) (name : String)
    (isInput : Bool := false) (isInteractive : Bool := true) : IO Unit := do
  if isInput then
    reg.inputNames.modify (fun names => removeFirst names name)
  if isInteractive then
    reg.interactiveNames.modify (fun names => removeFirst names name)
  reg.groupMembers.modify (fun members => members.erase name)
  let current ← reg.focusedInput.sample
  if current == some name then
    reg.fireFocus none

/-- Check if a widget name belongs to a focus group. -/
def ComponentRegistry.isInGroup (reg : ComponentRegistry) (name group : String) : IO Bool := do
  let members ← reg.groupMembers.get
  match members.get? name with
  | some groups => pure (groups.contains group)
  | none => pure false

/-- Cycle focus to the next registered input widget.
    If no widget is focused, focuses the first one.
    If the last widget is focused, wraps to the first. -/
def ComponentRegistry.focusNext (reg : ComponentRegistry) : IO Unit := do
  let names ← reg.inputNames.get
  if names.isEmpty then return
  let current ← reg.focusedInput.sample
  let nextName := match current with
    | none => names[0]!
    | some name =>
      match names.findIdx? (· == name) with
      | some idx => names[(idx + 1) % names.size]!
      | none => names[0]!
  reg.fireFocus (some nextName)

/-- Cycle focus to the previous registered input widget. -/
def ComponentRegistry.focusPrev (reg : ComponentRegistry) : IO Unit := do
  let names ← reg.inputNames.get
  if names.isEmpty then return
  let current ← reg.focusedInput.sample
  let prevName := match current with
    | none => names[names.size - 1]!
    | some name =>
      match names.findIdx? (· == name) with
      | some idx =>
        if idx == 0 then names[names.size - 1]!
        else names[idx - 1]!
      | none => names[names.size - 1]!
  reg.fireFocus (some prevName)

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
  /-- Focus group ancestry for child widgets (used for panel focus styling). -/
  focusGroups : List String := []
  /-- Whether a tick stream is needed (set by useTick/useFrame hooks). -/
  tickRequested : IO.Ref Bool
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
  let tickRequested ← SpiderM.liftIO (IO.mkRef false)
  let registry ← ComponentRegistry.create

  let events : TerminusEvents := {
    keyEvent := keyEvent
    mouseEvent := mouseEvent
    resizeEvent := resizeEvent
    tickEvent := tickEvent
    focusGroups := []
    tickRequested := tickRequested
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
