-- MouseDemo: Interactive mouse event demonstration
-- Click, scroll, and drag to see events. Press 'q' to quit.

import Terminus

open Terminus

structure State where
  lastEvent : Option MouseEvent := none
  clickCount : Nat := 0
  lastClickPos : Option (Nat × Nat) := none
  history : List String := []
  deriving Inhabited

def maxHistory : Nat := 15

def formatModifiers (mods : KeyModifiers) : String :=
  let parts := []
  let parts := if mods.shift then parts ++ ["Shift"] else parts
  let parts := if mods.ctrl then parts ++ ["Ctrl"] else parts
  let parts := if mods.alt then parts ++ ["Alt"] else parts
  if parts.isEmpty then "" else " [" ++ String.intercalate "+" parts ++ "]"

def formatEvent (me : MouseEvent) : String :=
  let button := match me.button with
    | .left => "Left"
    | .middle => "Middle"
    | .right => "Right"
    | .scrollUp => "ScrollUp"
    | .scrollDown => "ScrollDown"
    | .none => "None"
  let action := match me.action with
    | .press => "Press"
    | .release => "Release"
    | .motion => "Motion"
  let mods := formatModifiers me.modifiers
  s!"{button} {action} at ({me.x}, {me.y}){mods}"

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  -- Main border
  let mainBlock := Block.double
    |>.withTitle "Mouse Demo"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  -- Split into info panel and event history
  let sections := hsplit inner [.fixed 35, .fill]

  -- Left panel: Current state
  if h : 0 < sections.length then
    let infoArea := sections[0]
    let infoBlock := Block.rounded
      |>.withTitle "Current State"
      |>.withBorderStyle (Style.fgColor Color.green)
    f := f.render infoBlock infoArea
    let infoInner := infoBlock.innerArea infoArea

    if !infoInner.isEmpty then
      let mut y := infoInner.y

      -- Click count
      f := f.writeString infoInner.x y s!"Click count: {state.clickCount}" (Style.bold.withFg Color.yellow)
      y := y + 1

      -- Last click position
      match state.lastClickPos with
      | some (x, py) =>
        f := f.writeString infoInner.x y s!"Last click: ({x}, {py})" Style.dim
      | none =>
        f := f.writeString infoInner.x y "Last click: (none)" Style.dim
      y := y + 2

      -- Current event details
      f := f.writeString infoInner.x y "Last Event:" (Style.bold.withFg Color.cyan)
      y := y + 1

      match state.lastEvent with
      | some me =>
        f := f.writeString infoInner.x y s!"  Button: {match me.button with
          | .left => "Left"
          | .middle => "Middle"
          | .right => "Right"
          | .scrollUp => "Scroll Up"
          | .scrollDown => "Scroll Down"
          | .none => "None"}" Style.default
        y := y + 1

        f := f.writeString infoInner.x y s!"  Action: {match me.action with
          | .press => "Press"
          | .release => "Release"
          | .motion => "Motion"}" Style.default
        y := y + 1

        f := f.writeString infoInner.x y s!"  Position: ({me.x}, {me.y})" Style.default
        y := y + 1

        let mods := formatModifiers me.modifiers
        if !mods.isEmpty then
          f := f.writeString infoInner.x y s!"  Modifiers:{mods}" Style.default
          y := y + 1

      | none =>
        f := f.writeString infoInner.x y "  (no events yet)" Style.dim

  -- Right panel: Event history
  if h : 1 < sections.length then
    let historyArea := sections[1]
    let historyBlock := Block.rounded
      |>.withTitle "Event History"
      |>.withBorderStyle (Style.fgColor Color.magenta)
    f := f.render historyBlock historyArea
    let histInner := historyBlock.innerArea historyArea

    if !histInner.isEmpty then
      let visibleCount := min state.history.length histInner.height
      let visibleHistory := state.history.take visibleCount
      let mut idx : Nat := 0
      for entry in visibleHistory do
        if idx < histInner.height then
          let style := if idx == 0 then Style.bold else Style.dim
          f := f.writeString histInner.x (histInner.y + idx) entry style
        idx := idx + 1

  -- Status bar at bottom
  let statusY := area.y + area.height - 1
  let statusText := "Click, scroll, or drag anywhere | Press 'q' to quit"
  f := f.writeString (area.x + 2) statusY statusText Style.dim

  f

def update (state : State) (event : Option Event) : State × Bool :=
  match event with
  | none => (state, false)
  | some (.key k) =>
    match k.code with
    | .char 'q' => (state, true)
    | .escape => (state, true)
    | _ =>
      if k.isCtrlC || k.isCtrlQ then (state, true)
      else (state, false)
  | some (.mouse me) =>
    let isClick := me.action == .press && me.button != .none &&
                   me.button != .scrollUp && me.button != .scrollDown
    let newClickCount := if isClick then state.clickCount + 1 else state.clickCount
    let newClickPos := if isClick then some (me.x, me.y) else state.lastClickPos
    let eventStr := formatEvent me
    let newHistory := (eventStr :: state.history).take maxHistory
    ({ state with
       lastEvent := some me,
       clickCount := newClickCount,
       lastClickPos := newClickPos,
       history := newHistory }, false)
  | _ => (state, false)

def main : IO Unit := do
  let initialState : State := {}
  App.runApp initialState draw update
