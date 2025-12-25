-- Form: Grouped input widgets laid out in a form container
-- Tab/Down: next field, Up: previous, Space: toggle checkbox, q/Esc: quit

import Terminus

open Terminus

namespace FormDemo

inductive Focus where
  | name
  | email
  | age
  | subscribe
  deriving Repr, BEq, Inhabited

structure State where
  focus : Focus := .name
  name : TextInput := (TextInput.new.withPlaceholder "________________")
  email : TextInput := (TextInput.new.withPlaceholder "________________")
  age : TextInput := (TextInput.new.withPlaceholder "__" |>.withMaxLength 3)
  subscribe : Bool := true
  lastAction : String := "Tab/Down: next | Up: prev | Space: toggle | q: quit"
  deriving Inhabited

private def focusNext : Focus → Focus
  | .name => .email
  | .email => .age
  | .age => .subscribe
  | .subscribe => .name

private def focusPrev : Focus → Focus
  | .name => .subscribe
  | .email => .name
  | .age => .email
  | .subscribe => .age

private def applyFocus (s : State) : State :=
  { s with
    name := if s.focus == .name then s.name.focus else s.name.blur,
    email := if s.focus == .email then s.email.focus else s.email.blur,
    age := if s.focus == .age then s.age.focus else s.age.blur
  }

private def inputBlock (focused : Bool) : Block :=
  let border := if focused then Style.fgColor Color.yellow else Style.dim
  Block.single.withBorderStyle border

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame
  if area.isEmpty then return f

  let state := applyFocus state

  let w := min 60 area.width
  let h := min 14 area.height
  if w < 10 || h < 8 then return f
  let x := (area.width - w) / 2
  let y := (area.height - h) / 2
  let formArea : Rect := { x := x, y := y, width := w, height := h }

  let nameInput := state.name.withBlock (inputBlock (state.focus == .name))
  let emailInput := state.email.withBlock (inputBlock (state.focus == .email))
  let ageInput := state.age.withBlock (inputBlock (state.focus == .age))

  let subscribeStyle := if state.focus == .subscribe then Style.reversed else {}
  let subscribe :=
    (Checkbox.new "Subscribe to newsletter")
      |>.withChecked state.subscribe
      |>.withSymbols "[x]" "[ ]"
      |>.withLabelStyle subscribeStyle
      |>.withSymbolStyle subscribeStyle

  let buttons :=
    Paragraph.fromString "   [ Cancel ]  [ Submit ]"
      |>.withStyle Style.dim

  let ageSmall : AnyWidget := {
    renderFn := fun a buf => Id.run do
      let parts := hsplit a [.fixed 10, .fill]
      if h : 0 < parts.length then
        Widget.render ageInput parts[0] buf
      else
        buf
  }

  let form :=
    (Form.new)
      |>.withBlock (Block.double.withTitle "User Details" |>.withBorderStyle (Style.fgColor Color.blue))
      |>.withRowSpacing 1
      |>.withRows [
        FormRow.fieldOf "Name" nameInput 3,
        FormRow.fieldOf "Email" emailInput 3,
        FormRow.fieldWidget "Age" ageSmall 3,
        FormRow.fullOf subscribe,
        FormRow.gap 1,
        FormRow.fullOf buttons
      ]

  f := f.render form formArea

  if area.height > formArea.y + formArea.height + 1 then
    let statusY := formArea.y + formArea.height + 1
    f := f.writeString 1 statusY state.lastAction Style.dim

  f

def update (state : State) (event : Option Event) : State × Bool :=
  match event with
  | none => (state, false)
  | some (.key k) =>
    match k.code with
    | .char 'q' | .escape => (state, true)
    | .tab | .down =>
      ({ state with focus := focusNext state.focus }, false)
    | .up =>
      ({ state with focus := focusPrev state.focus }, false)
    | .space =>
      match state.focus with
      | .subscribe =>
        let newVal := !state.subscribe
        ({ state with subscribe := newVal, lastAction := s!"subscribe := {newVal}" }, false)
      | _ =>
        (state, false)
    | _ =>
      match state.focus with
      | .name => ({ state with name := state.name.handleKey k }, false)
      | .email => ({ state with email := state.email.handleKey k }, false)
      | .age => ({ state with age := state.age.handleKey k }, false)
      | .subscribe => (state, false)
  | _ => (state, false)

end FormDemo

def main : IO Unit := do
  App.runApp ({} : FormDemo.State) FormDemo.draw FormDemo.update
