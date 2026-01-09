-- Terminus.Core.Focus: Focus management utilities

import Terminus.Input.Key
import Terminus.Widgets.Widget

namespace Terminus

/-- Focus state for a collection of widgets. -/
structure FocusState where
  focused : Option Nat := none
  wrap : Bool := true
  deriving Repr, Inhabited

namespace FocusState

def withWrap (s : FocusState) (wrap : Bool) : FocusState :=
  { s with wrap }

private def isFocusableAt [Widget α] (items : Array α) (idx : Nat) : Bool :=
  match items[idx]? with
  | some item => Widget.focusable item
  | none => false

/-- First focusable index, if any. -/
def firstFocusable [Widget α] (items : Array α) : Option Nat := Id.run do
  for i in [:items.size] do
    match items[i]? with
    | some item =>
      if Widget.focusable item then
        return some i
    | none => pure ()
  return none

/-- Last focusable index, if any. -/
def lastFocusable [Widget α] (items : Array α) : Option Nat := Id.run do
  let size := items.size
  if size == 0 then return none
  let mut offset := 0
  while offset < size do
    let idx := size - 1 - offset
    match items[idx]? with
    | some item =>
      if Widget.focusable item then
        return some idx
    | none => pure ()
    offset := offset + 1
  return none

private def nextIndex [Widget α] (items : Array α) (start : Nat) (wrap : Bool) : Option Nat := Id.run do
  let size := items.size
  if size == 0 then return none
  let start := if start >= size then size - 1 else start
  let mut i := start + 1
  while i < size do
    match items[i]? with
    | some item =>
      if Widget.focusable item then
        return some i
    | none => pure ()
    i := i + 1
  if wrap then
    let mut j := 0
    while j < start do
      match items[j]? with
      | some item =>
        if Widget.focusable item then
          return some j
      | none => pure ()
      j := j + 1
  return none

private def prevIndex [Widget α] (items : Array α) (start : Nat) (wrap : Bool) : Option Nat := Id.run do
  let size := items.size
  if size == 0 then return none
  let start := if start >= size then size - 1 else start
  let mut i := start
  while i > 0 do
    i := i - 1
    match items[i]? with
    | some item =>
      if Widget.focusable item then
        return some i
    | none => pure ()
  if wrap then
    let mut j := size
    while j > start + 1 do
      j := j - 1
      match items[j]? with
      | some item =>
        if Widget.focusable item then
          return some j
      | none => pure ()
  return none

/-- Ensure focus points at a valid focusable widget (or none). -/
def normalize [Widget α] (s : FocusState) (items : Array α) : FocusState :=
  match s.focused with
  | some idx =>
    if idx < items.size && isFocusableAt items idx then s
    else { s with focused := firstFocusable items }
  | none =>
    { s with focused := firstFocusable items }

/-- Focus the first focusable widget. -/
def focusFirst [Widget α] (s : FocusState) (items : Array α) : FocusState :=
  { s with focused := firstFocusable items }

/-- Focus the last focusable widget. -/
def focusLast [Widget α] (s : FocusState) (items : Array α) : FocusState :=
  { s with focused := lastFocusable items }

/-- Move focus to the next focusable widget. -/
def focusNext [Widget α] (s : FocusState) (items : Array α) : FocusState :=
  let s := s.normalize items
  match s.focused with
  | none => { s with focused := firstFocusable items }
  | some idx =>
    match nextIndex items idx s.wrap with
    | some next => { s with focused := some next }
    | none => s

/-- Move focus to the previous focusable widget. -/
def focusPrev [Widget α] (s : FocusState) (items : Array α) : FocusState :=
  let s := s.normalize items
  match s.focused with
  | none => { s with focused := lastFocusable items }
  | some idx =>
    match prevIndex items idx s.wrap with
    | some prev => { s with focused := some prev }
    | none => s

/-- Whether this key is a focus navigation key. -/
def isFocusKey (k : KeyEvent) : Bool :=
  match k.code with
  | .tab | .up | .down | .left | .right | .home | .«end» => true
  | _ => false

/-- Update focus state from a key event. -/
def handleKey [Widget α] (s : FocusState) (items : Array α) (k : KeyEvent) : FocusState :=
  match k.code with
  | .tab =>
    if k.modifiers.shift then focusPrev s items else focusNext s items
  | .down | .right => focusNext s items
  | .up | .left => focusPrev s items
  | .home => focusFirst s items
  | .«end» => focusLast s items
  | _ => s

/-- Update focus state from an input event. -/
def handleEvent [Widget α] (s : FocusState) (items : Array α) (event : Event) : FocusState :=
  match event with
  | .key k => handleKey s items k
  | _ => s

/-- Apply focus state to a widget collection. -/
def apply [Widget α] (s : FocusState) (items : Array α) : Array α := Id.run do
  let mut out := items
  for i in [:items.size] do
    out := out.modify i (fun w => Widget.setFocused w (s.focused == some i))
  out

/-- Handle an event on the currently focused widget. -/
def handleEventFocused [Widget α] (s : FocusState) (items : Array α) (event : Event) : Array α :=
  match s.focused with
  | some idx =>
    if idx < items.size then
      items.modify idx (fun w => Widget.handleEvent w event)
    else
      items
  | none => items

/-- Normalize and apply focus to a widget collection. -/
def sync [Widget α] (s : FocusState) (items : Array α) : FocusState × Array α :=
  let s := normalize s items
  (s, apply s items)

/-- Update focus state and route events to widgets. -/
def update [Widget α] (s : FocusState) (items : Array α) (event : Event) : FocusState × Array α :=
  let (s, items) := sync s items
  match event with
  | .key k =>
    if isFocusKey k then
      let s := handleKey s items k
      (s, apply s items)
    else
      (s, handleEventFocused s items event)
  | _ =>
    (s, handleEventFocused s items event)

end FocusState

end Terminus
