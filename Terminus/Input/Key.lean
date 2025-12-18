-- Terminus.Input.Key: Keyboard event types

namespace Terminus

/-- Key modifiers (Ctrl, Alt, Shift) -/
structure KeyModifiers where
  ctrl : Bool := false
  alt : Bool := false
  shift : Bool := false
  deriving Repr, BEq, Inhabited

namespace KeyModifiers

def empty : KeyModifiers := {}
def mkCtrl : KeyModifiers := { ctrl := true }
def mkAlt : KeyModifiers := { alt := true }
def mkShift : KeyModifiers := { shift := true }

def hasAny (m : KeyModifiers) : Bool := m.ctrl || m.alt || m.shift

end KeyModifiers

/-- Keyboard key codes -/
inductive KeyCode where
  | char (c : Char)
  | enter
  | escape
  | backspace
  | tab
  | space
  | up
  | down
  | left
  | right
  | home
  | «end»
  | pageUp
  | pageDown
  | insert
  | delete
  | f (n : Nat) -- F1-F12
  | null
  deriving Repr, BEq, Inhabited

namespace KeyCode

def isChar : KeyCode → Bool
  | char _ => true
  | _ => false

def toChar : KeyCode → Option Char
  | char c => some c
  | _ => none

end KeyCode

/-- A keyboard event with key code and modifiers -/
structure KeyEvent where
  code : KeyCode
  modifiers : KeyModifiers := {}
  deriving Repr, BEq, Inhabited

namespace KeyEvent

def new (code : KeyCode) : KeyEvent := { code }

def withCtrl (e : KeyEvent) : KeyEvent := { e with modifiers.ctrl := true }
def withAlt (e : KeyEvent) : KeyEvent := { e with modifiers.alt := true }
def withShift (e : KeyEvent) : KeyEvent := { e with modifiers.shift := true }

def char (c : Char) : KeyEvent := { code := .char c }
def enter : KeyEvent := { code := .enter }
def escape : KeyEvent := { code := .escape }
def backspace : KeyEvent := { code := .backspace }
def tab : KeyEvent := { code := .tab }
def space : KeyEvent := { code := .space }
def up : KeyEvent := { code := .up }
def down : KeyEvent := { code := .down }
def left : KeyEvent := { code := .left }
def right : KeyEvent := { code := .right }

/-- Check if this is a specific character -/
def isChar (e : KeyEvent) (c : Char) : Bool :=
  match e.code with
  | .char ch => ch == c
  | _ => false

/-- Check if Ctrl+C -/
def isCtrlC (e : KeyEvent) : Bool :=
  e.modifiers.ctrl && e.isChar 'c'

/-- Check if Ctrl+Q (common quit key) -/
def isCtrlQ (e : KeyEvent) : Bool :=
  e.modifiers.ctrl && e.isChar 'q'

end KeyEvent

/-- Input event types -/
inductive Event where
  | key (event : KeyEvent)
  | resize (width height : Nat)
  | none
  deriving Repr, BEq, Inhabited

end Terminus
