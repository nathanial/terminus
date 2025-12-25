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

/-- Mouse button identifiers -/
inductive MouseButton where
  | left
  | middle
  | right
  | scrollUp
  | scrollDown
  | none  -- For motion-only events
  deriving Repr, BEq, Inhabited

/-- Mouse event action type -/
inductive MouseAction where
  | press
  | release
  | motion  -- Movement while button held (drag) or no button (hover)
  deriving Repr, BEq, Inhabited

/-- A mouse event with position, button, and modifiers -/
structure MouseEvent where
  x         : Nat            -- 1-based column
  y         : Nat            -- 1-based row
  button    : MouseButton
  action    : MouseAction
  modifiers : KeyModifiers   -- Reuse keyboard modifiers (Ctrl, Alt, Shift)
  deriving Repr, BEq, Inhabited

namespace MouseEvent

/-- Create a click event -/
def click (x y : Nat) (button : MouseButton := .left) : MouseEvent :=
  { x, y, button, action := .press, modifiers := {} }

/-- Create a release event -/
def release (x y : Nat) (button : MouseButton := .left) : MouseEvent :=
  { x, y, button, action := .release, modifiers := {} }

/-- Check if this is a left click -/
def isLeftClick (e : MouseEvent) : Bool :=
  e.button == .left && e.action == .press

/-- Check if this is a right click -/
def isRightClick (e : MouseEvent) : Bool :=
  e.button == .right && e.action == .press

/-- Check if this is a scroll event -/
def isScroll (e : MouseEvent) : Bool :=
  e.button == .scrollUp || e.button == .scrollDown

end MouseEvent

/-- Input event types -/
inductive Event where
  | key (event : KeyEvent)
  | mouse (event : MouseEvent)
  | resize (width height : Nat)
  | none
  deriving Repr, BEq, Inhabited

namespace Event

/-- Extract KeyEvent if this is a keyboard event -/
def toKeyEvent? : Event → Option KeyEvent
  | .key ke => some ke
  | _ => Option.none

/-- Extract MouseEvent if this is a mouse event -/
def toMouseEvent? : Event → Option MouseEvent
  | .mouse me => some me
  | _ => Option.none

end Event

end Terminus
