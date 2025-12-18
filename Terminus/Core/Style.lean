-- Terminus.Core.Style: Colors and text styling

namespace Terminus

/-- Standard 16 ANSI colors -/
inductive Color16 where
  | black
  | red
  | green
  | yellow
  | blue
  | magenta
  | cyan
  | white
  | brightBlack
  | brightRed
  | brightGreen
  | brightYellow
  | brightBlue
  | brightMagenta
  | brightCyan
  | brightWhite
  deriving Repr, BEq, Inhabited

/-- Color specification supporting multiple color modes -/
inductive Color where
  | default
  | ansi (c : Color16)
  | indexed (n : UInt8)
  | rgb (r g b : UInt8)
  deriving Repr, BEq, Inhabited

namespace Color

def black : Color := .ansi .black
def red : Color := .ansi .red
def green : Color := .ansi .green
def yellow : Color := .ansi .yellow
def blue : Color := .ansi .blue
def magenta : Color := .ansi .magenta
def cyan : Color := .ansi .cyan
def white : Color := .ansi .white

def brightBlack : Color := .ansi .brightBlack
def brightRed : Color := .ansi .brightRed
def brightGreen : Color := .ansi .brightGreen
def brightYellow : Color := .ansi .brightYellow
def brightBlue : Color := .ansi .brightBlue
def brightMagenta : Color := .ansi .brightMagenta
def brightCyan : Color := .ansi .brightCyan
def brightWhite : Color := .ansi .brightWhite

def gray : Color := .ansi .brightBlack

end Color

/-- Text modifiers (attributes) -/
structure Modifier where
  bold : Bool := false
  dim : Bool := false
  italic : Bool := false
  underline : Bool := false
  blink : Bool := false
  reverse : Bool := false
  hidden : Bool := false
  crossedOut : Bool := false
  deriving Repr, BEq, Inhabited

namespace Modifier

def empty : Modifier := {}
def mkBold : Modifier := { bold := true }
def mkDim : Modifier := { dim := true }
def mkItalic : Modifier := { italic := true }
def mkUnderline : Modifier := { underline := true }
def mkBlink : Modifier := { blink := true }
def mkReverse : Modifier := { reverse := true }
def mkHidden : Modifier := { hidden := true }
def mkCrossedOut : Modifier := { crossedOut := true }

def merge (m1 m2 : Modifier) : Modifier := {
  bold := m1.bold || m2.bold
  dim := m1.dim || m2.dim
  italic := m1.italic || m2.italic
  underline := m1.underline || m2.underline
  blink := m1.blink || m2.blink
  reverse := m1.reverse || m2.reverse
  hidden := m1.hidden || m2.hidden
  crossedOut := m1.crossedOut || m2.crossedOut
}

end Modifier

/-- Complete style specification -/
structure Style where
  fg : Color := .default
  bg : Color := .default
  modifier : Modifier := {}
  deriving Repr, BEq, Inhabited

namespace Style

def default : Style := {}

def fgColor (c : Color) : Style := { fg := c }
def bgColor (c : Color) : Style := { bg := c }

def bold : Style := { modifier := Modifier.mkBold }
def dim : Style := { modifier := Modifier.mkDim }
def italic : Style := { modifier := Modifier.mkItalic }
def underline : Style := { modifier := Modifier.mkUnderline }
def blink : Style := { modifier := Modifier.mkBlink }
def reversed : Style := { modifier := Modifier.mkReverse }
def hidden : Style := { modifier := Modifier.mkHidden }
def crossedOut : Style := { modifier := Modifier.mkCrossedOut }

def withFg (s : Style) (c : Color) : Style := { s with fg := c }
def withBg (s : Style) (c : Color) : Style := { s with bg := c }
def withModifier (s : Style) (m : Modifier) : Style := { s with modifier := Modifier.merge s.modifier m }

def merge (s1 s2 : Style) : Style := {
  fg := if s2.fg == .default then s1.fg else s2.fg
  bg := if s2.bg == .default then s1.bg else s2.bg
  modifier := Modifier.merge s1.modifier s2.modifier
}

instance : Append Style where
  append := merge

end Style

/-- Orientation for directional widgets -/
inductive Orientation where
  | horizontal
  | vertical
  deriving Repr, BEq, Inhabited

end Terminus
