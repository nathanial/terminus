-- Terminus.Backend.Ansi: ANSI escape code generation

import Terminus.Core.Style
import Terminus.Core.Base64

namespace Terminus.Ansi

/-- The escape character -/
def esc : String := "\x1b"

/-- Control Sequence Introducer -/
def csi : String := esc ++ "["

-- Cursor movement

def cursorUp (n : Nat := 1) : String := s!"{csi}{n}A"
def cursorDown (n : Nat := 1) : String := s!"{csi}{n}B"
def cursorForward (n : Nat := 1) : String := s!"{csi}{n}C"
def cursorBack (n : Nat := 1) : String := s!"{csi}{n}D"

/-- Move cursor to position (1-indexed) -/
def cursorTo (row col : Nat) : String := s!"{csi}{row};{col}H"

/-- Move cursor to position (0-indexed, converted to 1-indexed) -/
def cursorToZero (x y : Nat) : String := cursorTo (y + 1) (x + 1)

def cursorHome : String := s!"{csi}H"

def cursorHide : String := s!"{csi}?25l"
def cursorShow : String := s!"{csi}?25h"

def saveCursor : String := s!"{csi}s"
def restoreCursor : String := s!"{csi}u"

-- Screen clearing

/-- Clear from cursor to end of screen -/
def clearToEnd : String := s!"{csi}0J"

/-- Clear from cursor to beginning of screen -/
def clearToBegin : String := s!"{csi}1J"

/-- Clear entire screen -/
def clearScreen : String := s!"{csi}2J"

/-- Clear entire screen and scrollback buffer -/
def clearAll : String := s!"{csi}3J"

/-- Clear from cursor to end of line -/
def clearLineToEnd : String := s!"{csi}0K"

/-- Clear from cursor to beginning of line -/
def clearLineToBegin : String := s!"{csi}1K"

/-- Clear entire line -/
def clearLine : String := s!"{csi}2K"

-- Scrolling

def scrollUp (n : Nat := 1) : String := s!"{csi}{n}S"
def scrollDown (n : Nat := 1) : String := s!"{csi}{n}T"

-- Alternative screen buffer

def enterAltScreen : String := s!"{csi}?1049h"
def leaveAltScreen : String := s!"{csi}?1049l"

-- Text attributes

def resetAll : String := s!"{csi}0m"

def bold : String := s!"{csi}1m"
def dim : String := s!"{csi}2m"
def italic : String := s!"{csi}3m"
def underline : String := s!"{csi}4m"
def blink : String := s!"{csi}5m"
def reverse : String := s!"{csi}7m"
def hidden : String := s!"{csi}8m"
def crossedOut : String := s!"{csi}9m"

def noBold : String := s!"{csi}22m"
def noItalic : String := s!"{csi}23m"
def noUnderline : String := s!"{csi}24m"
def noBlink : String := s!"{csi}25m"
def noReverse : String := s!"{csi}27m"
def noHidden : String := s!"{csi}28m"
def noCrossedOut : String := s!"{csi}29m"

-- 16-color foreground (30-37, 90-97)

def color16ToFgCode : Color16 → Nat
  | .black => 30
  | .red => 31
  | .green => 32
  | .yellow => 33
  | .blue => 34
  | .magenta => 35
  | .cyan => 36
  | .white => 37
  | .brightBlack => 90
  | .brightRed => 91
  | .brightGreen => 92
  | .brightYellow => 93
  | .brightBlue => 94
  | .brightMagenta => 95
  | .brightCyan => 96
  | .brightWhite => 97

def color16ToBgCode : Color16 → Nat
  | .black => 40
  | .red => 41
  | .green => 42
  | .yellow => 43
  | .blue => 44
  | .magenta => 45
  | .cyan => 46
  | .white => 47
  | .brightBlack => 100
  | .brightRed => 101
  | .brightGreen => 102
  | .brightYellow => 103
  | .brightBlue => 104
  | .brightMagenta => 105
  | .brightCyan => 106
  | .brightWhite => 107

def fgColor16 (c : Color16) : String := s!"{csi}{color16ToFgCode c}m"
def bgColor16 (c : Color16) : String := s!"{csi}{color16ToBgCode c}m"

-- 256-color foreground/background

def fgColor256 (n : UInt8) : String := s!"{csi}38;5;{n.toNat}m"
def bgColor256 (n : UInt8) : String := s!"{csi}48;5;{n.toNat}m"

-- RGB true color foreground/background

def fgRgb (r g b : UInt8) : String := s!"{csi}38;2;{r.toNat};{g.toNat};{b.toNat}m"
def bgRgb (r g b : UInt8) : String := s!"{csi}48;2;{r.toNat};{g.toNat};{b.toNat}m"

-- Default colors

def fgDefault : String := s!"{csi}39m"
def bgDefault : String := s!"{csi}49m"

-- High-level color conversion

def fgColor : Color → String
  | .default => fgColor16 .white
  | .ansi c => fgColor16 c
  | .indexed n => fgColor256 n
  | .rgb r g b => fgRgb r g b

def bgColor : Color → String
  | .default => bgColor16 .black
  | .ansi c => bgColor16 c
  | .indexed n => bgColor256 n
  | .rgb r g b => bgRgb r g b

/-- Generate ANSI codes for a Modifier -/
def modifierCodes (m : Modifier) : String := Id.run do
  let mut s := ""
  if m.bold then s := s ++ bold
  if m.dim then s := s ++ dim
  if m.italic then s := s ++ italic
  if m.underline then s := s ++ underline
  if m.blink then s := s ++ blink
  if m.reverse then s := s ++ reverse
  if m.hidden then s := s ++ hidden
  if m.crossedOut then s := s ++ crossedOut
  s

/-- Generate ANSI codes for a complete Style -/
def styleCodes (st : Style) : String :=
  resetAll ++ fgColor st.fg ++ bgColor st.bg ++ modifierCodes st.modifier

-- Mouse tracking (SGR extended mode)
-- Mode 1003: Report all motion events (not just when button pressed)
-- Mode 1006: SGR extended format (supports coordinates > 223, uses M/m for press/release)

/-- Enable mouse tracking with all motion events in SGR extended format -/
def enableMouse : String := csi ++ "?1003h" ++ csi ++ "?1006h"

/-- Disable mouse tracking -/
def disableMouse : String := csi ++ "?1003l" ++ csi ++ "?1006l"

-- Hyperlinks (OSC 8)
-- Format: ESC ] 8 ; params ; uri ST
-- Where ST (String Terminator) is ESC \ or BEL (\x07)

/-- Operating System Command introducer -/
def osc : String := esc ++ "]"

/-- String Terminator (using BEL for broader compatibility) -/
def st : String := "\x07"

/-- Start a hyperlink. Text rendered after this will be clickable.
    Supported by iTerm2, Windows Terminal, GNOME Terminal, and others. -/
def hyperlinkStart (url : String) : String := s!"{osc}8;;{url}{st}"

/-- End the current hyperlink -/
def hyperlinkEnd : String := s!"{osc}8;;{st}"

/-- Wrap text in a hyperlink (convenience for inline use) -/
def hyperlink (url : String) (text : String) : String :=
  hyperlinkStart url ++ text ++ hyperlinkEnd

-- Clipboard (OSC 52)
-- Format: ESC ] 52 ; <selection> ; <base64-data> ST
-- Selection: c = clipboard, p = primary, s = select
-- To query clipboard: use "?" as data

/-- Write text to system clipboard using OSC 52.
    The text is base64-encoded before sending.
    Supported by iTerm2, Windows Terminal, xterm, and others.
    Note: Some terminals require explicit opt-in for clipboard access. -/
def clipboardWrite (text : String) (selection : String := "c") : String :=
  let encoded := Terminus.Base64.encodeString text
  s!"{osc}52;{selection};{encoded}{st}"

/-- Request clipboard contents using OSC 52.
    Terminal will respond with: ESC ] 52 ; <selection> ; <base64-data> ST
    The application must parse this response from the input stream. -/
def clipboardQuery (selection : String := "c") : String :=
  s!"{osc}52;{selection};?{st}"

/-- Clear the clipboard using OSC 52. -/
def clipboardClear (selection : String := "c") : String :=
  s!"{osc}52;{selection};{st}"

end Terminus.Ansi
