-- Terminus.Backend.Commands: Non-cell terminal commands (e.g. images)

import Terminus.Core.Rect

namespace Terminus

/-- Simple FNV-1a hash for payload keys. -/
private def fnv1a64 (bytes : ByteArray) : UInt64 := Id.run do
  let mut h : UInt64 := 0xcbf29ce484222325
  for b in bytes.data.toList do
    h := h ^^^ (UInt64.ofNat b.toNat)
    h := h * 0x00000100000001B3
  h

private def fnv1a64String (s : String) : UInt64 :=
  fnv1a64 s.toUTF8

/-- Binary source for an image payload. -/
inductive ImageSource where
  | bytes (data : ByteArray)
  | path (path : System.FilePath)
  deriving Inhabited, BEq

instance : Repr ImageSource where
  reprPrec s _ := match s with
    | .bytes b => s!"ImageSource.bytes ({b.size} bytes)"
    | .path p => s!"ImageSource.path \"{p}\""

namespace ImageSource

def key : ImageSource → String
  | .path p => s!"path:{p}"
  | .bytes b => s!"bytes:{b.size}:{fnv1a64 b}"

end ImageSource

/-- Terminal image protocol. -/
inductive ImageProtocol where
  | iterm2
  | sixel
  deriving BEq, Inhabited, Repr

namespace ImageProtocol

def label : ImageProtocol → String
  | .iterm2 => "iterm2"
  | .sixel => "sixel"

end ImageProtocol

/-- Command to render an image in a cell rectangle. -/
structure ImageCommand where
  rect : Rect
  source : ImageSource
  protocol : ImageProtocol := .iterm2
  preserveAspectRatio : Bool := true
  name : Option String := none
  deriving Inhabited

namespace ImageCommand

def key (c : ImageCommand) : String :=
  let nameKey := c.name.getD ""
  s!"img:{c.protocol.label}:{c.rect.x},{c.rect.y},{c.rect.width},{c.rect.height}:{c.preserveAspectRatio}:{nameKey}:{c.source.key}"

def offset (c : ImageCommand) (dx dy : Nat) : ImageCommand :=
  { c with rect := { c.rect with x := c.rect.x + dx, y := c.rect.y + dy } }

end ImageCommand

/-- Clipboard selection type for OSC 52. -/
inductive ClipboardSelection where
  | clipboard  -- System clipboard (c)
  | primary    -- Primary selection (p) - X11
  | secondary  -- Secondary selection (s)
  deriving BEq, Inhabited

namespace ClipboardSelection

def code : ClipboardSelection → String
  | .clipboard => "c"
  | .primary => "p"
  | .secondary => "s"

end ClipboardSelection

/-- Command to write text to the system clipboard. -/
structure ClipboardCommand where
  text : String
  selection : ClipboardSelection := .clipboard
  deriving Inhabited

namespace ClipboardCommand

def key (c : ClipboardCommand) : String :=
  s!"clip:{c.selection.code}:{c.text.length}:{fnv1a64String c.text}"

end ClipboardCommand

/-- Terminal commands emitted during rendering. -/
inductive TerminalCommand where
  | image (cmd : ImageCommand)
  | clipboard (cmd : ClipboardCommand)
  deriving Inhabited

namespace TerminalCommand

def key : TerminalCommand → String
  | .image c => c.key
  | .clipboard c => c.key

def offset (cmd : TerminalCommand) (dx dy : Nat) : TerminalCommand :=
  match cmd with
  | .image c => .image (c.offset dx dy)
  | .clipboard c => .clipboard c  -- Clipboard doesn't need offset

def rect? : TerminalCommand → Option Rect
  | .image c => some c.rect
  | .clipboard _ => none

/-- Create a clipboard write command -/
def copyToClipboard (text : String) : TerminalCommand :=
  .clipboard { text := text }

end TerminalCommand

end Terminus
