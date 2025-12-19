-- Terminus.Backend.Commands: Non-cell terminal commands (e.g. images)

import Terminus.Core.Rect

namespace Terminus

/-- Binary source for an image payload. -/
inductive ImageSource where
  | bytes (data : ByteArray)
  | path (path : System.FilePath)
  deriving Inhabited

namespace ImageSource

private def fnv1a64 (bytes : ByteArray) : UInt64 := Id.run do
  let mut h : UInt64 := 0xcbf29ce484222325
  for b in bytes.data.toList do
    h := h ^^^ (UInt64.ofNat b.toNat)
    h := h * 0x00000100000001B3
  h

def key : ImageSource → String
  | .path p => s!"path:{p}"
  | .bytes b => s!"bytes:{b.size}:{fnv1a64 b}"

end ImageSource

/-- Terminal image protocol. -/
inductive ImageProtocol where
  | iterm2
  deriving BEq, Inhabited

namespace ImageProtocol

def label : ImageProtocol → String
  | .iterm2 => "iterm2"

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

/-- Terminal commands emitted during rendering. -/
inductive TerminalCommand where
  | image (cmd : ImageCommand)
  deriving Inhabited

namespace TerminalCommand

def key : TerminalCommand → String
  | .image c => c.key

def offset (cmd : TerminalCommand) (dx dy : Nat) : TerminalCommand :=
  match cmd with
  | .image c => .image (c.offset dx dy)

def rect? : TerminalCommand → Option Rect
  | .image c => some c.rect

end TerminalCommand

end Terminus
