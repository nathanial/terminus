-- Terminus.Input.Events: Event polling and parsing

import Terminus.Input.Key
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalIO

namespace Terminus

namespace Events

/-- Read a byte with timeout, returns none if no input -/
def readByteOpt [Monad m] [TerminalEffect m] : m (Option UInt8) := TerminalEffect.readByte

/-- Try to read multiple bytes (for escape sequences) -/
def readBytes [Monad m] [TerminalEffect m] (count : Nat) : m (List UInt8) := do
  let mut bytes : List UInt8 := []
  for _ in [0 : count] do
    match ← readByteOpt with
    | some b => bytes := bytes ++ [b]
    | none => break
  pure bytes

/-- Read digits until a non-digit byte, returning the number and the terminating byte -/
private def readNumber [Monad m] [TerminalEffect m] : m (Nat × Option UInt8) := do
  let mut n : Nat := 0
  let mut term : Option UInt8 := none
  for _ in [0 : 10] do  -- Max 10 digits to prevent infinite loop
    match ← readByteOpt with
    | none =>
      term := none
      break
    | some b =>
      if b >= 48 && b <= 57 then  -- '0'-'9'
        n := n * 10 + (b.toNat - 48)
      else
        term := some b
        break
  pure (n, term)

/-- Parse SGR mouse event: ESC [ < Cb ; Cx ; Cy M|m
    Button encoding:
      0-2: left, middle, right
      3: release (no button)
      64-67: scroll (up, down, left, right)
      +4: shift, +8: alt, +16: ctrl, +32: motion
    M = press, m = release -/
private def parseMouseSGR [Monad m] [TerminalEffect m] : m (Option MouseEvent) := do
  -- Read button code until ';'
  let (buttonCode, term1) ← readNumber
  if term1 != some 59 then return none  -- ';' = 59

  -- Read x until ';'
  let (x, term2) ← readNumber
  if term2 != some 59 then return none

  -- Read y until 'M' or 'm'
  let (y, term3) ← readNumber
  let isRelease := term3 == some 109  -- 'm'
  let isPress := term3 == some 77     -- 'M'
  if !isRelease && !isPress then return none

  -- Decode button (bits 0-1, or 64+ for scroll)
  let isMotion := buttonCode &&& 32 != 0
  let baseButton := buttonCode &&& 0x43  -- Mask: keep bits 0,1 and 6 (for scroll)

  let button : MouseButton := match baseButton with
    | 0  => .left
    | 1  => .middle
    | 2  => .right
    | 3  => .none        -- No button (motion or release)
    | 64 => .scrollUp
    | 65 => .scrollDown
    | _  => .none

  -- Decode modifiers
  let modifiers : KeyModifiers := {
    shift := buttonCode &&& 4 != 0
    alt   := buttonCode &&& 8 != 0
    ctrl  := buttonCode &&& 16 != 0
  }

  -- Determine action
  let action : MouseAction :=
    if isMotion then .motion
    else if isRelease then .release
    else .press

  pure (some { x, y, button, action, modifiers })

/-- Parse an escape sequence into an Event -/
def parseEscapeSequence [Monad m] [TerminalEffect m] : m Event := do
  -- After ESC, check for [ which indicates CSI sequence
  match ← readByteOpt with
  | none => pure (.key KeyEvent.escape)
  | some 91 => -- '['
    -- CSI sequence
    match ← readByteOpt with
    | none => pure (.key KeyEvent.escape)
    | some b =>
      match b with
      | 60 => -- '<' - SGR mouse sequence
        match ← parseMouseSGR with
        | some me => pure (.mouse me)
        | none => pure (.key KeyEvent.escape)
      | 65 => pure (.key KeyEvent.up)      -- A
      | 66 => pure (.key KeyEvent.down)    -- B
      | 67 => pure (.key KeyEvent.right)   -- C
      | 68 => pure (.key KeyEvent.left)    -- D
      | 90 => pure (.key KeyEvent.tab.withShift) -- Z (Shift+Tab)
      | 72 => pure (.key { code := .home })    -- H
      | 70 => pure (.key { code := .«end» })   -- F
      | 49 => -- '1' - could be F5-F8 or Home
        match ← readByteOpt with
        | some 126 => pure (.key { code := .home }) -- ~
        | some 53 => -- '5' - F5
          discard readByteOpt -- consume ~
          pure (.key { code := .f 5 })
        | some 55 => -- '7' - F6
          discard readByteOpt
          pure (.key { code := .f 6 })
        | some 56 => -- '8' - F7
          discard readByteOpt
          pure (.key { code := .f 7 })
        | some 57 => -- '9' - F8
          discard readByteOpt
          pure (.key { code := .f 8 })
        | _ => pure (.key KeyEvent.escape)
      | 50 => -- '2' - Insert or F9-F12
        match ← readByteOpt with
        | some 126 => pure (.key { code := .insert }) -- ~
        | some 48 => -- '0' - F9
          discard readByteOpt
          pure (.key { code := .f 9 })
        | some 49 => -- '1' - F10
          discard readByteOpt
          pure (.key { code := .f 10 })
        | some 51 => -- '3' - F11
          discard readByteOpt
          pure (.key { code := .f 11 })
        | some 52 => -- '4' - F12
          discard readByteOpt
          pure (.key { code := .f 12 })
        | _ => pure (.key KeyEvent.escape)
      | 51 => -- '3' - Delete
        discard readByteOpt -- consume ~
        pure (.key { code := .delete })
      | 52 => -- '4' - End
        discard readByteOpt
        pure (.key { code := .«end» })
      | 53 => -- '5' - Page Up
        discard readByteOpt
        pure (.key { code := .pageUp })
      | 54 => -- '6' - Page Down
        discard readByteOpt
        pure (.key { code := .pageDown })
      | _ => pure (.key KeyEvent.escape)
  | some 79 => -- 'O' - SS3 sequence (F1-F4)
    match ← readByteOpt with
    | some 80 => pure (.key { code := .f 1 }) -- P
    | some 81 => pure (.key { code := .f 2 }) -- Q
    | some 82 => pure (.key { code := .f 3 }) -- R
    | some 83 => pure (.key { code := .f 4 }) -- S
    | _ => pure (.key KeyEvent.escape)
  | some b =>
    -- Alt + key
    if b >= 97 && b <= 122 then -- a-z
      pure (.key { code := .char (Char.ofNat b.toNat), modifiers := KeyModifiers.mkAlt })
    else if b >= 65 && b <= 90 then -- A-Z
      pure (.key { code := .char (Char.ofNat b.toNat), modifiers := { alt := true, shift := true } })
    else
      pure (.key KeyEvent.escape)

/-- Parse a single byte into an Event -/
def parseInput [Monad m] [TerminalEffect m] (b : UInt8) : m Event := do
  match b with
  | 0 => pure (.key { code := .null })
  | 3 => pure (.key { code := .char 'c', modifiers := KeyModifiers.mkCtrl }) -- Ctrl+C
  | 4 => pure (.key { code := .char 'd', modifiers := KeyModifiers.mkCtrl }) -- Ctrl+D
  | 9 => pure (.key KeyEvent.tab)
  | 10 | 13 => pure (.key KeyEvent.enter)
  | 17 => pure (.key { code := .char 'q', modifiers := KeyModifiers.mkCtrl }) -- Ctrl+Q
  | 27 => parseEscapeSequence -- Escape or start of sequence (may return mouse event)
  | 127 => pure (.key KeyEvent.backspace)
  | _ =>
    if b >= 1 && b <= 26 then
      -- Ctrl+A through Ctrl+Z
      let c := Char.ofNat (b.toNat + 96)
      pure (.key { code := .char c, modifiers := KeyModifiers.mkCtrl })
    else if b == 32 then
      pure (.key KeyEvent.space)
    else if b >= 32 && b < 127 then
      pure (.key (KeyEvent.char (Char.ofNat b.toNat)))
    else
      pure (.key { code := .null })

/-- Poll for the next event (non-blocking) -/
def poll [Monad m] [TerminalEffect m] : m Event := do
  match ← readByteOpt with
  | none => pure Event.none
  | some b => parseInput b

/-- Wait for the next event (blocking). -/
partial def read : IO Event := do
  match ← TerminalEffect.readByteBlocking with
  | some b => parseInput b
  | none => read

/-- Wait for the next key event (blocking), ignoring mouse events -/
partial def readKey : IO KeyEvent := do
  match ← read with
  | .key ke => pure ke
  | _ => readKey

/-- Wait for the next event asynchronously on a dedicated task. -/
def readAsync : IO (Task (Except IO.Error Event)) := do
  IO.asTask (prio := .dedicated) read

/-- Wait for the next key event asynchronously, ignoring mouse events. -/
def readKeyAsync : IO (Task (Except IO.Error KeyEvent)) := do
  IO.asTask (prio := .dedicated) readKey

/-- Check if any input is available -/
def available [Monad m] [TerminalEffect m] : m Bool := do
  match ← readByteOpt with
  | some b =>
    TerminalEffect.unreadByte b
    pure true
  | none => pure false

end Events

end Terminus
