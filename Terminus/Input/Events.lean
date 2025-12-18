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

/-- Parse an escape sequence into a KeyEvent -/
def parseEscapeSequence [Monad m] [TerminalEffect m] : m KeyEvent := do
  -- After ESC, check for [ which indicates CSI sequence
  match ← readByteOpt with
  | none => pure KeyEvent.escape
  | some 91 => -- '['
    -- CSI sequence
    match ← readByteOpt with
    | none => pure KeyEvent.escape
    | some b =>
      match b with
      | 65 => pure KeyEvent.up      -- A
      | 66 => pure KeyEvent.down    -- B
      | 67 => pure KeyEvent.right   -- C
      | 68 => pure KeyEvent.left    -- D
      | 72 => pure { code := .home }    -- H
      | 70 => pure { code := .«end» }   -- F
      | 49 => -- '1' - could be F5-F8 or Home
        match ← readByteOpt with
        | some 126 => pure { code := .home } -- ~
        | some 53 => -- '5' - F5
          discard readByteOpt -- consume ~
          pure { code := .f 5 }
        | some 55 => -- '7' - F6
          discard readByteOpt
          pure { code := .f 6 }
        | some 56 => -- '8' - F7
          discard readByteOpt
          pure { code := .f 7 }
        | some 57 => -- '9' - F8
          discard readByteOpt
          pure { code := .f 8 }
        | _ => pure KeyEvent.escape
      | 50 => -- '2' - Insert or F9-F12
        match ← readByteOpt with
        | some 126 => pure { code := .insert } -- ~
        | some 48 => -- '0' - F9
          discard readByteOpt
          pure { code := .f 9 }
        | some 49 => -- '1' - F10
          discard readByteOpt
          pure { code := .f 10 }
        | some 51 => -- '3' - F11
          discard readByteOpt
          pure { code := .f 11 }
        | some 52 => -- '4' - F12
          discard readByteOpt
          pure { code := .f 12 }
        | _ => pure KeyEvent.escape
      | 51 => -- '3' - Delete
        discard readByteOpt -- consume ~
        pure { code := .delete }
      | 52 => -- '4' - End
        discard readByteOpt
        pure { code := .«end» }
      | 53 => -- '5' - Page Up
        discard readByteOpt
        pure { code := .pageUp }
      | 54 => -- '6' - Page Down
        discard readByteOpt
        pure { code := .pageDown }
      | _ => pure KeyEvent.escape
  | some 79 => -- 'O' - SS3 sequence (F1-F4)
    match ← readByteOpt with
    | some 80 => pure { code := .f 1 } -- P
    | some 81 => pure { code := .f 2 } -- Q
    | some 82 => pure { code := .f 3 } -- R
    | some 83 => pure { code := .f 4 } -- S
    | _ => pure KeyEvent.escape
  | some b =>
    -- Alt + key
    if b >= 97 && b <= 122 then -- a-z
      pure { code := .char (Char.ofNat b.toNat), modifiers := KeyModifiers.mkAlt }
    else if b >= 65 && b <= 90 then -- A-Z
      pure { code := .char (Char.ofNat b.toNat), modifiers := { alt := true, shift := true } }
    else
      pure KeyEvent.escape

/-- Parse a single byte into a KeyEvent -/
def parseKey [Monad m] [TerminalEffect m] (b : UInt8) : m KeyEvent := do
  match b with
  | 0 => pure { code := .null }
  | 3 => pure { code := .char 'c', modifiers := KeyModifiers.mkCtrl } -- Ctrl+C
  | 4 => pure { code := .char 'd', modifiers := KeyModifiers.mkCtrl } -- Ctrl+D
  | 9 => pure KeyEvent.tab
  | 10 | 13 => pure KeyEvent.enter
  | 17 => pure { code := .char 'q', modifiers := KeyModifiers.mkCtrl } -- Ctrl+Q
  | 27 => parseEscapeSequence -- Escape or start of sequence
  | 127 => pure KeyEvent.backspace
  | _ =>
    if b >= 1 && b <= 26 then
      -- Ctrl+A through Ctrl+Z
      let c := Char.ofNat (b.toNat + 96)
      pure { code := .char c, modifiers := KeyModifiers.mkCtrl }
    else if b == 32 then
      pure KeyEvent.space
    else if b >= 32 && b < 127 then
      pure (KeyEvent.char (Char.ofNat b.toNat))
    else
      pure { code := .null }

/-- Poll for the next event (non-blocking) -/
def poll [Monad m] [TerminalEffect m] : m Event := do
  match ← readByteOpt with
  | none => pure Event.none
  | some b =>
    let key ← parseKey b
    pure (Event.key key)

/-- Wait for the next key event (blocking) - IO specific due to sleep -/
partial def read : IO KeyEvent := do
  match ← poll with
  | Event.key e => pure e
  | Event.resize _ _ => pure { code := .null } -- Could return resize event
  | Event.none =>
    IO.sleep 10 -- Small delay to avoid busy-waiting
    read

/-- Check if any input is available -/
def available [Monad m] [TerminalEffect m] : m Bool := do
  match ← readByteOpt with
  | some _ => pure true
  | none => pure false

end Events

end Terminus
