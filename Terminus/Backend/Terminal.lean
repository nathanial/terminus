-- Terminus.Backend.Terminal: High-level terminal operations

import Terminus.Core.Buffer
import Terminus.Core.Cell
import Terminus.Core.Base64
import Terminus.Core.Sixel
import Terminus.Backend.Ansi
import Terminus.Backend.Commands
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalIO

namespace Terminus

/-- Terminal state for rendering -/
structure Terminal where
  width : Nat
  height : Nat
  currentBuffer : Buffer
  previousBuffer : Buffer
  previousCommandKeys : List String := []
  previousImageRects : List Rect := []
  deriving Inhabited

namespace Terminal

/-- Create a new terminal with the current size -/
def new [Monad m] [TerminalEffect m] : m Terminal := do
  let (width, height) ← TerminalEffect.getTerminalSize
  pure {
    width
    height
    currentBuffer := Buffer.new width height
    previousBuffer := Buffer.new width height
    previousCommandKeys := []
    previousImageRects := []
  }

/-- Resize the terminal (call when window size changes) -/
def resize (term : Terminal) (width height : Nat) : Terminal := {
  width
  height
  currentBuffer := term.currentBuffer.resize width height
  previousBuffer := term.previousBuffer.resize width height
  previousCommandKeys := []
  previousImageRects := []
}

/-- Get the terminal's drawable area as a Rect -/
def area (term : Terminal) : Rect := {
  x := 0
  y := 0
  width := term.width
  height := term.height
}

/-- Clear the terminal screen -/
def clear [Monad m] [TerminalEffect m] : m Unit := do
  TerminalEffect.writeStdout (Ansi.bgColor .default)
  TerminalEffect.writeStdout Ansi.clearScreen
  TerminalEffect.writeStdout Ansi.cursorHome

/-- Hide the cursor -/
def hideCursor [Monad m] [TerminalEffect m] : m Unit := TerminalEffect.writeStdout Ansi.cursorHide

/-- Show the cursor -/
def showCursor [Monad m] [TerminalEffect m] : m Unit := TerminalEffect.writeStdout Ansi.cursorShow

/-- Enter the alternate screen buffer -/
def enterAltScreen [Monad m] [TerminalEffect m] : m Unit := TerminalEffect.writeStdout Ansi.enterAltScreen

/-- Leave the alternate screen buffer -/
def leaveAltScreen [Monad m] [TerminalEffect m] : m Unit := TerminalEffect.writeStdout Ansi.leaveAltScreen

/-- Move cursor to position (0-indexed) -/
def moveCursor [Monad m] [TerminalEffect m] (x y : Nat) : m Unit := TerminalEffect.writeStdout (Ansi.cursorToZero x y)

/-- Render a cell at the given position -/
private def renderCell [Monad m] [TerminalEffect m] (x y : Nat) (cell : Cell) (prevLink : Option String := none) : m (Option String) := do
  TerminalEffect.writeStdout (Ansi.cursorToZero x y)
  TerminalEffect.writeStdout (Ansi.styleCodes cell.style)
  -- Handle hyperlink transitions
  let newLink := cell.hyperlink
  if newLink != prevLink then
    -- End previous hyperlink if any
    if prevLink.isSome then
      TerminalEffect.writeStdout Ansi.hyperlinkEnd
    -- Start new hyperlink if any
    match newLink with
    | some url => TerminalEffect.writeStdout (Ansi.hyperlinkStart url)
    | none => pure ()
  TerminalEffect.writeStdout cell.char.toString
  pure newLink

/-- Force redraw of a rectangular region from the current buffer.
This is used to "erase" non-cell overlays (e.g. inline images) by overwriting with cells. -/
private def redrawRect [Monad m] [TerminalEffect m] (term : Terminal) (r : Rect) : m Unit := do
  let x0 := r.x
  let y0 := r.y
  let x1 := min (r.x + r.width) term.width
  let y1 := min (r.y + r.height) term.height
  let mut lastLink : Option String := none
  for y in [y0 : y1] do
    for x in [x0 : x1] do
      lastLink ← renderCell x y (term.currentBuffer.get x y) lastLink
  -- End any active hyperlink
  if lastLink.isSome then
    TerminalEffect.writeStdout Ansi.hyperlinkEnd

private def iterm2ImageEscape (payloadB64 : String) (nameB64 : Option String) (w h : Nat) (preserve : Bool) : String :=
  let esc := "\x1b]1337;File="
  let namePart :=
    match nameB64 with
    | none => ""
    | some n => s!"name={n};"
  let preservePart := if preserve then "preserveAspectRatio=1;" else "preserveAspectRatio=0;"
  -- width/height are in cells (iTerm2/WezTerm accept this form)
  let params := s!"inline=1;{preservePart}{namePart}width={w};height={h}:"
  esc ++ params ++ payloadB64 ++ "\x07"

/-- Convert decoded RGB bytes to Sixel RawImage -/
private def bytesToSixelImage (width height : Nat) (data : ByteArray) : Option Sixel.RawImage :=
  Sixel.RawImage.fromRGB width height data

private def applyCommands [Monad m] [TerminalEffect m] (term : Terminal) (cmds : List TerminalCommand) : m Terminal := do
  let keys := cmds.map TerminalCommand.key
  let imageRects := cmds.foldl (fun acc c => match c.rect? with | some r => r :: acc | none => acc) [] |>.reverse
  -- If any prior image rects disappeared (or changed), redraw those regions from the buffer to "erase" overlays.
  let removed := term.previousImageRects.filter (fun r => !imageRects.contains r)
  for r in removed do
    redrawRect term r

  -- Avoid re-sending identical command sets every tick (large image payloads).
  -- Keys include path for file-based images, so unchanged paths won't re-encode.
  if keys == term.previousCommandKeys then
    pure { term with previousCommandKeys := keys, previousImageRects := imageRects }
  else
    for cmd in cmds do
      match cmd with
      | .image ic =>
        if ic.rect.isEmpty then
          pure ()
        else
          let bytes ←
            match ic.source with
            | .bytes b => pure b
            | .path p => TerminalEffect.readFileBytes p
          match ic.protocol with
          | .iterm2 =>
            let payloadB64 := Base64.encode bytes
            let nameB64 := ic.name.map (fun s => Base64.encode s.toUTF8)
            TerminalEffect.writeStdout (Ansi.cursorToZero ic.rect.x ic.rect.y)
            TerminalEffect.writeStdout (iterm2ImageEscape payloadB64 nameB64 ic.rect.width ic.rect.height ic.preserveAspectRatio)
          | .sixel =>
            -- Decode image bytes to RGB pixels, then encode as Sixel
            let decoded ← TerminalEffect.decodeImageBytes bytes
            match decoded with
            | some (width, height, rgbData) =>
              match bytesToSixelImage width height rgbData with
              | some rawImg =>
                let sixelSeq := Sixel.encodeRaw rawImg
                TerminalEffect.writeStdout (Ansi.cursorToZero ic.rect.x ic.rect.y)
                TerminalEffect.writeStdout sixelSeq
              | none => pure ()  -- Failed to convert to Sixel image
            | none => pure ()  -- Failed to decode image
      | .clipboard cc =>
        -- Write text to system clipboard using OSC 52
        TerminalEffect.writeStdout (Ansi.clipboardWrite cc.text cc.selection.code)

    TerminalEffect.flushStdout
    pure { term with previousCommandKeys := keys, previousImageRects := imageRects }

/-- Flush the current buffer to the terminal using differential updates -/
def flush [Monad m] [TerminalEffect m] (term : Terminal) (commands : List TerminalCommand := []) : m Terminal := do
  let changes := Buffer.diff term.previousBuffer term.currentBuffer
  let mut lastLink : Option String := none
  for (x, y, cell) in changes do
    lastLink ← renderCell x y cell lastLink
  -- End any active hyperlink before reset
  if lastLink.isSome then
    TerminalEffect.writeStdout Ansi.hyperlinkEnd
  TerminalEffect.writeStdout Ansi.resetAll
  TerminalEffect.flushStdout
  let term := { term with previousBuffer := term.currentBuffer }
  applyCommands term commands

/-- Force a full redraw of the buffer -/
def draw [Monad m] [TerminalEffect m] (term : Terminal) : m Terminal := do
  TerminalEffect.writeStdout Ansi.cursorHome
  let mut lastStyle : Style := {}
  let mut lastLink : Option String := none
  TerminalEffect.writeStdout Ansi.resetAll
  for y in [0 : term.height] do
    TerminalEffect.writeStdout (Ansi.cursorToZero 0 y)
    for x in [0 : term.width] do
      let cell := term.currentBuffer.get x y
      if cell.style != lastStyle then
        TerminalEffect.writeStdout (Ansi.styleCodes cell.style)
        lastStyle := cell.style
      -- Handle hyperlink transitions
      if cell.hyperlink != lastLink then
        if lastLink.isSome then
          TerminalEffect.writeStdout Ansi.hyperlinkEnd
        match cell.hyperlink with
        | some url => TerminalEffect.writeStdout (Ansi.hyperlinkStart url)
        | none => pure ()
        lastLink := cell.hyperlink
      TerminalEffect.writeStdout cell.char.toString
  -- End any active hyperlink before reset
  if lastLink.isSome then
    TerminalEffect.writeStdout Ansi.hyperlinkEnd
  TerminalEffect.writeStdout Ansi.resetAll
  TerminalEffect.flushStdout
  pure { term with previousBuffer := term.currentBuffer, previousCommandKeys := [], previousImageRects := [] }

/-- Get a mutable reference to the current buffer for drawing -/
def getBuffer (term : Terminal) : Buffer := term.currentBuffer

/-- Set the buffer after drawing -/
def setBuffer (term : Terminal) (buf : Buffer) : Terminal :=
  { term with currentBuffer := buf }

/-- Clear the buffer (fill with empty cells) -/
def clearBuffer (term : Terminal) : Terminal :=
  { term with currentBuffer := term.currentBuffer.clear }

/-- Standard terminal setup for TUI applications -/
def setup : IO Unit := do
  TerminalEffect.enableRawMode
  try
    enterAltScreen
    hideCursor
    TerminalEffect.writeStdout Ansi.enableMouse
    clear
  catch e =>
    TerminalEffect.writeStdout Ansi.disableMouse
    showCursor
    leaveAltScreen
    TerminalEffect.disableRawMode
    throw e

/-- Standard terminal teardown -/
def teardown : IO Unit := do
  TerminalEffect.writeStdout Ansi.disableMouse
  showCursor
  leaveAltScreen
  TerminalEffect.disableRawMode

/-- Run a TUI application with proper setup and teardown -/
def withTerminal (app : Terminal → IO α) : IO α := do
  setup
  try
    let term ← Terminal.new
    app term
  finally
    teardown

end Terminal

end Terminus
