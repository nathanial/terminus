-- Terminus.Backend.Terminal: High-level terminal operations

import Terminus.Core.Buffer
import Terminus.Core.Cell
import Terminus.Backend.Ansi
import Terminus.Backend.TerminalEffect
import Terminus.Backend.TerminalIO

namespace Terminus

/-- Terminal state for rendering -/
structure Terminal where
  width : Nat
  height : Nat
  currentBuffer : Buffer
  previousBuffer : Buffer
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
  }

/-- Resize the terminal (call when window size changes) -/
def resize (term : Terminal) (width height : Nat) : Terminal := {
  width
  height
  currentBuffer := term.currentBuffer.resize width height
  previousBuffer := term.previousBuffer.resize width height
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
private def renderCell [Monad m] [TerminalEffect m] (x y : Nat) (cell : Cell) : m Unit := do
  TerminalEffect.writeStdout (Ansi.cursorToZero x y)
  TerminalEffect.writeStdout (Ansi.styleCodes cell.style)
  TerminalEffect.writeStdout cell.char.toString

/-- Flush the current buffer to the terminal using differential updates -/
def flush [Monad m] [TerminalEffect m] (term : Terminal) : m Terminal := do
  let changes := Buffer.diff term.previousBuffer term.currentBuffer
  for (x, y, cell) in changes do
    renderCell x y cell
  TerminalEffect.writeStdout Ansi.resetAll
  TerminalEffect.flushStdout
  pure { term with previousBuffer := term.currentBuffer }

/-- Force a full redraw of the buffer -/
def draw [Monad m] [TerminalEffect m] (term : Terminal) : m Terminal := do
  TerminalEffect.writeStdout Ansi.cursorHome
  let mut lastStyle : Style := {}
  TerminalEffect.writeStdout Ansi.resetAll
  for y in [0 : term.height] do
    TerminalEffect.writeStdout (Ansi.cursorToZero 0 y)
    for x in [0 : term.width] do
      let cell := term.currentBuffer.get x y
      if cell.style != lastStyle then
        TerminalEffect.writeStdout (Ansi.styleCodes cell.style)
        lastStyle := cell.style
      TerminalEffect.writeStdout cell.char.toString
  TerminalEffect.writeStdout Ansi.resetAll
  TerminalEffect.flushStdout
  pure { term with previousBuffer := term.currentBuffer }

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
  enterAltScreen
  hideCursor
  clear

/-- Standard terminal teardown -/
def teardown : IO Unit := do
  showCursor
  leaveAltScreen
  TerminalEffect.disableRawMode

/-- Run a TUI application with proper setup and teardown -/
def withTerminal (app : Terminal → IO α) : IO α := do
  setup
  let term ← Terminal.new
  try
    app term
  finally
    teardown

end Terminal

end Terminus
