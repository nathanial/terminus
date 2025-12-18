-- Terminus.Core.Cell: Single styled character

import Terminus.Core.Style

namespace Terminus

/-- A single cell in the terminal buffer -/
structure Cell where
  char : Char := ' '
  style : Style := {}
  deriving Repr, BEq, Inhabited

namespace Cell

def empty : Cell := {}

def new (c : Char) : Cell := { char := c }

def styled (c : Char) (s : Style) : Cell := { char := c, style := s }

def withChar (cell : Cell) (c : Char) : Cell := { cell with char := c }

def withStyle (cell : Cell) (s : Style) : Cell := { cell with style := s }

def setFg (cell : Cell) (c : Color) : Cell := { cell with style := cell.style.withFg c }

def setBg (cell : Cell) (c : Color) : Cell := { cell with style := cell.style.withBg c }

def reset : Cell := empty

end Cell

end Terminus
