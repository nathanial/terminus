-- Terminus.Core.Cell: Single styled character

import Terminus.Core.Style

namespace Terminus

/-- A single cell in the terminal buffer -/
structure Cell where
  char : Char := ' '
  style : Style := {}
  hyperlink : Option String := none
  /-- True if this cell is a placeholder for the 2nd column of a wide character -/
  isPlaceholder : Bool := false
  deriving Repr, BEq, Inhabited

namespace Cell

def empty : Cell := {}

/-- Placeholder cell for the second column of a wide (double-width) character -/
def placeholder : Cell := { isPlaceholder := true }

def new (c : Char) : Cell := { char := c }

def styled (c : Char) (s : Style) : Cell := { char := c, style := s }

def withChar (cell : Cell) (c : Char) : Cell := { cell with char := c }

def withStyle (cell : Cell) (s : Style) : Cell := { cell with style := s }

def setFg (cell : Cell) (c : Color) : Cell := { cell with style := cell.style.withFg c }

def setBg (cell : Cell) (c : Color) : Cell := { cell with style := cell.style.withBg c }

def withHyperlink (cell : Cell) (url : String) : Cell := { cell with hyperlink := some url }

def clearHyperlink (cell : Cell) : Cell := { cell with hyperlink := none }

/-- Create a styled cell with a hyperlink -/
def link (c : Char) (s : Style) (url : String) : Cell := { char := c, style := s, hyperlink := some url }

def reset : Cell := empty

end Cell

end Terminus
