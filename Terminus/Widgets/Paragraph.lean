-- Terminus.Widgets.Paragraph: Multi-line text widget

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Text alignment options -/
inductive Alignment where
  | left
  | center
  | right
  deriving Repr, BEq, Inhabited

/-- Wrap mode for long lines -/
inductive WrapMode where
  | noWrap    -- Truncate at boundary
  | wrap      -- Wrap at word boundaries
  | charWrap  -- Wrap at character boundaries
  deriving Repr, BEq, Inhabited

/-- Styled text span within a line -/
structure Span where
  content : String
  style : Style := {}
  deriving Repr, Inhabited

namespace Span

def new (s : String) : Span := { content := s }
def styled (s : String) (st : Style) : Span := { content := s, style := st }
def raw (s : String) : Span := { content := s }

end Span

/-- A line of text made up of styled spans -/
structure Line where
  spans : List Span
  deriving Repr, Inhabited

namespace Line

def new (spans : List Span) : Line := { spans }
def raw (s : String) : Line := { spans := [Span.new s] }
def styled (s : String) (st : Style) : Line := { spans := [Span.styled s st] }

def fromString (s : String) : Line := raw s

def width (l : Line) : Nat :=
  l.spans.foldl (fun acc span => acc + span.content.length) 0

end Line

/-- Paragraph widget for rendering multi-line text -/
structure Paragraph where
  lines : List Line := []
  style : Style := {}
  alignment : Alignment := .left
  wrapMode : WrapMode := .noWrap
  block : Option Block := none
  deriving Repr, Inhabited

namespace Paragraph

def new (lines : List Line) : Paragraph := { lines }

def fromString (s : String) : Paragraph := {
  lines := s.splitOn "\n" |>.map Line.fromString
}

def fromLines (strs : List String) : Paragraph := {
  lines := strs.map Line.fromString
}

def withStyle (p : Paragraph) (s : Style) : Paragraph := { p with style := s }
def withAlignment (p : Paragraph) (a : Alignment) : Paragraph := { p with alignment := a }
def withWrap (p : Paragraph) (w : WrapMode) : Paragraph := { p with wrapMode := w }
def withBlock (p : Paragraph) (b : Block) : Paragraph := { p with block := some b }

def centered (p : Paragraph) : Paragraph := p.withAlignment .center
def rightAligned (p : Paragraph) : Paragraph := p.withAlignment .right

/-- Word wrap a string to fit within a width -/
private def wordWrap (s : String) (width : Nat) : List String := Id.run do
  if width == 0 then return []
  let words := s.splitOn " "
  let mut result : List String := []
  let mut currentLine := ""

  for word in words do
    if currentLine.isEmpty then
      currentLine := word
    else if currentLine.length + 1 + word.length <= width then
      currentLine := currentLine ++ " " ++ word
    else
      result := result ++ [currentLine]
      currentLine := word

  if !currentLine.isEmpty then
    result := result ++ [currentLine]

  result

/-- Character wrap a string to fit within a width -/
private def charWrap (s : String) (width : Nat) : List String := Id.run do
  if width == 0 then return []
  let mut result : List String := []
  let mut current := ""

  for c in s.toList do
    if current.length >= width then
      result := result ++ [current]
      current := ""
    current := current.push c

  if !current.isEmpty then
    result := result ++ [current]

  result

/-- Calculate x offset for alignment -/
private def alignOffset (textWidth areaWidth : Nat) (align : Alignment) : Nat :=
  match align with
  | .left => 0
  | .center => if areaWidth > textWidth then (areaWidth - textWidth) / 2 else 0
  | .right => if areaWidth > textWidth then areaWidth - textWidth else 0

/-- Render a single line at the given position -/
private def renderLine (line : Line) (x y : Nat) (maxWidth : Nat) (baseStyle : Style) (buf : Buffer) : Buffer := Id.run do
  let mut result := buf
  let mut col := x

  for span in line.spans do
    let style := Style.merge baseStyle span.style
    for c in span.content.toList do
      if col >= x + maxWidth then break
      result := result.setStyled col y c style
      col := col + 1

  result

end Paragraph

instance : Widget Paragraph where
  render p area buf := Id.run do
    -- First render the block if present
    let mut result := match p.block with
      | some block => Widget.render block area buf
      | none => buf

    -- Get the content area
    let contentArea := match p.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty then return result

    -- Process lines based on wrap mode
    let processedLines : List Line := match p.wrapMode with
      | .noWrap => p.lines
      | .wrap =>
        p.lines.flatMap fun line =>
          let fullText := line.spans.foldl (fun acc span => acc ++ span.content) ""
          Paragraph.wordWrap fullText contentArea.width |>.map Line.fromString
      | .charWrap =>
        p.lines.flatMap fun line =>
          let fullText := line.spans.foldl (fun acc span => acc ++ span.content) ""
          Paragraph.charWrap fullText contentArea.width |>.map Line.fromString

    -- Render each line
    let mut row := contentArea.y
    for line in processedLines do
      if row >= contentArea.y + contentArea.height then break

      let xOffset := Paragraph.alignOffset line.width contentArea.width p.alignment
      result := Paragraph.renderLine line (contentArea.x + xOffset) row contentArea.width p.style result
      row := row + 1

    result

end Terminus
