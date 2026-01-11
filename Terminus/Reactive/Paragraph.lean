/-
  Terminus Reactive - Paragraph Widget
  Multi-line text display with wrapping and alignment.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Text Types -/

/-- Text alignment options. -/
inductive TextAlignment where
  | left
  | center
  | right
  deriving Repr, BEq, Inhabited

/-- Text wrapping mode. -/
inductive WrapMode' where
  /-- No wrapping - truncate at boundary. -/
  | noWrap
  /-- Wrap at word boundaries. -/
  | wrap
  /-- Wrap at character boundaries. -/
  | charWrap
  deriving Repr, BEq, Inhabited

/-- A styled text span. -/
structure TextSpan where
  /-- Text content. -/
  content : String
  /-- Style for this span. -/
  style : Style := {}
  deriving Repr, Inhabited

namespace TextSpan

/-- Create a plain text span. -/
def new (s : String) : TextSpan := { content := s }

/-- Create a styled text span. -/
def styled (s : String) (style : Style) : TextSpan := { content := s, style }

end TextSpan

/-- A line composed of styled spans. -/
structure TextLine where
  /-- Spans in this line. -/
  spans : Array TextSpan := #[]
  deriving Repr, Inhabited

namespace TextLine

/-- Create a line from plain text. -/
def new (s : String) : TextLine := { spans := #[TextSpan.new s] }

/-- Create a line from styled text. -/
def styled (s : String) (style : Style) : TextLine := { spans := #[TextSpan.styled s style] }

/-- Create a line from spans. -/
def fromSpans (spans : Array TextSpan) : TextLine := { spans }

/-- Get the display width of the line. -/
def width (l : TextLine) : Nat :=
  l.spans.foldl (fun acc span => acc + span.content.length) 0

/-- Get the full text content. -/
def text (l : TextLine) : String :=
  l.spans.foldl (fun acc span => acc ++ span.content) ""

end TextLine

/-! ## Paragraph Configuration -/

/-- Configuration for paragraph appearance. -/
structure ParagraphConfig where
  /-- Base style for text. -/
  style : Style := {}
  /-- Text alignment. -/
  alignment : TextAlignment := .left
  /-- Wrap mode. -/
  wrapMode : WrapMode' := .noWrap
  /-- Maximum width for wrapping (none = use container width). -/
  maxWidth : Option Nat := none
  deriving Repr, Inhabited

/-! ## Word Wrapping -/

/-- Word wrap a string to fit within a width. -/
private def wordWrap (s : String) (width : Nat) : Array String := Id.run do
  if width == 0 then return #[]
  let words := s.splitOn " "
  let mut result : Array String := #[]
  let mut currentLine := ""
  let mut currentWidth : Nat := 0

  for word in words do
    let wordWidth := word.length
    if currentLine.isEmpty then
      currentLine := word
      currentWidth := wordWidth
    else if currentWidth + 1 + wordWidth <= width then
      currentLine := currentLine ++ " " ++ word
      currentWidth := currentWidth + 1 + wordWidth
    else
      result := result.push currentLine
      currentLine := word
      currentWidth := wordWidth

  if !currentLine.isEmpty then
    result := result.push currentLine

  result

/-- Character wrap a string to fit within a width. -/
private def charWrap (s : String) (width : Nat) : Array String := Id.run do
  if width == 0 then return #[]
  let mut result : Array String := #[]
  let mut current := ""
  let mut currentWidth : Nat := 0

  for c in s.toList do
    let charWidth := 1  -- Simplified; could use displayWidth
    if currentWidth + charWidth > width && !current.isEmpty then
      result := result.push current
      current := ""
      currentWidth := 0
    current := current.push c
    currentWidth := currentWidth + charWidth

  if !current.isEmpty then
    result := result.push current

  result

/-- Calculate x offset for alignment. -/
private def alignOffset (textWidth areaWidth : Nat) (align : TextAlignment) : Nat :=
  match align with
  | .left => 0
  | .center => if areaWidth > textWidth then (areaWidth - textWidth) / 2 else 0
  | .right => if areaWidth > textWidth then areaWidth - textWidth else 0

/-! ## Paragraph Widget -/

/-- Create a paragraph widget from plain text.

    The paragraph displays multi-line text with optional wrapping and alignment.

    Example:
    ```
    paragraph' "Hello, world!\nThis is a paragraph." { alignment := .center }
    ```
-/
def paragraph' (text : String) (config : ParagraphConfig := {}) : WidgetM Unit := do
  let lines := text.splitOn "\n"

  emit do
    let maxWidth := config.maxWidth.getD 80  -- Default max width

    -- Process lines based on wrap mode
    let processedLines : Array String := match config.wrapMode with
      | .noWrap => lines.toArray
      | .wrap => lines.foldl (fun acc line => acc ++ wordWrap line maxWidth) #[]
      | .charWrap => lines.foldl (fun acc line => acc ++ charWrap line maxWidth) #[]

    let nodes := processedLines.map fun line =>
      let offset := alignOffset line.length maxWidth config.alignment
      let padding := String.mk (List.replicate offset ' ')
      RNode.text (padding ++ line) config.style

    pure (RNode.column 0 {} nodes)

/-- Create a paragraph widget from styled lines.

    Example:
    ```
    let lines := #[
      TextLine.styled "Title" { modifier := { bold := true } },
      TextLine.new "Regular text"
    ]
    styledParagraph' lines {}
    ```
-/
def styledParagraph' (lines : Array TextLine) (config : ParagraphConfig := {}) : WidgetM Unit := do
  emit do
    let maxWidth := config.maxWidth.getD 80

    let nodes := lines.map fun line =>
      let offset := alignOffset line.width maxWidth config.alignment
      let padding := if offset > 0 then #[RNode.text (String.mk (List.replicate offset ' ')) {}] else #[]
      let spans := line.spans.map fun span =>
        RNode.text span.content (Style.merge config.style span.style)
      RNode.row 0 {} (padding ++ spans)

    pure (RNode.column 0 {} nodes)

/-- Create a dynamic paragraph widget.

    Example:
    ```
    let textDyn ← someTextSource
    dynParagraph' textDyn { wrapMode := .wrap }
    ```
-/
def dynParagraph' (text : Reactive.Dynamic Spider String) (config : ParagraphConfig := {})
    : WidgetM Unit := do
  emitDynamic do
    let content ← text.sample
    let lines := content.splitOn "\n"
    let maxWidth := config.maxWidth.getD 80

    let processedLines : Array String := match config.wrapMode with
      | .noWrap => lines.toArray
      | .wrap => lines.foldl (fun acc line => acc ++ wordWrap line maxWidth) #[]
      | .charWrap => lines.foldl (fun acc line => acc ++ charWrap line maxWidth) #[]

    let nodes := processedLines.map fun line =>
      let offset := alignOffset line.length maxWidth config.alignment
      let padding := String.mk (List.replicate offset ' ')
      RNode.text (padding ++ line) config.style

    pure (RNode.column 0 {} nodes)

/-! ## Convenience Functions -/

/-- Create a centered paragraph. -/
def centeredParagraph' (text : String) (config : ParagraphConfig := {}) : WidgetM Unit :=
  paragraph' text { config with alignment := .center }

/-- Create a right-aligned paragraph. -/
def rightParagraph' (text : String) (config : ParagraphConfig := {}) : WidgetM Unit :=
  paragraph' text { config with alignment := .right }

/-- Create a wrapped paragraph. -/
def wrappedParagraph' (text : String) (width : Nat) (config : ParagraphConfig := {}) : WidgetM Unit :=
  paragraph' text { config with wrapMode := .wrap, maxWidth := some width }

end Terminus.Reactive
