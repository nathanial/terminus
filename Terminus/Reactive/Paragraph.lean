/-
  Terminus Reactive - Paragraph Widget
  Multi-line text display with wrapping and alignment.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Terminus.Reactive.Hooks
import Terminus.Core.Unicode
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

/-- Get the display width of the line (accounts for wide CJK/emoji characters). -/
def width (l : TextLine) : Nat :=
  l.spans.foldl (fun acc span => acc + span.content.displayWidth) 0

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

/-- Word wrap a string to fit within a width (using display width for Unicode support). -/
private def wordWrap (s : String) (width : Nat) : Array String := Id.run do
  if width == 0 then return #[]
  let words := s.splitOn " "
  let mut result : Array String := #[]
  let mut currentLine := ""
  let mut currentWidth : Nat := 0

  for word in words do
    let wordWidth := word.displayWidth
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

/-- Character wrap a string to fit within a width (using display width for Unicode support). -/
private def charWrap (s : String) (width : Nat) : Array String := Id.run do
  if width == 0 then return #[]
  let mut result : Array String := #[]
  let mut current := ""
  let mut currentWidth : Nat := 0

  for c in s.toList do
    let charWidth := c.displayWidth
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
  let maxWidth := config.maxWidth.getD 80  -- Default max width

  let processedLines : Array String := match config.wrapMode with
    | .noWrap => lines.toArray
    | .wrap => lines.foldl (fun acc line => acc ++ wordWrap line maxWidth) #[]
    | .charWrap => lines.foldl (fun acc line => acc ++ charWrap line maxWidth) #[]

  let nodes := processedLines.map fun line =>
    let offset := alignOffset line.displayWidth maxWidth config.alignment
    let padding := String.ofList (List.replicate offset ' ')
    RNode.text (padding ++ line) config.style

  emitStatic (RNode.column 0 {} nodes)

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
  let maxWidth := config.maxWidth.getD 80

  let nodes := lines.map fun line =>
    let offset := alignOffset line.width maxWidth config.alignment
    let padding := if offset > 0 then #[RNode.text (String.ofList (List.replicate offset ' ')) {}] else #[]
    let spans := line.spans.map fun span =>
      RNode.text span.content (Style.merge config.style span.style)
    RNode.row 0 {} (padding ++ spans)

  emitStatic (RNode.column 0 {} nodes)

/-- Create a dynamic paragraph widget.

    Example:
    ```
    let textDyn ← someTextSource
    dynParagraph' textDyn { wrapMode := .wrap }
    ```
-/
def dynParagraph' (text : Reactive.Dynamic Spider String) (config : ParagraphConfig := {})
    : WidgetM Unit := do
  let node ← text.map' fun content =>
    let lines := content.splitOn "\n"
    let maxWidth := config.maxWidth.getD 80

    let processedLines : Array String := match config.wrapMode with
      | .noWrap => lines.toArray
      | .wrap => lines.foldl (fun acc line => acc ++ wordWrap line maxWidth) #[]
      | .charWrap => lines.foldl (fun acc line => acc ++ charWrap line maxWidth) #[]

    let nodes := processedLines.map fun line =>
      let offset := alignOffset line.displayWidth maxWidth config.alignment
      let padding := String.ofList (List.replicate offset ' ')
      RNode.text (padding ++ line) config.style

    RNode.column 0 {} nodes
  emit node

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

/-! ## Horizontal Scrolling Support -/

/-- Extract a substring by display column range, respecting character display widths.
    Returns the characters that fall within the [startCol, startCol + width) range.
    Wide characters (CJK, emoji) that would be partially visible are included. -/
def sliceByDisplayWidth (s : String) (startCol : Nat) (width : Nat) : String := Id.run do
  if width == 0 then return ""
  let endCol := startCol + width
  let mut result := ""
  let mut col : Nat := 0

  for c in s.toList do
    let charWidth := c.displayWidth
    let charEnd := col + charWidth
    -- Include character if it overlaps with the visible range
    if charEnd > startCol && col < endCol then
      result := result.push c
    col := charEnd
    -- Early exit if we've passed the visible range
    if col >= endCol then
      break

  result

/-- State for horizontal scrolling in a paragraph. -/
structure ParagraphScrollState where
  /-- Horizontal scroll offset (in display columns). -/
  offsetX : Nat := 0
  /-- Total content width (max line width). -/
  contentWidth : Nat := 0
  /-- Viewport width for display. -/
  viewportWidth : Nat := 80
  deriving Repr, BEq, Inhabited

namespace ParagraphScrollState

/-- Scroll left by a number of columns. -/
def scrollLeft (state : ParagraphScrollState) (amount : Nat := 1) : ParagraphScrollState :=
  { state with offsetX := state.offsetX - min state.offsetX amount }

/-- Scroll right by a number of columns. -/
def scrollRight (state : ParagraphScrollState) (amount : Nat := 1) : ParagraphScrollState :=
  let maxOffset := if state.contentWidth > state.viewportWidth
                   then state.contentWidth - state.viewportWidth
                   else 0
  { state with offsetX := min maxOffset (state.offsetX + amount) }

/-- Scroll to the beginning (left edge). -/
def scrollToStart (state : ParagraphScrollState) : ParagraphScrollState :=
  { state with offsetX := 0 }

/-- Scroll to the end (right edge). -/
def scrollToEnd (state : ParagraphScrollState) : ParagraphScrollState :=
  let maxOffset := if state.contentWidth > state.viewportWidth
                   then state.contentWidth - state.viewportWidth
                   else 0
  { state with offsetX := maxOffset }

/-- Check if content is scrollable (wider than viewport). -/
def isScrollable (state : ParagraphScrollState) : Bool :=
  state.contentWidth > state.viewportWidth

/-- Check if we can scroll left. -/
def canScrollLeft (state : ParagraphScrollState) : Bool :=
  state.offsetX > 0

/-- Check if we can scroll right. -/
def canScrollRight (state : ParagraphScrollState) : Bool :=
  state.offsetX + state.viewportWidth < state.contentWidth

end ParagraphScrollState

/-- Configuration for scrollable paragraph. -/
structure ScrollableParagraphConfig where
  /-- Base style for text. -/
  style : Style := {}
  /-- Viewport width (visible area). -/
  viewportWidth : Nat := 80
  /-- Number of columns to scroll per key press. -/
  scrollStep : Nat := 4
  /-- Whether to show scroll indicators. -/
  showScrollIndicators : Bool := true
  /-- Left scroll indicator. -/
  leftIndicator : String := "◀"
  /-- Right scroll indicator. -/
  rightIndicator : String := "▶"
  /-- Style for scroll indicators. -/
  indicatorStyle : Style := { fg := .ansi .brightBlack }
  deriving Repr, Inhabited

/-- Result from scrollable paragraph widget. -/
structure ScrollableParagraphResult where
  /-- Current scroll state. -/
  scrollState : Reactive.Dynamic Spider ParagraphScrollState
  /-- Event fired when scroll position changes. -/
  onScroll : Reactive.Event Spider ParagraphScrollState

/-- Create a horizontally scrollable paragraph widget.

    When focused, responds to Left/Right/h/l keys to scroll horizontally,
    and Home/End to jump to start/end.

    Example:
    ```
    let result ← scrollableParagraph' "scroller" "This is a very long line that extends beyond the viewport..." {}
    -- The paragraph can now be scrolled with arrow keys when focused
    ```
-/
def scrollableParagraph' (name : String) (text : String) (config : ScrollableParagraphConfig := {})
    : WidgetM ScrollableParagraphResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "scrollable-paragraph" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Calculate content width (max line width)
  let lines := text.splitOn "\n"
  let contentWidth := lines.foldl (fun acc line => max acc line.displayWidth) 0

  -- Initial scroll state
  let initialState : ParagraphScrollState := {
    offsetX := 0
    contentWidth := contentWidth
    viewportWidth := config.viewportWidth
  }

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    match kd.event.code with
    | .left | .char 'h' =>
      some fun (state : ParagraphScrollState) => state.scrollLeft config.scrollStep
    | .right | .char 'l' =>
      some fun (state : ParagraphScrollState) => state.scrollRight config.scrollStep
    | .home =>
      some fun (state : ParagraphScrollState) => state.scrollToStart
    | .end =>
      some fun (state : ParagraphScrollState) => state.scrollToEnd
    | _ => none) keyEvents

  -- Fold state operations
  let stateDyn ← foldDyn (fun op state => op state) initialState stateOps

  -- Get focus state for rendering
  let focusedInput ← useFocusedInputW
  let isFocusedDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render the paragraph with horizontal scroll
  let node ← stateDyn.zipWith' (fun state _isFocused =>
    let linesArr := lines.toArray
    let lineNodes := linesArr.map fun line =>
      let visibleText := sliceByDisplayWidth line state.offsetX config.viewportWidth
      -- Pad to viewport width if needed
      let padWidth := config.viewportWidth - visibleText.displayWidth
      let paddedText := visibleText ++ String.ofList (List.replicate padWidth ' ')
      RNode.text paddedText config.style

    -- Add scroll indicators if enabled and content is scrollable
    if config.showScrollIndicators && state.isScrollable then
      let leftInd := if state.canScrollLeft
                     then RNode.text config.leftIndicator config.indicatorStyle
                     else RNode.text " " {}
      let rightInd := if state.canScrollRight
                      then RNode.text config.rightIndicator config.indicatorStyle
                      else RNode.text " " {}
      -- Wrap content with indicators
      let content := RNode.column 0 {} lineNodes
      RNode.row 0 {} #[leftInd, content, rightInd]
    else
      RNode.column 0 {} lineNodes
  ) isFocusedDyn
  emit node

  pure {
    scrollState := stateDyn
    onScroll := stateDyn.updated
  }

/-- Create a dynamic horizontally scrollable paragraph widget.

    Like `scrollableParagraph'` but accepts a dynamic text source.

    Example:
    ```
    let textDyn ← someTextSource
    let result ← dynScrollableParagraph' "scroller" textDyn {}
    ```
-/
def dynScrollableParagraph' (name : String) (text : Reactive.Dynamic Spider String)
    (config : ScrollableParagraphConfig := {}) : WidgetM ScrollableParagraphResult := do
  -- Register as focusable component
  let widgetName ← registerComponentW "scrollable-paragraph" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Initial scroll state (will be updated when text changes)
  let initialState : ParagraphScrollState := {
    offsetX := 0
    contentWidth := 0
    viewportWidth := config.viewportWidth
  }

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun (kd : KeyData) =>
    match kd.event.code with
    | .left | .char 'h' =>
      some fun (state : ParagraphScrollState) => state.scrollLeft config.scrollStep
    | .right | .char 'l' =>
      some fun (state : ParagraphScrollState) => state.scrollRight config.scrollStep
    | .home =>
      some fun (state : ParagraphScrollState) => state.scrollToStart
    | .end =>
      some fun (state : ParagraphScrollState) => state.scrollToEnd
    | _ => none) keyEvents

  -- Also update state when text changes (to recalculate content width)
  let textChangeOps ← Event.mapM (fun content =>
    let lines := content.splitOn "\n"
    let contentWidth := lines.foldl (fun acc line => max acc line.displayWidth) 0
    fun (state : ParagraphScrollState) =>
      -- Reset offset if content shrank
      let maxOffset := if contentWidth > config.viewportWidth
                       then contentWidth - config.viewportWidth
                       else 0
      { state with
        contentWidth := contentWidth
        offsetX := min state.offsetX maxOffset
      }
  ) text.updated

  -- Merge key and text change events
  let allOps ← Event.leftmostM [stateOps, textChangeOps]

  -- Fold state operations
  let stateDyn ← foldDyn (fun op state => op state) initialState allOps

  -- Get focus state for rendering
  let focusedInput ← useFocusedInputW
  let isFocusedDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Combine text, state, and focus for rendering
  let renderData ← text.zipWith' (fun content state => (content, state)) stateDyn
  let node ← renderData.zipWith' (fun (content, state) _isFocused =>
    let lines := content.splitOn "\n"
    let linesArr := lines.toArray
    let lineNodes := linesArr.map fun line =>
      let visibleText := sliceByDisplayWidth line state.offsetX config.viewportWidth
      let padWidth := config.viewportWidth - visibleText.displayWidth
      let paddedText := visibleText ++ String.ofList (List.replicate padWidth ' ')
      RNode.text paddedText config.style

    if config.showScrollIndicators && state.isScrollable then
      let leftInd := if state.canScrollLeft
                     then RNode.text config.leftIndicator config.indicatorStyle
                     else RNode.text " " {}
      let rightInd := if state.canScrollRight
                      then RNode.text config.rightIndicator config.indicatorStyle
                      else RNode.text " " {}
      let content := RNode.column 0 {} lineNodes
      RNode.row 0 {} #[leftInd, content, rightInd]
    else
      RNode.column 0 {} lineNodes
  ) isFocusedDyn
  emit node

  pure {
    scrollState := stateDyn
    onScroll := stateDyn.updated
  }

end Terminus.Reactive
