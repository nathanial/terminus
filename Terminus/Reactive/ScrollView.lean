/-
  Terminus Reactive - ScrollView Widget
  Scrollable container with keyboard/mouse navigation.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## ScrollView Configuration -/

/-- Configuration for reactive ScrollView widget. -/
structure ScrollViewConfig where
  /-- Enable vertical scrolling. -/
  verticalScroll : Bool := true
  /-- Enable horizontal scrolling. -/
  horizontalScroll : Bool := false
  /-- Show vertical scrollbar when content overflows. -/
  showVerticalScrollbar : Bool := true
  /-- Show horizontal scrollbar when content overflows. -/
  showHorizontalScrollbar : Bool := false
  /-- Maximum visible rows (viewport height). Required for scrolling to work. -/
  maxVisible : Nat := 5
  /-- Scroll amount per arrow key press. -/
  scrollStep : Nat := 1
  /-- Scroll amount per page up/down (none = viewport height - 1). -/
  pageStep : Option Nat := none
  /-- Focus name for keyboard routing. Empty = auto-generated. -/
  focusName : String := ""
  /-- Whether to capture keys globally (without focus). -/
  globalKeys : Bool := false
  /-- Character for scrollbar track. -/
  trackChar : Char := '░'
  /-- Character for scrollbar thumb. -/
  thumbChar : Char := '█'
  /-- Style for scrollbar track. -/
  trackStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for scrollbar thumb. -/
  thumbStyle : Style := {}
  deriving Repr, Inhabited

/-! ## ScrollView State -/

/-- Internal state for scroll position. -/
structure ScrollState where
  /-- Vertical scroll offset (in cells from top). -/
  offsetY : Nat := 0
  /-- Horizontal scroll offset (in cells from left). -/
  offsetX : Nat := 0
  /-- Total content height (set after measuring). -/
  contentHeight : Nat := 0
  /-- Total content width (set after measuring). -/
  contentWidth : Nat := 0
  /-- Viewport height. -/
  viewportHeight : Nat := 0
  /-- Viewport width. -/
  viewportWidth : Nat := 0
  deriving Repr, Inhabited, BEq

namespace ScrollState

/-- Maximum vertical scroll offset. -/
def maxOffsetY (s : ScrollState) : Nat :=
  if s.contentHeight > s.viewportHeight then
    s.contentHeight - s.viewportHeight
  else 0

/-- Maximum horizontal scroll offset. -/
def maxOffsetX (s : ScrollState) : Nat :=
  if s.contentWidth > s.viewportWidth then
    s.contentWidth - s.viewportWidth
  else 0

/-- Clamp offsets to valid range. -/
def clamp (s : ScrollState) : ScrollState :=
  { s with
    offsetY := min s.offsetY s.maxOffsetY
    offsetX := min s.offsetX s.maxOffsetX }

/-- Scroll down by n cells. -/
def scrollDown (s : ScrollState) (n : Nat := 1) : ScrollState :=
  { s with offsetY := min (s.offsetY + n) s.maxOffsetY }

/-- Scroll up by n cells. -/
def scrollUp (s : ScrollState) (n : Nat := 1) : ScrollState :=
  { s with offsetY := s.offsetY - min n s.offsetY }

/-- Scroll right by n cells. -/
def scrollRight (s : ScrollState) (n : Nat := 1) : ScrollState :=
  { s with offsetX := min (s.offsetX + n) s.maxOffsetX }

/-- Scroll left by n cells. -/
def scrollLeft (s : ScrollState) (n : Nat := 1) : ScrollState :=
  { s with offsetX := s.offsetX - min n s.offsetX }

/-- Page down (scroll by viewport height). -/
def pageDown (s : ScrollState) (pageSize : Option Nat := none) : ScrollState :=
  let step := pageSize.getD (if s.viewportHeight > 1 then s.viewportHeight - 1 else 1)
  s.scrollDown (max step 1)

/-- Page up (scroll by viewport height). -/
def pageUp (s : ScrollState) (pageSize : Option Nat := none) : ScrollState :=
  let step := pageSize.getD (if s.viewportHeight > 1 then s.viewportHeight - 1 else 1)
  s.scrollUp (max step 1)

/-- Scroll to top. -/
def scrollToTop (s : ScrollState) : ScrollState :=
  { s with offsetY := 0 }

/-- Scroll to bottom. -/
def scrollToBottom (s : ScrollState) : ScrollState :=
  { s with offsetY := s.maxOffsetY }

/-- Scroll to make a specific y position visible. -/
def scrollToVisible (s : ScrollState) (y : Nat) : ScrollState :=
  if y < s.offsetY then
    { s with offsetY := y }
  else if s.viewportHeight > 0 && y >= s.offsetY + s.viewportHeight then
    { s with offsetY := y - s.viewportHeight + 1 }
  else s

/-- Check if vertical scrolling is needed. -/
def needsVerticalScroll (s : ScrollState) : Bool :=
  s.contentHeight > s.viewportHeight

/-- Check if horizontal scrolling is needed. -/
def needsHorizontalScroll (s : ScrollState) : Bool :=
  s.contentWidth > s.viewportWidth

end ScrollState

/-! ## ScrollView Result -/

/-- Result returned by scrollView' containing reactive values and controls. -/
structure ScrollViewResult (α : Type) where
  /-- Result from the content builder. -/
  content : α
  /-- Current scroll state as a Dynamic. -/
  scrollState : Reactive.Dynamic Spider ScrollState
  /-- Event fired when scroll position changes. -/
  onScroll : Reactive.Event Spider ScrollState
  /-- Programmatic scroll to absolute position. -/
  scrollTo : Nat → Nat → IO Unit
  /-- Scroll to make a y position visible. -/
  scrollToY : Nat → IO Unit

/-! ## Scrollbar Rendering -/

/-- Compute scrollbar thumb size and position.
    Returns (thumbSize, thumbPosition). -/
def computeThumbMetrics (contentSize viewportSize offset trackLen : Nat) : (Nat × Nat) :=
  if contentSize <= viewportSize || trackLen == 0 then
    (trackLen, 0)
  else
    -- Thumb size proportional to viewport/content ratio
    let thumbSize := max 1 (trackLen * viewportSize / contentSize)
    -- Thumb position proportional to scroll offset
    let maxOffset := contentSize - viewportSize
    let scrollableTrack := trackLen - thumbSize
    let thumbPos := if maxOffset > 0 then scrollableTrack * offset / maxOffset else 0
    (thumbSize, thumbPos)

/-- Render a vertical scrollbar as an RNode. -/
def renderVerticalScrollbar (state : ScrollState) (config : ScrollViewConfig)
    (height : Nat) : RNode :=
  if height == 0 then .empty
  else Id.run do
    let (thumbSize, thumbPos) := computeThumbMetrics
      state.contentHeight state.viewportHeight state.offsetY height

    -- Build array of characters for the scrollbar
    let mut chars : Array RNode := #[]
    for i in [:height] do
      let inThumb := i >= thumbPos && i < thumbPos + thumbSize
      let char := if inThumb then config.thumbChar else config.trackChar
      let style := if inThumb then config.thumbStyle else config.trackStyle
      chars := chars.push (.text char.toString style)

    .column 0 {} chars

/-! ## ScrollView Widget -/

/-- Create a reactive scrollable container.

    The content is built using WidgetM, allowing any nested widgets.
    Scroll state is managed via FRP - changes propagate automatically.

    Keyboard bindings (when focused):
    - Up/Down, j/k: Scroll by scrollStep
    - Page Up/Down: Scroll by viewport height
    - Home/End: Jump to top/bottom

    Mouse bindings:
    - Scroll wheel: Scroll by scrollStep

    Example:
    ```
    let scroll ← scrollView' { showVerticalScrollbar := true } do
      for line in lines do
        text' line theme.bodyStyle
    -- Arrow keys and mouse wheel work automatically
    ```
-/
def scrollView' (config : ScrollViewConfig := {})
    (content : WidgetM α) : WidgetM (ScrollViewResult α) := do
  let events ← getEventsW

  -- Register for focus
  let widgetName ← registerComponentW "scrollView" (isInput := true)
    (nameOverride := config.focusName)
  let scrollName := if config.focusName.isEmpty then widgetName else config.focusName

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW scrollName config.globalKeys

  -- Create scroll state
  let initialState : ScrollState := {}
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)

  -- Events for scroll changes
  let (scrollEvent, fireScroll) ← newTriggerEvent (t := Spider) (a := ScrollState)
  let scrollDyn ← holdDyn initialState scrollEvent

  -- Programmatic scroll control
  let scrollTo := fun x y => do
    let state ← stateRef.get
    let newState := { state with offsetX := x, offsetY := y }.clamp
    if newState != state then
      stateRef.set newState
      fireScroll newState

  let scrollToY := fun y => do
    let state ← stateRef.get
    let newState := state.scrollToVisible y
    if newState != state then
      stateRef.set newState
      fireScroll newState

  -- Run content builder to get child widgets
  let (result, childRenders) ← runWidgetChildren content

  -- Set content height based on number of children, viewport from config
  let contentHeight := childRenders.size
  let viewportHeight := config.maxVisible
  SpiderM.liftIO <| stateRef.set { initialState with
    contentHeight := contentHeight
    viewportHeight := viewportHeight
  }

  -- Subscribe to key events for scrolling
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
      let state ← stateRef.get
      let ke := kd.event

      let newState ← match ke.code with
        | .up => pure (state.scrollUp config.scrollStep)
        | .down => pure (state.scrollDown config.scrollStep)
        | .left => pure (state.scrollLeft config.scrollStep)
        | .right => pure (state.scrollRight config.scrollStep)
        | .pageUp => pure (state.pageUp config.pageStep)
        | .pageDown => pure (state.pageDown config.pageStep)
        | .home => pure state.scrollToTop
        | .end => pure state.scrollToBottom
        | .char 'k' => pure (state.scrollUp config.scrollStep)
        | .char 'j' => pure (state.scrollDown config.scrollStep)
        | .char 'h' => pure (state.scrollLeft config.scrollStep)
        | .char 'l' => pure (state.scrollRight config.scrollStep)
        | _ => pure state

      if newState != state then
        stateRef.set newState
        fireScroll newState

  -- Subscribe to mouse scroll events
  let _mouseUnsub ← SpiderM.liftIO <| events.mouseEvent.subscribe fun md => do
    let me := md.event
    let state ← stateRef.get
    let newState := match me.button with
      | .scrollUp => state.scrollUp config.scrollStep
      | .scrollDown => state.scrollDown config.scrollStep
      | _ => state

    if newState != state then
      stateRef.set newState
      fireScroll newState

  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let node ← scrollDyn.zipWith' (fun state nodes =>
    let arr := nodes.toArray
    let startIdx := state.offsetY
    let endIdx := min (startIdx + config.maxVisible) arr.size
    let visible := arr.toSubarray startIdx endIdx |>.toArray
    let contentNode := RNode.column 0 {} visible
    if config.showVerticalScrollbar && state.needsVerticalScroll then
      let scrollbar := renderVerticalScrollbar state config config.maxVisible
      RNode.row 0 {} #[contentNode, scrollbar]
    else
      contentNode
  ) childrenList
  emit node

  pure {
    content := result
    scrollState := scrollDyn
    onScroll := scrollEvent
    scrollTo := scrollTo
    scrollToY := scrollToY
  }

/-- Create a vertical-only scrollview (convenience wrapper). -/
def vscrollView' (config : ScrollViewConfig := {})
    (content : WidgetM α) : WidgetM (ScrollViewResult α) :=
  scrollView' { config with verticalScroll := true, horizontalScroll := false } content

/-- Create a horizontal-only scrollview (convenience wrapper). -/
def hscrollView' (config : ScrollViewConfig := {})
    (content : WidgetM α) : WidgetM (ScrollViewResult α) :=
  scrollView' { config with verticalScroll := false, horizontalScroll := true } content

end Terminus.Reactive
