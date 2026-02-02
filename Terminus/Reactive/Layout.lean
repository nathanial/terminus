/-
  Terminus Reactive - Layout Utilities
  Split pane layouts for multi-view applications.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Containers
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

private def mkColumnNode (children : Array ComponentRender) : WidgetM (Dynamic Spider RNode) := do
  let list ← Reactive.Dynamic.sequence children.toList
  list.map' fun kids =>
    if kids.isEmpty then
      RNode.empty
    else if _ : kids.length = 1 then
      kids.head!
    else
      RNode.column 0 {} kids.toArray

/-! ## Split Direction -/

/-- Direction for split layouts. -/
inductive SplitDirection where
  /-- Horizontal split: left | right -/
  | horizontal
  /-- Vertical split: top / bottom -/
  | vertical
  deriving Repr, BEq, Inhabited

/-! ## Pane Configuration -/

/-- Configuration for pane layouts. -/
structure PaneConfig where
  /-- Minimum size in cells. -/
  minSize : Nat := 5
  /-- Gap between panes. -/
  gap : Nat := 1
  /-- Show divider line between panes. -/
  showDivider : Bool := true
  /-- Style for divider. -/
  dividerStyle : Style := { fg := .ansi .brightBlack }
  deriving Repr, Inhabited

/-! ## Split Result -/

/-- Result from a resizable split, containing divider state. -/
structure SplitResult where
  /-- Current divider position (percentage 0-100). -/
  dividerPos : Reactive.Dynamic Spider Nat
  /-- Event fired when divider position changes. -/
  onDividerChange : Reactive.Event Spider Nat

/-! ## Basic Split Layouts -/

/-- Split into two panes horizontally (left | right).

    Both panes render side by side with a gap between them.

    Example:
    ```
    horizontalSplit' 40
      (do titledBlock' "Left" .rounded theme do ...)
      (do titledBlock' "Right" .rounded theme do ...)
    ```
-/
def horizontalSplit' (_leftPercent : Nat := 50) (config : PaneConfig := {})
    (left : WidgetM α) (right : WidgetM β) : WidgetM (α × β) := do
  -- Run left pane and collect its children
  let (leftResult, leftChildren) ← runWidgetChildren left
  let leftNode ← mkColumnNode leftChildren

  -- Run right pane and collect its children
  let (rightResult, rightChildren) ← runWidgetChildren right
  let rightNode ← mkColumnNode rightChildren

  -- Emit the combined row
  let node ← leftNode.zipWith' (fun l r =>
    if config.showDivider then
      let divider := RNode.text "│" config.dividerStyle
      RNode.row config.gap {} #[l, divider, r]
    else
      RNode.row config.gap {} #[l, r]
  ) rightNode
  emit node

  pure (leftResult, rightResult)

/-- Split into two panes vertically (top / bottom).

    Both panes render stacked with a gap between them.

    Example:
    ```
    verticalSplit' 30
      (do titledBlock' "Top" .rounded theme do ...)
      (do titledBlock' "Bottom" .rounded theme do ...)
    ```
-/
def verticalSplit' (_topPercent : Nat := 50) (config : PaneConfig := {})
    (top : WidgetM α) (bottom : WidgetM β) : WidgetM (α × β) := do
  -- Run top pane
  let (topResult, topChildren) ← runWidgetChildren top
  let topNode ← mkColumnNode topChildren

  -- Run bottom pane
  let (bottomResult, bottomChildren) ← runWidgetChildren bottom
  let bottomNode ← mkColumnNode bottomChildren

  -- Emit stacked column
  let node ← topNode.zipWith' (fun t b =>
    if config.showDivider then
      let divider := RNode.text "─────────────────────────────────────────" config.dividerStyle
      RNode.column config.gap {} #[t, divider, b]
    else
      RNode.column config.gap {} #[t, b]
  ) bottomNode
  emit node

  pure (topResult, bottomResult)

/-- Generic split with direction parameter.

    Example:
    ```
    splitPane' .horizontal 50 {}
      (do text' "Pane 1" theme.bodyStyle)
      (do text' "Pane 2" theme.bodyStyle)
    ```
-/
def splitPane' (direction : SplitDirection) (percent : Nat := 50)
    (config : PaneConfig := {})
    (pane1 : WidgetM α) (pane2 : WidgetM β) : WidgetM (α × β) :=
  match direction with
  | .horizontal => horizontalSplit' percent config pane1 pane2
  | .vertical => verticalSplit' percent config pane1 pane2

/-! ## Three-Pane Layouts -/

/-- Three-pane horizontal layout: left | center | right -/
def threeColumnSplit' (_leftPercent _centerPercent : Nat := 33)
    (config : PaneConfig := {})
    (left : WidgetM α) (center : WidgetM β) (right : WidgetM γ)
    : WidgetM (α × β × γ) := do
  -- Run all panes
  let (leftResult, leftChildren) ← runWidgetChildren left
  let (centerResult, centerChildren) ← runWidgetChildren center
  let (rightResult, rightChildren) ← runWidgetChildren right
  let leftNode ← mkColumnNode leftChildren
  let centerNode ← mkColumnNode centerChildren
  let rightNode ← mkColumnNode rightChildren

  let node ← leftNode.zipWith3' (fun l c r =>
    if config.showDivider then
      let divider := RNode.text "│" config.dividerStyle
      RNode.row config.gap {} #[l, divider, c, divider, r]
    else
      RNode.row config.gap {} #[l, c, r]
  ) centerNode rightNode
  emit node

  pure (leftResult, centerResult, rightResult)

/-- Three-pane vertical layout: top / middle / bottom -/
def threeRowSplit' (_topPercent _middlePercent : Nat := 33)
    (config : PaneConfig := {})
    (top : WidgetM α) (middle : WidgetM β) (bottom : WidgetM γ)
    : WidgetM (α × β × γ) := do
  let (topResult, topChildren) ← runWidgetChildren top
  let (middleResult, middleChildren) ← runWidgetChildren middle
  let (bottomResult, bottomChildren) ← runWidgetChildren bottom
  let topNode ← mkColumnNode topChildren
  let middleNode ← mkColumnNode middleChildren
  let bottomNode ← mkColumnNode bottomChildren

  let node ← topNode.zipWith3' (fun t m b =>
    if config.showDivider then
      let divider := RNode.text "─────────────────────────────────────────" config.dividerStyle
      RNode.column config.gap {} #[t, divider, m, divider, b]
    else
      RNode.column config.gap {} #[t, m, b]
  ) middleNode bottomNode
  emit node

  pure (topResult, middleResult, bottomResult)

/-! ## Sidebar Layout -/

/-- Sidebar layout with fixed-width sidebar.

    Common pattern for navigation (sidebar) + content (main area).

    Example:
    ```
    sidebarLayout' 20 .left
      (do selectableList' menuItems 0 {})
      (do text' "Main content" theme.bodyStyle)
    ```
-/
inductive SidebarPosition where
  | left
  | right
  deriving Repr, BEq, Inhabited

def sidebarLayout' (_sidebarWidth : Nat := 20) (position : SidebarPosition := .left)
    (config : PaneConfig := {})
    (sidebar : WidgetM α) (content : WidgetM β) : WidgetM (α × β) :=
  match position with
  | .left => horizontalSplit' 25 config sidebar content  -- Approximate percentage
  | .right => do
    let (contentResult, sidebarResult) ← horizontalSplit' 75 config content sidebar
    pure (sidebarResult, contentResult)

/-! ## Header/Footer Layout -/

/-- Layout with header and main content. -/
def headerLayout' (_headerHeight : Nat := 3) (config : PaneConfig := {})
    (header : WidgetM α) (content : WidgetM β) : WidgetM (α × β) :=
  verticalSplit' 10 config header content

/-- Layout with main content and footer. -/
def footerLayout' (_footerHeight : Nat := 2) (config : PaneConfig := {})
    (content : WidgetM α) (footer : WidgetM β) : WidgetM (α × β) :=
  verticalSplit' 90 config content footer

/-- Layout with header, content, and footer. -/
def headerFooterLayout' (config : PaneConfig := {})
    (header : WidgetM α) (content : WidgetM β) (footer : WidgetM γ)
    : WidgetM (α × β × γ) :=
  threeRowSplit' 10 80 config header content footer

/-! ## Resizable Split (Advanced) -/

/-- Create a resizable split pane that can be adjusted with keyboard.

    Note: This is a simplified version. Full resize would need:
    - Mouse drag support
    - Keyboard shortcuts to adjust divider
    - Constraint handling for min/max sizes
-/
def resizableSplit' (direction : SplitDirection) (initialPercent : Nat := 50)
    (config : PaneConfig := {})
    (pane1 : WidgetM α) (pane2 : WidgetM β)
    : WidgetM (α × β × SplitResult) := do
  -- Track divider position
  let (dividerEvent, fireDivider) ← newTriggerEvent (t := Spider) (a := Nat)
  let dividerDyn ← holdDyn initialPercent dividerEvent
  let posRef ← SpiderM.liftIO (IO.mkRef initialPercent)

  -- Subscribe to keys for resize (simplified: +/- keys)
  let keyEvents ← useKeyEventW
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd =>
    match kd.event.code with
    | .char '+' | .char '=' => do
      let pos ← posRef.get
      let newPos := min 90 (pos + 5)
      posRef.set newPos
      fireDivider newPos
    | .char '-' | .char '_' => do
      let pos ← posRef.get
      let newPos := if pos > 10 then pos - 5 else 10
      posRef.set newPos
      fireDivider newPos
    | _ => pure ()

  -- Render split based on current position
  let (result1, result2) ← splitPane' direction initialPercent config pane1 pane2

  pure (result1, result2, {
    dividerPos := dividerDyn
    onDividerChange := dividerEvent
  })

end Terminus.Reactive
