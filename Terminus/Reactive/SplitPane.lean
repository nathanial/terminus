/-
  Terminus Reactive - SplitPane Widget
  A resizable split pane widget for creating adjustable two-panel layouts.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Split Orientation -/

/-- Orientation for split pane layout. -/
inductive SplitOrientation where
  /-- Horizontal split: left | right -/
  | horizontal
  /-- Vertical split: top / bottom -/
  | vertical
  deriving Repr, BEq, Inhabited

/-! ## SplitPane Configuration -/

/-- Configuration for SplitPane appearance and behavior. -/
structure SplitPaneConfig where
  /-- Split orientation: horizontal (left|right) or vertical (top/bottom). -/
  orientation : SplitOrientation := .horizontal
  /-- Initial ratio for divider position (0.0-1.0). -/
  initialRatio : Float := 0.5
  /-- Minimum ratio for first pane (prevents collapse). -/
  minRatio : Float := 0.1
  /-- Maximum ratio for first pane (prevents second pane collapse). -/
  maxRatio : Float := 0.9
  /-- Character used to render the divider.
      Default is '|' for horizontal, '-' for vertical. -/
  dividerChar : Option Char := none
  /-- Style for the divider line. -/
  dividerStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for divider when focused (for resize mode). -/
  focusedDividerStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Allow keyboard resizing when focused. -/
  resizable : Bool := true
  /-- Step size for normal arrow key adjustments. -/
  step : Float := 0.02
  /-- Step size for Shift+arrow adjustments (larger). -/
  largeStep : Float := 0.1
  /-- Gap between panes (in addition to divider). -/
  gap : Nat := 0
  deriving Inhabited

namespace SplitPaneConfig

/-- Get the divider character based on orientation. -/
def getDividerChar (config : SplitPaneConfig) : Char :=
  match config.dividerChar with
  | some c => c
  | none =>
    match config.orientation with
    | .horizontal => '|'
    | .vertical => '-'

end SplitPaneConfig

/-! ## SplitPane Result -/

/-- Result from SplitPane widget containing reactive state. -/
structure SplitPaneResult where
  /-- Current split ratio (0.0-1.0) as a Dynamic. -/
  ratio : Reactive.Dynamic Spider Float
  /-- Event fired when ratio changes. -/
  onChange : Reactive.Event Spider Float

/-! ## SplitPane Widget -/

/-- Internal helper to combine children into a column node. -/
private def mkColumnNodeSP (children : Array ComponentRender) : WidgetM (Dynamic Spider RNode) := do
  let list ← Reactive.Dynamic.sequence children.toList
  list.map' fun kids =>
    if kids.isEmpty then
      RNode.empty
    else if _h : kids.length = 1 then
      kids.head!
    else
      RNode.column 0 {} kids.toArray

/-- Internal helper to combine children into a row node. -/
private def mkRowNodeSP (children : Array ComponentRender) : WidgetM (Dynamic Spider RNode) := do
  let list ← Reactive.Dynamic.sequence children.toList
  list.map' fun kids =>
    if kids.isEmpty then
      RNode.empty
    else if _h : kids.length = 1 then
      kids.head!
    else
      RNode.row 0 {} kids.toArray

/-- Create a resizable split pane widget.

    SplitPane divides its container into two resizable regions separated by
    a divider. Users can resize the panes using keyboard controls when the
    split pane is focused.

    **Keyboard Controls (when focused and resizable):**
    - For horizontal orientation:
      - Left/Right arrows: Adjust divider position
      - Shift+Left/Right: Larger adjustments
    - For vertical orientation:
      - Up/Down arrows: Adjust divider position
      - Shift+Up/Down: Larger adjustments
    - Home: Move divider to minimum position
    - End: Move divider to maximum position

    **Example:**
    ```lean
    let split ← resizableSplitPane' "mainSplit" { orientation := .horizontal, initialRatio := 0.3 }
      (do
        titledBlock' "Sidebar" .rounded theme do
          text' "Navigation" theme.bodyStyle)
      (do
        titledBlock' "Content" .rounded theme do
          text' "Main content area" theme.bodyStyle)

    -- React to ratio changes
    performEvent_ (← Event.mapM (fun r => IO.println s!"Ratio: {r}") split.onChange)
    ```
-/
def resizableSplitPane' (name : String) (config : SplitPaneConfig := {})
    (first : WidgetM α) (second : WidgetM β) : WidgetM (SplitPaneResult × α × β) := do
  -- Register as focusable if resizable
  let widgetName ← registerComponentW "splitPane" (isInput := config.resizable) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Clamp initial ratio
  let clampedInitial := max config.minRatio (min config.maxRatio config.initialRatio)

  -- Build ratio state using declarative FRP
  let ratioDyn ← if config.resizable then
    let keyEvents ← useFocusedKeyEventsW inputName

    -- Map key events to ratio transformation functions
    let ratioOps ← Event.mapMaybeM (fun kd => do
      let ke := kd.event
      let step := if ke.modifiers.shift then config.largeStep else config.step
      match config.orientation with
      | .horizontal =>
        match ke.code with
        | .left | .char 'h' => some fun r => max config.minRatio (r - step)
        | .right | .char 'l' => some fun r => min config.maxRatio (r + step)
        | .home => some fun _ => config.minRatio
        | .end => some fun _ => config.maxRatio
        | _ => none
      | .vertical =>
        match ke.code with
        | .up | .char 'k' => some fun r => max config.minRatio (r - step)
        | .down | .char 'j' => some fun r => min config.maxRatio (r + step)
        | .home => some fun _ => config.minRatio
        | .end => some fun _ => config.maxRatio
        | _ => none) keyEvents

    -- Fold ratio operations into state
    foldDyn (fun op r => op r) clampedInitial ratioOps
  else
    -- Not resizable, just a constant
    Dynamic.pureM clampedInitial

  -- Run first pane and collect its children
  let (firstResult, firstChildren) ← runWidgetChildren first
  let firstNode ← mkColumnNodeSP firstChildren

  -- Run second pane and collect its children
  let (secondResult, secondChildren) ← runWidgetChildren second
  let secondNode ← mkColumnNodeSP secondChildren

  -- Get focus state for styling
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Build the combined render node
  -- Combine focus state into the render since we only have zipWith3'
  let node ← firstNode.zipWith3' (fun fNode sNode isFocused =>
    let dividerChar := config.getDividerChar
    let dividerStyle := if isFocused && config.resizable
                        then config.focusedDividerStyle
                        else config.dividerStyle

    match config.orientation with
    | .horizontal =>
      -- Horizontal: first | divider | second
      let divider := RNode.text (String.singleton dividerChar) dividerStyle
      RNode.row config.gap {} #[fNode, divider, sNode]
    | .vertical =>
      -- Vertical: first / divider / second
      -- Use a line of divider characters
      let dividerLine := String.ofList (List.replicate 40 dividerChar)
      let divider := RNode.text dividerLine dividerStyle
      RNode.column config.gap {} #[fNode, divider, sNode]
  ) secondNode focusDyn

  emit node

  pure ({
    ratio := ratioDyn
    onChange := ratioDyn.updated
  }, firstResult, secondResult)

/-- Convenience wrapper for splitPane' when you only need the result.

    **Example:**
    ```lean
    let result ← splitPaneSimple' "split" {}
      (text' "Left" {})
      (text' "Right" {})
    ```
-/
def resizableSplitPaneSimple' (name : String) (config : SplitPaneConfig := {})
    (first : WidgetM Unit) (second : WidgetM Unit) : WidgetM SplitPaneResult := do
  let (result, _, _) ← resizableSplitPane' name config first second
  pure result

/-- Create a horizontal split pane (left | right).

    Convenience function that sets orientation to horizontal.

    **Example:**
    ```lean
    let split ← horizontalSplitPane' "hSplit" 0.3
      (text' "Left pane" {})
      (text' "Right pane" {})
    ```
-/
def resizableHorizontalSplit' (name : String) (initialRatio : Float := 0.5)
    (config : SplitPaneConfig := {})
    (left : WidgetM α) (right : WidgetM β) : WidgetM (SplitPaneResult × α × β) :=
  resizableSplitPane' name { config with orientation := .horizontal, initialRatio } left right

/-- Create a vertical split pane (top / bottom).

    Convenience function that sets orientation to vertical.

    **Example:**
    ```lean
    let split ← verticalSplitPane' "vSplit" 0.3
      (text' "Top pane" {})
      (text' "Bottom pane" {})
    ```
-/
def resizableVerticalSplit' (name : String) (initialRatio : Float := 0.5)
    (config : SplitPaneConfig := {})
    (top : WidgetM α) (bottom : WidgetM β) : WidgetM (SplitPaneResult × α × β) :=
  resizableSplitPane' name { config with orientation := .vertical, initialRatio } top bottom

/-! ## Nested Split Panes -/

/-- Create a three-pane layout with two horizontal splits.

    Creates: [first | second | third]

    **Example:**
    ```lean
    let (r1, r2, (), (), ()) ← threeHorizontalPanes' "triple" 0.25 0.5
      (text' "Left" {})
      (text' "Center" {})
      (text' "Right" {})
    ```
-/
def threeHorizontalPanes' (name : String) (firstRatio : Float := 0.33)
    (secondRatio : Float := 0.5) (config : SplitPaneConfig := {})
    (first : WidgetM α) (second : WidgetM β) (third : WidgetM γ)
    : WidgetM (SplitPaneResult × SplitPaneResult × α × β × γ) := do
  let (result1, firstRes, (result2, secondRes, thirdRes)) ←
    resizableSplitPane' s!"{name}-outer" { config with orientation := .horizontal, initialRatio := firstRatio }
      first
      (resizableSplitPane' s!"{name}-inner" { config with orientation := .horizontal, initialRatio := secondRatio }
        second third)
  pure (result1, result2, firstRes, secondRes, thirdRes)

/-- Create a three-pane layout with two vertical splits.

    Creates:
    ```
    [first]
    -------
    [second]
    -------
    [third]
    ```

    **Example:**
    ```lean
    let (r1, r2, (), (), ()) ← threeVerticalPanes' "triple" 0.2 0.5
      (text' "Header" {})
      (text' "Content" {})
      (text' "Footer" {})
    ```
-/
def threeVerticalPanes' (name : String) (firstRatio : Float := 0.33)
    (secondRatio : Float := 0.5) (config : SplitPaneConfig := {})
    (first : WidgetM α) (second : WidgetM β) (third : WidgetM γ)
    : WidgetM (SplitPaneResult × SplitPaneResult × α × β × γ) := do
  let (result1, firstRes, (result2, secondRes, thirdRes)) ←
    resizableSplitPane' s!"{name}-outer" { config with orientation := .vertical, initialRatio := firstRatio }
      first
      (resizableSplitPane' s!"{name}-inner" { config with orientation := .vertical, initialRatio := secondRatio }
        second third)
  pure (result1, result2, firstRes, secondRes, thirdRes)

end Terminus.Reactive
