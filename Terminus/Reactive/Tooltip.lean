/-
  Terminus Reactive - Tooltip Widget
  Shows hint text when an element is focused.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Tooltip Position -/

/-- Position of the tooltip relative to the content. -/
inductive TooltipPosition where
  | above
  | below
  | left
  | right
  deriving Repr, BEq, Inhabited

/-! ## Tooltip Configuration -/

/-- Configuration for tooltip appearance and behavior. -/
structure TooltipConfig where
  /-- Position of the tooltip relative to content. -/
  position : TooltipPosition := .below
  /-- Style for the tooltip text. -/
  style : Style := { fg := .ansi .brightBlack }
  /-- Reserved for future animation support. -/
  showDelay : Nat := 0
  /-- Prefix shown before the tooltip text. -/
  hintPrefix : String := "? "
  deriving Inhabited

/-! ## Tooltip Widget -/

/-- Wrap content with a tooltip that shows when focused.

    The tooltip appears adjacent to the content based on the configured position.
    It only displays when the component is focused.

    Example:
    ```
    tooltip' "username" "Enter your email or username" {} do
      textInput' "username" { placeholder := "Username" }
    ```
-/
def tooltip' (name : String) (tip : String) (config : TooltipConfig := {})
    (content : WidgetM α) : WidgetM α := do
  -- Register as focusable component for focus tracking
  let widgetName ← registerComponentW "tooltip" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Get focus state
  let focusedInput ← useFocusedInputW
  let isFocusedDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Run content and capture its render
  let (result, childRenders) ← runWidgetChildren content
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList

  -- Build the tooltip text node
  let tooltipText := config.hintPrefix ++ tip
  let tooltipNode := RNode.text tooltipText config.style

  -- Combine focus state with children to produce final render
  let node ← isFocusedDyn.zipWith' (fun isFocused kids =>
    let contentNode := if kids.isEmpty then
        RNode.empty
      else if _ : kids.length = 1 then
        kids.head!
      else
        RNode.column 0 {} kids.toArray

    if isFocused then
      -- Position tooltip based on config
      match config.position with
      | .below =>
        RNode.column 0 {} #[contentNode, tooltipNode]
      | .above =>
        RNode.column 0 {} #[tooltipNode, contentNode]
      | .right =>
        RNode.row 0 {} #[contentNode, RNode.text " " {}, tooltipNode]
      | .left =>
        RNode.row 0 {} #[tooltipNode, RNode.text " " {}, contentNode]
    else
      contentNode
  ) childrenList
  emit node

  pure result

/-- Simple text with tooltip.

    A convenience function for displaying styled text with a tooltip hint.
    The tooltip appears when the text element is focused.

    Example:
    ```
    textWithTooltip' "Click here" "Opens the settings dialog" theme.primaryStyle {}
    ```
-/
def textWithTooltip' (text : String) (tip : String) (style : Style := {})
    (config : TooltipConfig := {}) : WidgetM Unit := do
  tooltip' "" tip config do
    text' text style

end Terminus.Reactive
