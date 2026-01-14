/-
  Terminus Reactive - Badge Widget
  Small status indicators for counts, status dots, and labels.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Badge Variant

Badges can display counts, status dots, or text labels.
-/

/-- The type of badge to display. -/
inductive BadgeVariant where
  /-- Count badge showing a number, with optional max (shows "99+" if over max). -/
  | count (n : Nat) (max : Nat := 99)
  /-- Status dot (a single colored character). -/
  | dot
  /-- Text label badge (e.g., "NEW", "BETA"). -/
  | label (text : String)
  deriving Repr

/-! ## Badge Configuration -/

/-- Configuration for badge appearance. -/
structure BadgeConfig where
  /-- Style for the badge (foreground and background colors). -/
  style : Style := { fg := .ansi .white, bg := .ansi .red }
  /-- Character used for dot badges. -/
  dotChar : String := "●"
  deriving Inhabited

/-! ## Badge Rendering Helpers -/

/-- Render a badge variant to a string. -/
def BadgeVariant.render (variant : BadgeVariant) (config : BadgeConfig := {}) : String :=
  match variant with
  | .count n max =>
    if n > max then
      s!"[{max}+]"
    else
      s!"[{n}]"
  | .dot => config.dotChar
  | .label text => s!"[{text}]"

/-! ## Badge Widgets -/

/-- Render a standalone badge.

    Examples:
    - `badge' (.count 5)` renders `[5]`
    - `badge' (.count 150)` renders `[99+]`
    - `badge' .dot` renders `●`
    - `badge' (.label "NEW")` renders `[NEW]`
-/
def badge' (variant : BadgeVariant) (config : BadgeConfig := {}) : WidgetM Unit := do
  let content := variant.render config
  emitStatic (RNode.text content config.style)

/-- Render a badge from a Dynamic count.
    The badge updates automatically when the count changes.

    Example:
    ```
    let unreadCount ← someDataSource
    countBadge' unreadCount
    ```
-/
def countBadge' (count : Dynamic Spider Nat) (config : BadgeConfig := {}) : WidgetM Unit := do
  let node ← count.map' fun n =>
    let content := if n > 99 then "[99+]" else s!"[{n}]"
    RNode.text content config.style
  emit node

/-- Render a badge from a Dynamic count with custom max value.
    The badge updates automatically when the count changes.
-/
def countBadgeMax' (count : Dynamic Spider Nat) (max : Nat) (config : BadgeConfig := {})
    : WidgetM Unit := do
  let node ← count.map' fun n =>
    let content := if n > max then s!"[{max}+]" else s!"[{n}]"
    RNode.text content config.style
  emit node

/-- Render a status dot badge with a specific color.
    Status dots are useful for indicating online/offline status, health, etc.

    Example:
    ```
    statusDot' (.ansi .green)  -- Online indicator
    statusDot' (.ansi .red)    -- Offline indicator
    ```
-/
def statusDot' (color : Color) : WidgetM Unit := do
  emitStatic (RNode.text "●" { fg := color })

/-- Render a dynamic status dot that changes color based on a Dynamic value.

    Example:
    ```
    let isOnline ← someStatusSource
    let color ← isOnline.map' fun online =>
      if online then Color.ansi .green else Color.ansi .red
    dynStatusDot' color
    ```
-/
def dynStatusDot' (color : Dynamic Spider Color) : WidgetM Unit := do
  let node ← color.map' fun c => RNode.text "●" { fg := c }
  emit node

/-- Wrap content with a badge to the right.
    The badge appears immediately after the content on the same line.

    Example:
    ```
    withBadge' (.count 3) {} do
      text' "Notifications" {}
    -- Renders: "Notifications [3]"
    ```
-/
def withBadge' (variant : BadgeVariant) (config : BadgeConfig := {})
    (content : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren content
  let badgeContent := variant.render config
  let badgeNode := RNode.text badgeContent config.style
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let combined ← childrenList.map' fun kids =>
    -- Combine content nodes with badge in a row
    let contentArray := kids.toArray
    let allNodes := contentArray.push badgeNode
    RNode.row 1 {} allNodes
  emit combined
  pure result

/-- Wrap content with a dynamic count badge to the right.
    The badge updates automatically when the count changes.
-/
def withCountBadge' (count : Dynamic Spider Nat) (config : BadgeConfig := {})
    (content : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren content
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let combined ← count.zipWith' (fun n kids =>
    let badgeContent := if n > 99 then "[99+]" else s!"[{n}]"
    let badgeNode := RNode.text badgeContent config.style
    let contentArray := kids.toArray
    let allNodes := contentArray.push badgeNode
    RNode.row 1 {} allNodes
  ) childrenList
  emit combined
  pure result

/-- Wrap content with a status dot to the left.
    Useful for showing status before a label.

    Example:
    ```
    withStatusDot' (.ansi .green) do
      text' "Server running" {}
    -- Renders: "● Server running"
    ```
-/
def withStatusDot' (color : Color) (content : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren content
  let dotNode := RNode.text "● " { fg := color }
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let combined ← childrenList.map' fun kids =>
    let contentArray := kids.toArray
    let allNodes := #[dotNode] ++ contentArray
    RNode.row 0 {} allNodes
  emit combined
  pure result

/-- Wrap content with a dynamic status dot to the left.
    The dot color updates automatically when the color Dynamic changes.
-/
def withDynStatusDot' (color : Dynamic Spider Color) (content : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren content
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let combined ← color.zipWith' (fun c kids =>
    let dotNode := RNode.text "● " { fg := c }
    let contentArray := kids.toArray
    let allNodes := #[dotNode] ++ contentArray
    RNode.row 0 {} allNodes
  ) childrenList
  emit combined
  pure result

end Terminus.Reactive
