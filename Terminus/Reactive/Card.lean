/-
  Terminus Reactive - Card Widget
  A structured container with optional header, body, and footer sections.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Card Configuration -/

/-- Configuration for card appearance and behavior. -/
structure CardConfig where
  /-- Border style for the card. -/
  borderType : BorderType := .rounded
  /-- Style for the border. -/
  borderStyle : Style := {}
  /-- Style for the header/title text. -/
  headerStyle : Style := { modifier := { bold := true } }
  /-- Padding inside the body section (gap between children). -/
  bodyPadding : Nat := 1
  /-- Whether to show a separator line before the footer. -/
  showFooterSeparator : Bool := true
  /-- Style for the footer separator line. -/
  separatorStyle : Style := {}
  /-- Fill style for the card background. -/
  fillStyle : Option Style := none
  deriving Inhabited

/-! ## Card Helpers -/

/-- Get the horizontal line character for a border type. -/
private def horizontalChar (bt : BorderType) : Char :=
  match bt with
  | .none => '─'
  | .single => '─'
  | .double => '═'
  | .rounded => '─'
  | .thick => '━'

/-- Get the left T-junction character for a border type. -/
private def leftTeeChar (bt : BorderType) : Char :=
  match bt with
  | .none => '├'
  | .single => '├'
  | .double => '╠'
  | .rounded => '├'
  | .thick => '┣'

/-- Get the right T-junction character for a border type. -/
private def rightTeeChar (bt : BorderType) : Char :=
  match bt with
  | .none => '┤'
  | .single => '┤'
  | .double => '╣'
  | .rounded => '┤'
  | .thick => '┫'

/-- Create a separator line node for footer separation.
    The separator fits within the card's border. -/
private def mkSeparatorNode (config : CardConfig) (width : Nat := 40) : RNode :=
  let leftTee := String.singleton (leftTeeChar config.borderType)
  let rightTee := String.singleton (rightTeeChar config.borderType)
  let hChar := horizontalChar config.borderType
  let middle := String.ofList (List.replicate (width - 2) hChar)
  RNode.text s!"{leftTee}{middle}{rightTee}" config.separatorStyle

/-! ## Card Widgets -/

/-- Create a card with header, body, and optional footer.

    Visual structure:
    ```
    ╭─ Card Title ─────────╮
    │ Body content here    │
    │ More content...      │
    ├──────────────────────┤
    │ Footer actions       │
    ╰──────────────────────╯
    ```

    The card uses a block container for the outer border, and arranges
    content in a column with an optional separator before the footer.

    Example:
    ```
    card' (some "Settings") {} do
      text' "Configure your preferences" theme.bodyStyle
      text' "Option 1: Enabled" theme.bodyStyle
    do some do
      text' "[Save] [Cancel]" theme.primaryStyle
    ```
-/
def card' (title : Option String) (config : CardConfig := {})
    (body : WidgetM α) (footer : Option (WidgetM Unit) := none) : WidgetM α := do
  -- Run body and collect children
  let (result, bodyRenders) ← runWidgetChildren body
  let bodyList ← Reactive.Dynamic.sequence bodyRenders.toList

  -- Run footer if provided and collect children
  let footerRenders ← match footer with
    | some footerWidget => do
      let (_, footerChildren) ← runWidgetChildren footerWidget
      Reactive.Dynamic.sequence footerChildren.toList
    | none => Dynamic.pureM ([] : List RNode)

  -- Build the card node
  let node ← bodyList.zipWith' (fun bodyNodes footerNodes =>
    -- Body content column
    let bodyColumn := RNode.column config.bodyPadding {} bodyNodes.toArray

    -- If we have footer content, add separator and footer
    if footerNodes.isEmpty then
      -- No footer: just wrap body in block
      RNode.block title config.borderType config.borderStyle config.fillStyle bodyColumn
    else
      -- Has footer: body + separator + footer
      let footerColumn := RNode.column 0 {} footerNodes.toArray
      let innerContent :=
        if config.showFooterSeparator then
          -- Include separator line (rendered as text within the content area)
          let sepLine := RNode.text (String.ofList (List.replicate 40 (horizontalChar config.borderType)))
              config.separatorStyle
          RNode.column 0 {} #[bodyColumn, sepLine, footerColumn]
        else
          RNode.column 0 {} #[bodyColumn, footerColumn]
      RNode.block title config.borderType config.borderStyle config.fillStyle innerContent
  ) footerRenders

  emit node
  pure result

/-- Simple card with just a title and body.

    A convenience wrapper for cards without a footer section.

    Example:
    ```
    simpleCard' "Status" {} do
      text' "All systems operational" theme.successStyle
    ```
-/
def simpleCard' (title : String) (config : CardConfig := {})
    (body : WidgetM α) : WidgetM α :=
  card' (some title) config body none

/-- Card with action buttons in the footer.

    Displays an array of action labels in the footer section.
    Useful for confirmation dialogs or action panels.

    Example:
    ```
    actionCard' "Confirm Delete" #["Yes", "No", "Cancel"] {} do
      text' "Are you sure you want to delete this item?" theme.bodyStyle
    ```
-/
def actionCard' (title : String) (actions : Array String)
    (config : CardConfig := {}) (body : WidgetM α) : WidgetM α := do
  let footerWidget : WidgetM Unit := do
    let actionText := String.intercalate "  " actions.toList
    emitStatic (RNode.text actionText {})
  card' (some title) config body (some footerWidget)

/-- Card without a title (anonymous card).

    Creates a bordered card container without a header title.

    Example:
    ```
    anonymousCard' {} do
      text' "Some content in a bordered box" theme.bodyStyle
    ```
-/
def anonymousCard' (config : CardConfig := {})
    (body : WidgetM α) (footer : Option (WidgetM Unit) := none) : WidgetM α :=
  card' none config body footer

/-- Information card with icon-style prefix.

    A card variant that prepends an icon character to the title.

    Example:
    ```
    infoCard' "System Status" 'i' {} do
      text' "CPU: 45%" theme.bodyStyle
      text' "Memory: 2.1 GB" theme.bodyStyle
    ```
-/
def infoCard' (title : String) (icon : Char := 'i')
    (config : CardConfig := {}) (body : WidgetM α) : WidgetM α :=
  card' (some s!"{icon} {title}") config body none

/-- Warning card with highlighted border.

    A card styled for warnings with yellow border by default.

    Example:
    ```
    warningCard' "Disk Space Low" theme do
      text' "Only 5% disk space remaining" theme.bodyStyle
    ```
-/
def warningCard' (title : String) (theme : Theme) (body : WidgetM α) : WidgetM α :=
  let config : CardConfig := {
    borderType := .rounded
    borderStyle := { fg := theme.warning }
    headerStyle := { fg := theme.warning, modifier := { bold := true } }
    showFooterSeparator := true
  }
  card' (some s!"! {title}") config body none

/-- Error card with highlighted border.

    A card styled for errors with red border by default.

    Example:
    ```
    errorCard' "Connection Failed" theme do
      text' "Unable to reach the server" theme.bodyStyle
    ```
-/
def errorCard' (title : String) (theme : Theme) (body : WidgetM α) : WidgetM α :=
  let config : CardConfig := {
    borderType := .rounded
    borderStyle := { fg := theme.error }
    headerStyle := { fg := theme.error, modifier := { bold := true } }
    showFooterSeparator := true
  }
  card' (some s!"x {title}") config body none

/-- Success card with highlighted border.

    A card styled for success messages with green border by default.

    Example:
    ```
    successCard' "Operation Complete" theme do
      text' "All files have been saved" theme.bodyStyle
    ```
-/
def successCard' (title : String) (theme : Theme) (body : WidgetM α) : WidgetM α :=
  let config : CardConfig := {
    borderType := .rounded
    borderStyle := { fg := theme.success }
    headerStyle := { fg := theme.success, modifier := { bold := true } }
    showFooterSeparator := true
  }
  card' (some s!"+ {title}") config body none

end Terminus.Reactive
