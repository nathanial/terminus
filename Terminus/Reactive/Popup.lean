/-
  Terminus Reactive - Popup Widget
  Modal overlay dialog container.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Popup Configuration -/

/-- Configuration for popup appearance. -/
structure PopupConfig where
  /-- Optional title for the popup. -/
  title : Option String := none
  /-- Border type. -/
  borderType : BorderType := .double
  /-- Style for the border. -/
  borderStyle : Style := {}
  /-- Style for the title. -/
  titleStyle : Style := { modifier := { bold := true } }
  /-- Style for content area. -/
  contentStyle : Style := {}
  deriving Repr, Inhabited

/-! ## Popup Result -/

/-- Result returned by popup widget. -/
structure PopupResult where
  /-- Whether popup is currently visible. -/
  visible : Reactive.Dynamic Spider Bool
  /-- Show the popup. -/
  «show» : IO Unit
  /-- Hide the popup. -/
  hide : IO Unit
  /-- Toggle popup visibility. -/
  toggle : IO Unit

/-! ## Popup Widget -/

/-- Create a popup widget.

    The popup is a modal overlay that can contain arbitrary content.
    It is controlled via the returned PopupResult.

    Example:
    ```
    let popup ← popup' "Confirm" {} do
      text' "Are you sure?"

    -- Show popup when needed
    popup.«show»
    ```
-/
def popup' (name : String) (config : PopupConfig := {}) (content : WidgetM Unit)
    : WidgetM PopupResult := do
  let env ← SpiderM.getEnv
  -- Visibility state
  let visibleRef ← SpiderM.liftIO (IO.mkRef false)

  -- Events
  let (visibleEvent, fireVisible) ← newTriggerEvent (t := Spider) (a := Bool)
  let visibleDyn ← holdDyn false visibleEvent

  -- Control functions
  let showFn : IO Unit := do
    env.withFrame do
      visibleRef.set true
      fireVisible true

  let hideFn : IO Unit := do
    env.withFrame do
      visibleRef.set false
      fireVisible false

  let toggleFn : IO Unit := do
    env.withFrame do
      let current ← visibleRef.get
      visibleRef.set (!current)
      fireVisible (!current)

  -- Render content and wrap in block
  let (_, childRenders) ← runWidgetChildren content

  emitDynamic do
    let isVisible ← visibleRef.get
    if !isVisible then
      pure RNode.empty
    else
      -- Render children
      let nodes ← childRenders.mapM id
      let inner := RNode.column 0 {} nodes

      -- Wrap in block with optional title
      pure (RNode.block config.title config.borderType config.borderStyle inner)

  pure {
    visible := visibleDyn
    «show» := showFn
    hide := hideFn
    toggle := toggleFn
  }

/-- Create a simple message popup.

    Example:
    ```
    let popup ← messagePopup' "info-popup" "Information" "Operation completed."
    popup.«show»
    ```
-/
def messagePopup' (name : String) (title : String) (message : String)
    (config : PopupConfig := {}) : WidgetM PopupResult := do
  popup' name { config with title := some title } do
    text' message config.contentStyle

/-- Create a confirmation popup.

    Note: This is a simple version. For full interactivity, combine with
    keyboard handling in your app.

    Example:
    ```
    let popup ← confirmPopup' "confirm" "Delete File" "Are you sure?"
    popup.«show»
    -- Handle Yes/No with keyboard events
    ```
-/
def confirmPopup' (name : String) (title : String) (message : String)
    (config : PopupConfig := {}) : WidgetM PopupResult := do
  popup' name { config with title := some title } do
    column' (gap := 1) {} do
      text' message config.contentStyle
      row' (gap := 2) {} do
        text' "[ Yes ]" { fg := .ansi .green }
        text' "[ No ]" { fg := .ansi .red }

/-! ## Overlay Helpers -/

/-- Create a centered overlay container (popup without border). -/
def centeredOverlay' (name : String) (content : WidgetM Unit)
    : WidgetM PopupResult := do
  popup' name { borderType := .none } content

end Terminus.Reactive
