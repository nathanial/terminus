/-
  Terminus Reactive - Overlay and Modal Components
  Layered UI components for dialogs, popups, and overlays.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Overlay Types -/

/-- Configuration for overlay positioning and appearance. -/
structure OverlayConfig where
  /-- Center the overlay horizontally. -/
  centerHorizontal : Bool := true
  /-- Center the overlay vertically. -/
  centerVertical : Bool := true
  /-- Optional backdrop style (dims the background). -/
  backdropStyle : Option Style := some { fg := .ansi .brightBlack }
  /-- Backdrop character (space for solid, special char for pattern). -/
  backdropChar : Char := ' '
  /-- Border type. -/
  border : BorderType := .rounded
  /-- Border style. -/
  borderStyle : Style := { fg := .ansi .white }
  deriving Repr, Inhabited

/-- Configuration for modal dialogs. -/
structure ModalConfig where
  /-- Border type for the modal. -/
  borderType : BorderType := .rounded
  /-- Padding inside the modal border. -/
  padding : Nat := 1
  /-- Minimum width of the modal. -/
  minWidth : Option Nat := some 30
  /-- Center horizontally. -/
  centerHorizontal : Bool := true
  /-- Center vertically. -/
  centerVertical : Bool := true
  deriving Repr, Inhabited

/-! ## Overlay Container

The overlay container renders content on top of the base UI when visible.
Unlike traditional overlays that modify a buffer in place, reactive overlays
work by emitting a special structure that the render pipeline understands.

For now, we implement overlays using conditional rendering - when visible,
the overlay content replaces the base content in the widget tree.
-/

/-- Create an overlay that shows content when visible.

    When `visible` is true, both the base content and overlay content are rendered,
    with the overlay appearing centered on top with an optional backdrop.

    Example:
    ```
    let showDialog ← useToggle escapeEvent false

    overlay' showDialog.current {} do
      -- Base content (always visible)
      text' "Main content" theme.bodyStyle
    do
      -- Overlay content (only when showDialog is true)
      modal' "Confirm" theme do
        text' "Are you sure?" theme.bodyStyle
    ```
-/
def overlay' (visible : Reactive.Dynamic Spider Bool) (config : OverlayConfig := {})
    (baseContent : WidgetM α) (overlayContent : WidgetM β) : WidgetM α := do
  let (baseResult, baseRenders) ← runWidgetChildren baseContent
  let (_, overlayRenders) ← runWidgetChildren overlayContent

  let mkNode (renders : Array ComponentRender) : WidgetM (Dynamic Spider RNode) := do
    let list ← Reactive.Dynamic.sequence renders.toList
    list.map' fun nodes =>
      if nodes.isEmpty then
        RNode.empty
      else if nodes.length = 1 then
        nodes.head!
      else
        RNode.column 0 {} nodes.toArray

  let baseNode ← mkNode baseRenders
  let overlayNode ← mkNode overlayRenders
  let node ← visible.zipWith3' (fun isVisible base overlay =>
    if isVisible then
      let wrapped :=
        if config.border != .none then
          let title := none
          let fillStyle := none
          RNode.block title config.border config.borderStyle fillStyle overlay
        else
          overlay
      RNode.overlay base wrapped config.backdropStyle
    else
      base
  ) baseNode overlayNode
  emit node

  pure baseResult

/-- Simpler overlay that only shows when visible (no base content parameter).
    Use this when you want to conditionally show a popup/dialog.
    The content will be centered on screen with an optional backdrop. -/
def overlayWhen' (visible : Reactive.Dynamic Spider Bool) (config : OverlayConfig := {})
    (content : WidgetM α) : WidgetM Unit := do
  let (_, contentRenders) ← runWidgetChildren content

  let list ← Reactive.Dynamic.sequence contentRenders.toList
  let contentNode ← list.map' fun nodes =>
    if nodes.isEmpty then
      RNode.empty
    else if nodes.length = 1 then
      nodes.head!
    else
      RNode.column 0 {} nodes.toArray

  let node ← visible.zipWith' (fun isVisible contentNode =>
    if isVisible then
      RNode.overlay RNode.empty contentNode config.backdropStyle
    else
      RNode.empty
  ) contentNode
  emit node

/-! ## Modal Dialog

Modals are overlays with borders and titles, typically used for dialogs.
-/

/-- Create a modal dialog box with opaque background.

    Example:
    ```
    modal' "Confirm Action" theme {} do
      text' "Are you sure you want to proceed?" theme.bodyStyle
      spacer' 1 1
      row' (gap := 2) {} do
        text' "[Y]es" theme.primaryStyle
        text' "[N]o" theme.bodyStyle
    ```
-/
def modal' (title : String) (theme : Theme) (config : ModalConfig := {})
    (content : WidgetM α) : WidgetM α := do
  -- Use fillStyle for opaque background
  let fillStyle : Style := { bg := theme.background }
  -- Use white border for better visibility on dark backgrounds
  let modalTheme := { theme with border := .ansi .white }
  titledBlock' title config.borderType modalTheme (some fillStyle) content

/-- Create an untitled modal (simple bordered box) with opaque background. -/
def modalBox' (theme : Theme) (config : ModalConfig := {})
    (content : WidgetM α) : WidgetM α := do
  let fillStyle : Style := { bg := theme.background }
  -- Use white border for better visibility on dark backgrounds
  let modalTheme := { theme with border := .ansi .white }
  block' config.borderType modalTheme (some fillStyle) content

/-- Create a modal that appears when visible.
    Combines overlayWhen' with modal'. Uses backdrop dimming. -/
def modalWhen' (visible : Reactive.Dynamic Spider Bool) (title : String) (theme : Theme)
    (config : ModalConfig := {}) (content : WidgetM α) : WidgetM Unit := do
  -- Use backdrop style for dimming effect
  let backdropStyle : Style := { bg := .ansi .black, modifier := { dim := true } }
  overlayWhen' visible { backdropStyle := some backdropStyle } do
    let _ ← modal' title theme config content
    pure ()

/-! ## Confirmation Dialog

A common modal pattern for yes/no confirmations.
-/

/-- Result from a confirmation dialog. -/
structure ConfirmResult where
  /-- Event fired when user confirms (Y/Enter). -/
  confirmed : Reactive.Event Spider Unit
  /-- Event fired when user cancels (N/Escape). -/
  cancelled : Reactive.Event Spider Unit


/-- Create a confirmation dialog.

    The dialog responds to:
    - Y or Enter: confirm
    - N or Escape: cancel

    Example:
    ```
    let showConfirm ← ... -- Dynamic Bool controlling visibility
    let confirm ← confirmDialog' "Delete this item?" showConfirm theme

    -- Handle confirmation
    performEvent_ confirm.confirmed do
      -- Delete the item
      ...
    ```
-/
def confirmDialog' (message : String) (visible : Reactive.Dynamic Spider Bool) (theme : Theme)
    : WidgetM ConfirmResult := do
  -- Get visibility-gated key events
  let keyEvents ← useVisibilityGatedKeyEventsW visible

  -- Derive confirmed event from key presses
  let confirmedEvent ← Event.filterM (fun kd =>
    match kd.event.code with
    | .char 'y' | .char 'Y' | .enter => true
    | _ => false) keyEvents
  let confirmedEvent ← Event.voidM confirmedEvent

  -- Derive cancelled event from key presses
  let cancelledEvent ← Event.filterM (fun kd =>
    match kd.event.code with
    | .char 'n' | .char 'N' | .escape => true
    | _ => false) keyEvents
  let cancelledEvent ← Event.voidM cancelledEvent

  -- Emit the dialog content with backdrop dimming
  let backdropStyle : Style := { bg := .ansi .black, modifier := { dim := true } }
  overlayWhen' visible { backdropStyle := some backdropStyle } do
    let _ ← modal' "Confirm" theme {} do
      text' message theme.bodyStyle
      spacer' 1 1
      row' (gap := 3) {} do
        text' "[Y]es" theme.primaryStyle
        text' "[N]o" theme.captionStyle
    pure ()

  pure { confirmed := confirmedEvent, cancelled := cancelledEvent }

/-- Create a simple message dialog (OK to dismiss).

    Example:
    ```
    messageDialog' "Operation completed successfully!" showMessage theme
    ```
-/
def messageDialog' (message : String) (visible : Reactive.Dynamic Spider Bool) (theme : Theme)
    : WidgetM (Reactive.Event Spider Unit) := do
  -- Get visibility-gated key events
  let keyEvents ← useVisibilityGatedKeyEventsW visible

  -- Derive dismiss event from key presses
  let dismissEvent ← Event.filterM (fun kd =>
    match kd.event.code with
    | .enter | .escape | .space => true
    | _ => false) keyEvents
  let dismissEvent ← Event.voidM dismissEvent

  -- Emit the dialog content with backdrop dimming
  let backdropStyle : Style := { bg := .ansi .black, modifier := { dim := true } }
  overlayWhen' visible { backdropStyle := some backdropStyle } do
    let _ ← modal' "Message" theme {} do
      text' message theme.bodyStyle
      spacer' 1 1
      text' "[OK]" theme.primaryStyle
    pure ()

  pure dismissEvent

/-! ## Alert/Error Dialogs -/

/-- Create an error dialog with error styling. -/
def errorDialog' (message : String) (visible : Reactive.Dynamic Spider Bool) (theme : Theme)
    : WidgetM (Reactive.Event Spider Unit) := do
  -- Get visibility-gated key events
  let keyEvents ← useVisibilityGatedKeyEventsW visible

  -- Derive dismiss event from key presses
  let dismissEvent ← Event.filterM (fun kd =>
    match kd.event.code with
    | .enter | .escape | .space => true
    | _ => false) keyEvents
  let dismissEvent ← Event.voidM dismissEvent

  let backdropStyle : Style := { bg := .ansi .black, modifier := { dim := true } }
  overlayWhen' visible { backdropStyle := some backdropStyle } do
    let fillStyle : Style := { bg := theme.background }
    let _ ← titledBlock' "Error" .rounded { theme with border := theme.error } (some fillStyle) do
      text' message { fg := theme.error }
      spacer' 1 1
      text' "[OK]" theme.primaryStyle
    pure ()

  pure dismissEvent

/-- Create a warning dialog with warning styling. -/
def warningDialog' (message : String) (visible : Reactive.Dynamic Spider Bool) (theme : Theme)
    : WidgetM (Reactive.Event Spider Unit) := do
  -- Get visibility-gated key events
  let keyEvents ← useVisibilityGatedKeyEventsW visible

  -- Derive dismiss event from key presses
  let dismissEvent ← Event.filterM (fun kd =>
    match kd.event.code with
    | .enter | .escape | .space => true
    | _ => false) keyEvents
  let dismissEvent ← Event.voidM dismissEvent

  let backdropStyle : Style := { bg := .ansi .black, modifier := { dim := true } }
  overlayWhen' visible { backdropStyle := some backdropStyle } do
    let fillStyle : Style := { bg := theme.background }
    let _ ← titledBlock' "Warning" .rounded { theme with border := theme.warning } (some fillStyle) do
      text' message { fg := theme.warning }
      spacer' 1 1
      text' "[OK]" theme.primaryStyle
    pure ()

  pure dismissEvent

/-! ## Input Dialog

Modal with a text input field.
-/

/-- Result from an input dialog. -/
structure InputDialogResult where
  /-- Event fired when user submits (contains the input value). -/
  submitted : Reactive.Event Spider String
  /-- Event fired when user cancels. -/
  cancelled : Reactive.Event Spider Unit


/-- Create an input dialog with a text field.

    Example:
    ```
    let showInput ← ...
    let result ← inputDialog' "Enter your name:" showInput theme

    performEvent_ result.submitted fun name =>
      -- Use the entered name
      ...
    ```
-/
def inputDialog' (prompt : String) (visible : Reactive.Dynamic Spider Bool) (theme : Theme)
    (placeholder : String := "") : WidgetM InputDialogResult := do
  let events ← getEventsW

  -- Focus management for the input
  let dialogInputName := "input-dialog-field"

  -- Auto-focus when visibility changes
  let _focusUnsub ← SpiderM.liftIO <| visible.updated.subscribe fun isVisible => do
    if isVisible then
      events.registry.fireFocus (some dialogInputName)
    else
      events.registry.fireFocus none

  -- Get visibility-gated key events
  let keyEvents ← useVisibilityGatedKeyEventsW visible

  -- Get focus state for text input gating
  let focusedInput ← useFocusedInputW
  let isFocusedDyn ← focusedInput.map' (· == some dialogInputName)

  -- Focus-gated key events for text input
  let focusedKeyEvents ← Event.gateM isFocusedDyn.current keyEvents

  -- Map key events to input string transformations
  let inputOps ← Event.mapMaybeM (fun kd =>
    match kd.event.code with
    | .char c => if c.val >= 32 then some (fun s => s ++ c.toString) else none
    | .backspace => some (fun s => s.dropRight 1)
    | _ => none) focusedKeyEvents

  -- Fold to get input value
  let inputDyn ← foldDyn id "" inputOps

  -- Derive submitted event (Enter when focused, with current value)
  let enterEvents ← Event.filterM (fun kd => kd.event.code == .enter) focusedKeyEvents
  let submittedEvent ← Event.attachWithM (fun value _ => value) inputDyn.current enterEvents

  -- Derive cancelled event (Escape, regardless of focus within dialog)
  let cancelledEvent ← Event.filterM (fun kd => kd.event.code == .escape) keyEvents
  let cancelledEvent ← Event.voidM cancelledEvent

  -- Emit the dialog content with backdrop dimming
  let backdropStyle : Style := { bg := .ansi .black, modifier := { dim := true } }
  overlayWhen' visible { backdropStyle := some backdropStyle } do
    let _ ← modal' "Input" theme {} do
      text' prompt theme.bodyStyle
      spacer' 1 1
      -- Simple text display for the input (full textInput' would create circular dep)
      let focusDyn ← focusedInput.map' (· == some dialogInputName)
      let node ← focusDyn.zipWith' (fun isFocused value =>
        let display := if value.isEmpty then
          if isFocused then "|" else placeholder
        else
          if isFocused then value ++ "|" else value
        let style := if isFocused then theme.primaryStyle else theme.bodyStyle
        RNode.text display style
      ) inputDyn
      emit node
      spacer' 1 1
      row' (gap := 2) {} do
        text' "[Enter] Submit" theme.captionStyle
        text' "[Esc] Cancel" theme.captionStyle
    pure ()

  pure { submitted := submittedEvent, cancelled := cancelledEvent }

end Terminus.Reactive
