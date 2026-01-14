/-
  Terminus Reactive - Display Components
  Text and label widgets for reactive terminal UIs.
-/
import Terminus.Reactive.Monad
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Static Text Components

These emit visual-only widgets without returning events or dynamics.
-/

/-- Emit plain text with a style. -/
def text' (content : String) (style : Style) : WidgetM Unit := do
  emitStatic (RNode.text content style)

/-- Emit styled text with foreground color. -/
def styledText' (content : String) (fg : Color) : WidgetM Unit := do
  emitStatic (RNode.text content { fg })

/-- Emit a heading level 1. -/
def heading1' (content : String) (theme : Theme) : WidgetM Unit := do
  emitStatic (RNode.text content (theme.heading1Style))

/-- Emit a heading level 2. -/
def heading2' (content : String) (theme : Theme) : WidgetM Unit := do
  emitStatic (RNode.text content (theme.heading2Style))

/-- Emit a heading level 3. -/
def heading3' (content : String) (theme : Theme) : WidgetM Unit := do
  emitStatic (RNode.text content (theme.heading3Style))

/-- Emit body text. -/
def bodyText' (content : String) (theme : Theme) : WidgetM Unit := do
  emitStatic (RNode.text content (theme.bodyStyle))

/-- Emit caption/muted text. -/
def caption' (content : String) (theme : Theme) : WidgetM Unit := do
  emitStatic (RNode.text content (theme.captionStyle))

/-- Emit primary-colored text. -/
def primaryText' (content : String) (theme : Theme) : WidgetM Unit := do
  emitStatic (RNode.text content (theme.primaryStyle))

/-- Emit a spacer. -/
def spacer' (width height : Nat := 1) : WidgetM Unit := do
  emitStatic (RNode.spacer width height)

/-! ## Dynamic Text Components

These sample dynamics at render time to produce updated content.
-/

/-- Emit text that updates when the Dynamic changes. -/
def dynText' (content : Dynamic Spider String) (style : Style) : WidgetM Unit := do
  let node ← content.map' fun text => RNode.text text style
  emit node

/-- Emit dynamic body text. -/
def dynBodyText' (content : Dynamic Spider String) (theme : Theme) : WidgetM Unit := do
  let node ← content.map' fun text => RNode.text text (theme.bodyStyle)
  emit node

/-- Emit dynamic caption text. -/
def dynCaption' (content : Dynamic Spider String) (theme : Theme) : WidgetM Unit := do
  let node ← content.map' fun text => RNode.text text (theme.captionStyle)
  emit node

/-! ## Conditional Rendering -/

/-- Emit a widget only when condition is true (sampled at render time). -/
def when' (condition : Dynamic Spider Bool) (content : WidgetM Unit) : WidgetM Unit := do
  let (_, childRenders) ← runWidgetChildren content
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let node ← condition.zipWith' (fun visible kids =>
    if visible then
      if kids.isEmpty then
        RNode.empty
      else
        RNode.column 0 {} kids.toArray
    else
      RNode.empty
  ) childrenList
  emit node

/-- Emit one of two widgets based on condition (sampled at render time). -/
def ifThenElse' (condition : Dynamic Spider Bool)
    (thenContent : WidgetM Unit) (elseContent : WidgetM Unit) : WidgetM Unit := do
  let (_, thenRenders) ← runWidgetChildren thenContent
  let (_, elseRenders) ← runWidgetChildren elseContent
  let thenList ← Reactive.Dynamic.sequence thenRenders.toList
  let elseList ← Reactive.Dynamic.sequence elseRenders.toList
  let node ← condition.zipWith3' (fun visible thenKids elseKids =>
    let kids := if visible then thenKids else elseKids
    if kids.isEmpty then
      RNode.empty
    else
      RNode.column 0 {} kids.toArray
  ) thenList elseList
  emit node

/-- Emit a dynamic widget (the IO action is run at render time). -/
def emitDynamic (render : ComponentRender) : WidgetM Unit := emit render

/-! ## Progress Bar Components -/

/-- Configuration for a progress bar. -/
structure ProgressBarConfig where
  /-- Total width of the progress bar in characters. -/
  width : Nat := 20
  /-- Character used for the filled portion. -/
  filledChar : Char := '█'
  /-- Character used for the empty portion. -/
  emptyChar : Char := '░'
  /-- Style for the filled portion. -/
  filledStyle : Style := { fg := .ansi .cyan }
  /-- Style for the empty portion. -/
  emptyStyle : Style := { fg := .ansi .brightBlack }
  /-- Whether to show percentage text. -/
  showPercentage : Bool := true
  /-- Style for percentage text. -/
  percentageStyle : Style := {}
  deriving Repr, Inhabited

/-- Create a progress bar string from a progress value (0.0 to 1.0). -/
def renderProgressBar (progress : Float) (config : ProgressBarConfig := {}) : String :=
  let clampedProgress := max 0.0 (min 1.0 progress)
  let filledCount := (clampedProgress * config.width.toFloat).toUInt32.toNat
  let emptyCount := config.width - filledCount
  let filled := String.ofList (List.replicate filledCount config.filledChar)
  let empty := String.ofList (List.replicate emptyCount config.emptyChar)
  if config.showPercentage then
    let percent := (clampedProgress * 100).toUInt32
    s!"{filled}{empty} {percent}%"
  else
    s!"{filled}{empty}"

/-- Emit a static progress bar. -/
def progressBar' (progress : Float) (config : ProgressBarConfig := {}) : WidgetM Unit := do
  let barStr := renderProgressBar progress config
  text' barStr config.filledStyle

/-- Emit a dynamic progress bar that updates when the progress Dynamic changes. -/
def dynProgressBar' (progress : Reactive.Dynamic Spider Float) (config : ProgressBarConfig := {})
    : WidgetM Unit := do
  let node ← progress.map' fun p =>
    let clampedProgress := max 0.0 (min 1.0 p)
    let filledCount := (clampedProgress * config.width.toFloat).toUInt32.toNat
    let emptyCount := config.width - filledCount
    let filled := String.ofList (List.replicate filledCount config.filledChar)
    let empty := String.ofList (List.replicate emptyCount config.emptyChar)
    let filledNode := RNode.text filled config.filledStyle
    let emptyNode := RNode.text empty config.emptyStyle
    if config.showPercentage then
      let percent := (clampedProgress * 100).toUInt32
      let percentNode := RNode.text s!" {percent}%" config.percentageStyle
      RNode.row 0 {} #[filledNode, emptyNode, percentNode]
    else
      RNode.row 0 {} #[filledNode, emptyNode]
  emit node

/-! ## Dynamic Widget Subtrees

`dynWidget` enables rebuilding entire widget subtrees when a Dynamic value changes.
This is similar to Reflex's `dyn` combinator.
-/

/-- Run a dynamic widget computation. When the input Dynamic changes, the widget
    builder is re-run with the new value, rebuilding the subtree with fresh
    reactive subscriptions.

    Each build runs in its own child scope. When the dynamic updates, the previous
    build's scope is cleared (cleaning up all subscriptions from that build) and
    reused for the rebuild. This prevents subscription leaks that would occur if
    we created a new child scope on each rebuild.

    Example (dependent content):
    ```
    let selection ← dropdown options theme 0
    let _ ← dynWidget selection.value fun idx =>
      bodyText' s!"Selected: {options[idx]!}" theme
    ```
-/
def dynWidget (dynValue : Dynamic Spider a) (builder : a → WidgetM b)
    : WidgetM (Dynamic Spider b) := do
  let events ← getEventsW  -- Capture TerminusEvents context

  -- All scope and initial build logic in one SpiderM block to access env.currentScope
  let (initialResult, initialRender, childScopeRef) ← (⟨fun env => do
    -- Create child scope for builder subscriptions (enables cleanup on rebuild)
    let initialChildScope ← env.currentScope.child
    let scopeRef : IO.Ref Reactive.SubscriptionScope ← IO.mkRef initialChildScope

    -- Initial build in child scope
    let initialValue ← dynValue.sample
    let widgetM := runWidget (builder initialValue)
    let spiderM := widgetM.run events
    let (result, render) ← spiderM.run { env with currentScope := initialChildScope }

    pure (result, render, scopeRef)
  ⟩ : SpiderM (b × ComponentRender × IO.Ref Reactive.SubscriptionScope))

  -- Result tracking via trigger event
  let (resultTrigger, fireResult) ← newTriggerEvent
  let resultDyn ← holdDyn initialResult resultTrigger

  -- Render tracking via Dynamic.switch (terminus uses Dynamic Spider RNode)
  let (renderTrigger, fireRender) ← newTriggerEvent
  let renderDynDyn ← holdDyn initialRender renderTrigger
  let switchedRender ← Reactive.Dynamic.switch renderDynDyn

  -- Subscribe to rebuilds when dynValue changes
  let subscribeAction : SpiderM Unit := ⟨fun env => do
    let unsub ← Reactive.Event.subscribe dynValue.updated fun newValue => do
      -- Clear the child scope's subscriptions (but keep the scope alive for reuse).
      -- Using clear instead of dispose+child prevents leaking entries in the parent
      -- scope's subscriptions array, which would grow unboundedly for animated widgets.
      let childScope ← childScopeRef.get
      childScope.clear

      -- Run the builder with the new value, reusing the same child scope
      let widgetM := runWidget (builder newValue)
      let spiderM := widgetM.run events
      let (result, render) ← spiderM.run { env with currentScope := childScope }
      fireResult result
      fireRender render
    env.currentScope.register unsub⟩
  subscribeAction  -- Lift SpiderM to WidgetM via MonadLift

  emit switchedRender

  pure resultDyn

end Terminus.Reactive
