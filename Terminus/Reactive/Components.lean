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
  emit (pure (RNode.text content style))

/-- Emit styled text with foreground color. -/
def styledText' (content : String) (fg : Color) : WidgetM Unit := do
  emit (pure (RNode.text content { fg }))

/-- Emit a heading level 1. -/
def heading1' (content : String) (theme : Theme) : WidgetM Unit := do
  emit (pure (RNode.text content (theme.heading1Style)))

/-- Emit a heading level 2. -/
def heading2' (content : String) (theme : Theme) : WidgetM Unit := do
  emit (pure (RNode.text content (theme.heading2Style)))

/-- Emit a heading level 3. -/
def heading3' (content : String) (theme : Theme) : WidgetM Unit := do
  emit (pure (RNode.text content (theme.heading3Style)))

/-- Emit body text. -/
def bodyText' (content : String) (theme : Theme) : WidgetM Unit := do
  emit (pure (RNode.text content (theme.bodyStyle)))

/-- Emit caption/muted text. -/
def caption' (content : String) (theme : Theme) : WidgetM Unit := do
  emit (pure (RNode.text content (theme.captionStyle)))

/-- Emit primary-colored text. -/
def primaryText' (content : String) (theme : Theme) : WidgetM Unit := do
  emit (pure (RNode.text content (theme.primaryStyle)))

/-- Emit a spacer. -/
def spacer' (width height : Nat := 1) : WidgetM Unit := do
  emit (pure (RNode.spacer width height))

/-! ## Dynamic Text Components

These sample dynamics at render time to produce updated content.
-/

/-- Emit text that updates when the Dynamic changes. -/
def dynText' (content : Dynamic Spider String) (style : Style) : WidgetM Unit := do
  emit do
    let text ← content.sample
    pure (RNode.text text style)

/-- Emit dynamic body text. -/
def dynBodyText' (content : Dynamic Spider String) (theme : Theme) : WidgetM Unit := do
  emit do
    let text ← content.sample
    pure (RNode.text text (theme.bodyStyle))

/-- Emit dynamic caption text. -/
def dynCaption' (content : Dynamic Spider String) (theme : Theme) : WidgetM Unit := do
  emit do
    let text ← content.sample
    pure (RNode.text text (theme.captionStyle))

/-! ## Conditional Rendering -/

/-- Emit a widget only when condition is true (sampled at render time). -/
def when' (condition : Dynamic Spider Bool) (content : WidgetM Unit) : WidgetM Unit := do
  let (_, childRenders) ← runWidgetChildren content
  emit do
    let visible ← condition.sample
    if visible then
      let nodes ← childRenders.mapM id
      pure (RNode.column 0 {} nodes)
    else
      pure RNode.empty

/-- Emit one of two widgets based on condition (sampled at render time). -/
def ifThenElse' (condition : Dynamic Spider Bool)
    (thenContent : WidgetM Unit) (elseContent : WidgetM Unit) : WidgetM Unit := do
  let (_, thenRenders) ← runWidgetChildren thenContent
  let (_, elseRenders) ← runWidgetChildren elseContent
  emit do
    let visible ← condition.sample
    let renders := if visible then thenRenders else elseRenders
    let nodes ← renders.mapM id
    pure (RNode.column 0 {} nodes)

/-- Emit a dynamic widget (the IO action is run at render time). -/
def emitDynamic (render : ComponentRender) : WidgetM Unit := emit render

/-! ## Dynamic Widget Subtrees

`dynWidget` enables rebuilding entire widget subtrees when a Dynamic value changes.
This is similar to Reflex's `dyn` combinator.
-/

/-- Run a dynamic widget computation. When the input Dynamic changes, the widget
    builder is re-run with the new value, rebuilding the subtree with fresh
    reactive subscriptions.

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

  -- Initial build (Dynamic.sample is IO, so lift it)
  let initialValue ← SpiderM.liftIO dynValue.sample
  let (initialResult, initialRenders) ← runWidgetChildren (builder initialValue)

  -- Refs for current state
  let rendersRef : IO.Ref (Array ComponentRender) ← SpiderM.liftIO (IO.mkRef initialRenders)

  -- Result tracking via trigger event
  let (resultTrigger, fireResult) ← newTriggerEvent
  let resultDyn ← holdDyn initialResult resultTrigger

  -- Subscribe to rebuilds when dynValue changes
  let subscribeAction : SpiderM Unit := ⟨fun env => do
    let unsub ← Reactive.Event.subscribe dynValue.updated fun newValue => do
      -- Run the builder with the new value in captured context
      let widgetM := runWidgetChildren (builder newValue)
      let reactiveM := widgetM.run { children := #[] }
      let spiderM := reactiveM.run events
      let ((result, renders), _) ← spiderM.run env
      rendersRef.set renders
      fireResult result
    env.currentScope.register unsub⟩
  subscribeAction

  -- Emit render that uses current renders
  emit do
    let renders ← rendersRef.get
    if renders.isEmpty then
      pure RNode.empty
    else
      let nodes ← renders.mapM id
      pure (RNode.column 0 {} nodes)

  pure resultDyn

end Terminus.Reactive
