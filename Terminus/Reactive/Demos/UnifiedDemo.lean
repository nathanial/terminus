/-
  Terminus Unified Demo
  A single application demonstrating all Terminus reactive widgets.
-/
import Terminus.Reactive
import Terminus.Reactive.Demos.UnifiedBasics
import Terminus.Reactive.Demos.UnifiedInput
import Terminus.Reactive.Demos.UnifiedNavigation
import Terminus.Reactive.Demos.UnifiedData
import Terminus.Reactive.Demos.UnifiedCharts
import Terminus.Reactive.Demos.UnifiedFeedback
import Terminus.Reactive.Demos.UnifiedMedia
import Terminus.Reactive.Demos.UnifiedAsync
import Terminus.Reactive.Demos.UnifiedOverlay

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Tab Identifiers -/

inductive DemoTab where
  | basics
  | input
  | navigation
  | data
  | charts
  | feedback
  | media
  | async
  | overlay
  deriving Repr, BEq, Inhabited

def DemoTab.label : DemoTab → String
  | .basics => "Basics"
  | .input => "Input"
  | .navigation => "Navigation"
  | .data => "Data"
  | .charts => "Charts"
  | .feedback => "Feedback"
  | .media => "Media"
  | .async => "Async"
  | .overlay => "Overlay"

def DemoTab.allTabs : Array String := #[
  "Basics", "Input", "Navigation", "Data",
  "Charts", "Feedback", "Media", "Async", "Overlay"
]

def DemoTab.fromIndex : Nat → DemoTab
  | 0 => .basics
  | 1 => .input
  | 2 => .navigation
  | 3 => .data
  | 4 => .charts
  | 5 => .feedback
  | 6 => .media
  | 7 => .async
  | 8 => .overlay
  | _ => .basics

/-! ## Main Application -/

def mainContent (theme : Theme) (events : TerminusEvents) : WidgetM Unit := do
  column' (gap := 1) (style := {}) do
    -- Header
    text' "═══ Terminus Widget Showcase ═══" theme.heading1Style

    -- Main tabs for navigation
    let tabResult ← tabs' DemoTab.allTabs 0 {
      focusName := "main-tabs"
      activeStyle := { fg := .ansi .cyan, modifier := { bold := true } }
      globalKeys := false
    }

    -- Help text
    row' (gap := 2) {} do
      text' "Ctrl+C: quit" theme.captionStyle
      text' "←/→: tabs" theme.captionStyle
      text' "Tab/Shift+Tab: cycle focus" theme.captionStyle

    spacer' 0 1

    -- Handle Tab key for automatic focus cycling using FRP
    let keyEvents ← useKeyEventW
    let tabKeys ← Event.filterM (fun kd => kd.event.code == .tab) keyEvents
    let focusAction ← Event.mapM (fun kd =>
      if kd.event.modifiers.shift then
        events.registry.focusPrev
      else
        events.registry.focusNext
    ) tabKeys
    performEvent_ focusAction

    -- Content area based on selected tab (rebuilds on change)
    let _ ← dynWidget tabResult.activeTab fun idx => do
      match idx with
      | 0 => basicsContent theme
      | 1 => inputContent theme events
      | 2 => navigationContent theme events
      | 3 => dataContent theme events
      | 4 => chartsContent theme
      | 5 => feedbackContent theme
      | 6 => mediaContent theme
      | 7 => asyncContent theme
      | 8 => overlayContent theme
      | _ => basicsContent theme

def footerContent (theme : Theme) : WidgetM Unit := do
  let focusedInput ← useFocusedInputW
  let node ← focusedInput.map' (fun focused =>
    let focusName := focused.getD "(none)"
    RNode.text s!"Focused: {focusName}" theme.captionStyle
  )
  emit node

def app : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark
  let events ← getEvents

  -- Build the UI
  let (_, render) ← runWidget do
    let _ ← dockBottom' (footerHeight := 1)
      (content := mainContent theme events)
      (footer := footerContent theme)

  -- Focus first input widget after setup
  SpiderM.liftIO <| events.registry.focusNext

  pure { render }

def runDemo : IO Unit := runReactiveApp app { debug := true }

end Terminus.Reactive.Demos.UnifiedDemo
