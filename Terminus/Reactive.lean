/-
  Terminus Reactive
  FRP-powered reactive widget system for terminal UIs.

  This module provides a Reflex-style reactive widget system for building
  terminal applications with declarative, composable components.

  ## Quick Start

  ```lean
  import Terminus.Reactive

  open Terminus.Reactive

  def main : IO Unit := runReactiveApp fun => do
    let keyEvents ← useKeyEvent
    let count ← Reactive.foldDyn (fun _ n => n + 1) 0 keyEvents

    let (_, render) ← runWidget do
      column' (gap := 1) (style := {}) do
        heading1' "Key Counter" Theme.dark
        emitDynamic do
          let n ← count.sample
          pure (RNode.text s!"Keys pressed: {n}" (Theme.dark.bodyStyle))

    pure { render }
  ```

  ## Module Overview

  - **Types**: `RNode`, `Theme`, `TerminusEvents`, `ComponentRegistry`
  - **Monad**: `ReactiveTermM`, `WidgetM` monad stack
  - **Hooks**: `useKeyEvent`, `useMouseEvent`, `useResize`
  - **Components**: `text'`, `heading1'`, `caption'`, `dynText'`
  - **Containers**: `row'`, `column'`, `block'`, `titledPanel'`
  - **Input**: `textInput'`, `labeledTextInput'`
  - **List**: `selectableList'`, `dynSelectableList'`, `numberedList'`
  - **Tabs**: `tabs'`, `numberedTabs'`, `dynTabs'`
  - **Tree**: `tree'`, `forest'`, `TreeNode`
  - **Overlay**: `overlay'`, `modal'`, `confirmDialog'`, `messageDialog'`
  - **ScrollView**: `scrollView'`, `vscrollView'`, `hscrollView'`
  - **Grid**: `grid'`, `dynGrid'`, `cursorGrid'`, `GridCell`
  - **Animation**: `useAnimation`, `usePulse`, `useCycle`, `AnimPhase`, `Easing`
  - **TextArea**: `textArea'`, `labeledTextArea'`, `textDisplay'`
  - **Form**: `form'`, `optionSelector'`, `checkbox'`, `submitButton'`
  - **Async**: `useAsync`, `useAsyncOnce`, `useStream`, `useDebounceAsync`
  - **Layout**: `horizontalSplit'`, `verticalSplit'`, `sidebarLayout'`
  - **Render**: `render`, `renderToBuffer`, `computeLayout`, `ClipContext`
  - **App**: `runReactiveApp`, `runSimpleApp`
-/
import Terminus.Reactive.Types
import Terminus.Reactive.Monad
import Terminus.Reactive.Render
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Terminus.Reactive.Hooks
import Terminus.Reactive.Input
import Terminus.Reactive.List
import Terminus.Reactive.Tabs
import Terminus.Reactive.Tree
import Terminus.Reactive.Overlay
import Terminus.Reactive.ScrollView
import Terminus.Reactive.Grid
import Terminus.Reactive.Animation
import Terminus.Reactive.TextArea
import Terminus.Reactive.Form
import Terminus.Reactive.Async
import Terminus.Reactive.Layout
import Terminus.Reactive.App
import Terminus.Reactive.Checkbox
import Terminus.Reactive.Spinner
import Terminus.Reactive.Logger
import Terminus.Reactive.Notification
import Terminus.Reactive.Menu
import Terminus.Reactive.Table
import Terminus.Reactive.Calendar
import Terminus.Reactive.Scrollbar
import Terminus.Reactive.Paragraph
import Terminus.Reactive.Popup
import Terminus.Reactive.Clear
import Terminus.Reactive.Gauge
import Terminus.Reactive.Sparkline
import Terminus.Reactive.Charts
import Terminus.Reactive.Canvas
import Terminus.Reactive.Image
import Terminus.Reactive.BigText
