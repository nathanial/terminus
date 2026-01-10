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
  - **Render**: `render`, `renderToBuffer`, `computeLayout`
  - **App**: `runReactiveApp`, `runSimpleApp`
-/
import Terminus.Reactive.Types
import Terminus.Reactive.Monad
import Terminus.Reactive.Render
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Terminus.Reactive.Hooks
import Terminus.Reactive.App
