/-
  Reactive Editor Demo
  Run with: lake exe reactive_editor
-/
import Terminus.Reactive
import Terminus.Reactive.Demos.ReactiveEditor

open Terminus.Reactive

def main : IO Unit :=
  runReactiveApp reactiveEditorApp { debug := true, logPath := "reactive_editor.log" }
