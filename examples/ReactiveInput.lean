/-
  Reactive Input Demo Executable
  Run with: lake exe reactive_input
-/
import Terminus.Reactive.Demos.ReactiveInput

open Terminus.Reactive

def main : IO Unit :=
  runReactiveApp reactiveInputApp { debug := true, logPath := "reactive_input.log" }
