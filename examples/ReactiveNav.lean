/-
  Reactive Navigation Demo Executable
  Run with: lake exe reactive_nav
-/
import Terminus.Reactive.Demos.ReactiveNav

open Terminus.Reactive

def main : IO Unit :=
  runReactiveApp reactiveNavApp { debug := true, logPath := "reactive_nav.log" }
