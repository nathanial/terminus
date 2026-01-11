/-
  Reactive Data Demo Executable
  Run with: lake exe reactive_data
-/
import Terminus.Reactive.Demos.ReactiveData

open Terminus.Reactive

def main : IO Unit :=
  runReactiveApp reactiveDataApp { debug := true, logPath := "reactive_data.log" }
