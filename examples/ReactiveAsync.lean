/-
  Reactive Async Demo
  Run with: lake exe reactive_async
-/
import Terminus.Reactive
import Terminus.Reactive.Demos.ReactiveAsync

open Terminus.Reactive

def main : IO Unit :=
  runReactiveApp reactiveAsyncApp { debug := false }
