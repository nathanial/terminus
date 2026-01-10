/-
  Reactive Demo
  Demonstrates the Terminus.Reactive widget system with animated progress bar.
-/
import Terminus.Reactive
import Terminus.Reactive.Demos.ReactiveDemo

open Terminus.Reactive
open Reactive Reactive.Host

def main : IO Unit := runReactiveApp (config := { debug := true }) reactiveDemoApp
