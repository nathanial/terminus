/-
  Reactive Game Demo
  Demonstrates Grid and Animation widgets in a simple game-like app.
-/
import Terminus.Reactive
import Terminus.Reactive.Demos.ReactiveGame

open Terminus.Reactive

def main : IO Unit :=
  runReactiveApp reactiveGameApp { debug := true, logPath := "reactive_game.log" }
