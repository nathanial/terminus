/-
  Reactive Charts Demo Executable
  Run with: lake exe reactive_charts
-/
import Terminus.Reactive.Demos.ReactiveCharts

open Terminus.Reactive

def main : IO Unit :=
  runReactiveApp reactiveChartsApp { debug := true, logPath := "reactive_charts.log" }
