/-
  Reactive Feedback Demo Executable
  Run with: lake exe reactive_feedback
-/
import Terminus.Reactive.Demos.ReactiveFeedback

open Terminus.Reactive

def main : IO Unit :=
  runReactiveApp reactiveFeedbackApp { debug := true, logPath := "reactive_feedback.log" }
