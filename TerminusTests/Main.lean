-- TerminusTests.Main: Test runner for Terminus terminal library

import Crucible

-- Core tests
import TerminusTests.RawModeTests
import TerminusTests.TerminalSizeTests
import TerminusTests.InputTests
import TerminusTests.OutputTests
import TerminusTests.CellTests
import TerminusTests.BufferTests
import TerminusTests.RectTests
import TerminusTests.StyleTests
import TerminusTests.LayoutTests
import TerminusTests.DebugTests

-- Reactive tests
import TerminusTests.Reactive.Common
import TerminusTests.Reactive.RenderingTests
import TerminusTests.Reactive.WidgetTests
import TerminusTests.Reactive.ComponentTests
import TerminusTests.Reactive.OverlayTests
import TerminusTests.Reactive.ScrollTests
import TerminusTests.Reactive.IntegrationTests
import TerminusTests.Reactive.TabsTests
import TerminusTests.Reactive.TreeTests
import TerminusTests.Reactive.GridTests
import TerminusTests.Reactive.DataGridTests
import TerminusTests.Reactive.AnimationTests
import TerminusTests.Reactive.TextAreaTests
import TerminusTests.Reactive.FormTests
import TerminusTests.Reactive.AsyncTests
import TerminusTests.Reactive.LayoutTests
import TerminusTests.Reactive.CheckboxTests
import TerminusTests.Reactive.FeedbackTests
import TerminusTests.Reactive.NavigationTests
import TerminusTests.Reactive.UtilityTests
import TerminusTests.Reactive.ChartTests
import TerminusTests.Reactive.ReproOverlayRendering

open Crucible

def main : IO UInt32 := do
  IO.println "╔════════════════════════════════════════╗"
  IO.println "║       Terminus Test Suite              ║"
  IO.println "╚════════════════════════════════════════╝"

  let exitCode ← runAllSuites

  IO.println ""
  if exitCode == 0 then
    IO.println "✓ All test suites passed!"
  else
    IO.println s!"✗ {exitCode} test suite(s) had failures"

  return exitCode
