/-
  Reactive Charts Demo Widgets
  Demonstrates Gauge, Sparkline, BarChart, LineChart, and PieChart widgets.
-/
import Terminus.Reactive

open Terminus.Reactive
open Reactive Reactive.Host

def reactiveChartsApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Get tick events for animation
  let tickEvents ← useTick

  -- Animated progress value (0.0 to 1.0 cycling over 5 seconds)
  let progressDyn ← Reactive.foldDyn (fun td _ =>
    let cycleMs := td.elapsedMs % 5000
    cycleMs.toFloat / 5000.0
  ) 0.0 tickEvents

  -- Simulated data that changes over time
  let sparkDataRef ← SpiderM.liftIO (IO.mkRef #[3.0, 5.0, 2.0, 8.0, 4.0, 6.0, 7.0, 3.0, 9.0, 5.0])

  -- Update sparkline data periodically
  let _unsub ← SpiderM.liftIO <| tickEvents.subscribe fun td => do
    if td.frame % 30 == 0 then  -- Update every ~500ms at 60fps
      let current ← sparkDataRef.get
      -- Shift left and add new random-ish value
      let newVal := (td.elapsedMs % 10).toFloat + 1.0
      let newData := (current.toList.drop 1 ++ [newVal]).toArray
      sparkDataRef.set newData

  -- Build the UI
  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Charts Demo ===" theme.heading1Style
      text' "Visualization widgets with live data" theme.captionStyle

      row' (gap := 3) {} do
        -- Left column: Gauges
        column' (gap := 1) {} do
          titledBlock' "Gauges" .rounded theme do
            -- Animated gauge
            text' "Progress:" theme.captionStyle
            dynGauge' progressDyn { width := 25, showPercent := true }

            text' "" {}

            -- Static gauges
            text' "CPU Usage:" theme.captionStyle
            gauge' 0.65 { width := 25, filledStyle := { fg := .ansi .green } }

            text' "Memory:" theme.captionStyle
            gauge' 0.82 { width := 25, filledStyle := { fg := .ansi .yellow } }

            text' "Disk:" theme.captionStyle
            gauge' 0.45 { width := 25, filledStyle := { fg := .ansi .cyan } }

            text' "" {}
            text' "Line Gauges:" theme.captionStyle
            lineGauge' 0.7 { width := 25, label := some "Download" }
            lineGauge' 0.3 { width := 25, label := some "Upload" }

        -- Middle column: Sparklines and Bar Chart
        column' (gap := 1) {} do
          titledBlock' "Sparklines" .rounded theme do
            labeledSparkline' "CPU " #[30.0, 45.0, 60.0, 55.0, 70.0, 65.0, 80.0, 75.0] {
              style := { fg := .ansi .green }
              showValue := true
            }
            labeledSparkline' "MEM " #[50.0, 52.0, 55.0, 60.0, 58.0, 62.0, 65.0, 68.0] {
              style := { fg := .ansi .yellow }
              showValue := true
            }
            labeledSparkline' "NET " #[10.0, 25.0, 15.0, 40.0, 20.0, 35.0, 30.0, 45.0] {
              style := { fg := .ansi .cyan }
              showValue := true
            }

          titledBlock' "Bar Chart" .rounded theme do
            barChart' #[
              BarData.styled "Jan" 45 { fg := .ansi .blue },
              BarData.styled "Feb" 62 { fg := .ansi .blue },
              BarData.styled "Mar" 38 { fg := .ansi .blue },
              BarData.styled "Apr" 75 { fg := .ansi .green },
              BarData.styled "May" 58 { fg := .ansi .blue }
            ] {
              orientation := .vertical
              barWidth := 3
              size := 6
            }

        -- Right column: Line Chart and Pie Chart
        column' (gap := 1) {} do
          titledBlock' "Line Chart" .rounded theme do
            lineChart' #[
              DataSeries.labeled "Revenue" #[10.0, 25.0, 18.0, 32.0, 28.0, 40.0, 35.0]
                |> fun s => { s with style := { fg := .ansi .green }, marker := '●' },
              DataSeries.labeled "Costs" #[8.0, 12.0, 15.0, 18.0, 22.0, 25.0, 28.0]
                |> fun s => { s with style := { fg := .ansi .red }, marker := '○' }
            ] {
              width := 35
              height := 8
            }

          titledBlock' "Pie Chart" .rounded theme do
            pieChart' #[
              PieSlice.styled "Product A" 35 { fg := .ansi .blue },
              PieSlice.styled "Product B" 28 { fg := .ansi .green },
              PieSlice.styled "Product C" 22 { fg := .ansi .yellow },
              PieSlice.styled "Other" 15 { fg := .ansi .magenta }
            ] { radius := 8 }

      -- Footer
      text' "Press Ctrl+C to quit" theme.captionStyle

  pure { render }
