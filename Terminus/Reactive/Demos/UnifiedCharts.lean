import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Charts Tab Content -/

def chartsContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Gauges, sparklines, and charts." theme.captionStyle

    row' (gap := 2) {} do
      -- Sparklines
      column' (gap := 1) {} do
        titledBlock' "Sparklines" .rounded theme none do
          labeledSparkline' "CPU " #[30.0, 45.0, 60.0, 55.0, 70.0, 65.0, 80.0] {
            style := { fg := .ansi .green }
            showValue := true
          }
          labeledSparkline' "MEM " #[50.0, 52.0, 55.0, 60.0, 58.0, 62.0, 65.0] {
            style := { fg := .ansi .yellow }
            showValue := true
          }
          labeledSparkline' "NET " #[10.0, 25.0, 15.0, 40.0, 20.0, 35.0, 30.0] {
            style := { fg := .ansi .cyan }
            showValue := true
          }

      -- Bar chart
      column' (gap := 1) {} do
        titledBlock' "Bar Chart" .rounded theme none do
          barChart' #[
            BarData.styled "Jan" 45 { fg := .ansi .blue },
            BarData.styled "Feb" 62 { fg := .ansi .blue },
            BarData.styled "Mar" 38 { fg := .ansi .blue },
            BarData.styled "Apr" 75 { fg := .ansi .green }
          ] {
            orientation := .vertical
            barWidth := 3
            size := 5
          }

      -- Pie chart
      column' (gap := 1) {} do
        titledBlock' "Pie Chart" .rounded theme none do
          pieChart' #[
            PieSlice.styled "A" 35 { fg := .ansi .blue },
            PieSlice.styled "B" 28 { fg := .ansi .green },
            PieSlice.styled "C" 22 { fg := .ansi .yellow },
            PieSlice.styled "D" 15 { fg := .ansi .magenta }
          ] { radius := 6 }

    spacer' 0 1

    row' (gap := 2) {} do
      -- Line chart
      column' (gap := 1) {} do
        titledBlock' "Line Chart" .rounded theme none do
          lineChart' #[
            DataSeries.labeled "Sales" #[10.0, 18.0, 25.0, 20.0, 30.0, 28.0],
            DataSeries.labeled "Costs" #[8.0, 12.0, 15.0, 14.0, 20.0, 18.0]
          ] {
            width := 32
            height := 8
            showLegend := true
          }

      -- Vertical gauge
      column' (gap := 1) {} do
        titledBlock' "Vertical Gauge" .rounded theme none do
          row' (gap := 2) {} do
            vGauge' 0.65 { height := 8 }
            vGauge' 0.35 { height := 8, filledStyle := { fg := .ansi .yellow } }
