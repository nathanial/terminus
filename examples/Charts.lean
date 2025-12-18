-- Charts: Data visualization demo with Sparkline, BarChart, and LineChart
-- Use Tab to switch views, 'q' to quit

import Terminus

open Terminus

structure ChartsState where
  activeTab : Nat := 0
  tick : Nat := 0
  sparklineData : List Float := []
  sparklineData2 : List Float := []
  deriving Inhabited

def tabTitles : List String := ["Sparklines", "Bar Chart", "Line Chart"]

def barChartData : List BarData := [
  BarData.new "Mon" 45 |>.withStyle (Style.fgColor Color.blue),
  BarData.new "Tue" 72 |>.withStyle (Style.fgColor Color.green),
  BarData.new "Wed" 58 |>.withStyle (Style.fgColor Color.yellow),
  BarData.new "Thu" 91 |>.withStyle (Style.fgColor Color.magenta),
  BarData.new "Fri" 66 |>.withStyle (Style.fgColor Color.cyan)
]

def lineChartSeries : List DataSeries := [
  { data := [20, 45, 30, 55, 40, 60, 35, 70, 50, 80],
    label := "Revenue",
    style := Style.fgColor Color.green },
  { data := [10, 25, 35, 30, 45, 40, 55, 50, 65, 60],
    label := "Costs",
    style := Style.fgColor Color.red }
]

def generateSparklineValue (tick : Nat) (offset : Float) : Float :=
  let t := tick.toFloat * 0.2 + offset
  50 + 40 * Float.sin t

def draw (frame : Frame) (state : ChartsState) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  -- Main layout: tabs at top, content below
  let mainSections := vsplit area [.fixed 3, .fill, .fixed 1]

  -- Tab bar
  if h : 0 < mainSections.length then
    let tabArea := mainSections[0]
    let tabs := Tabs.new tabTitles
      |>.withSelected state.activeTab
      |>.withSelectedStyle (Style.bold.withFg Color.yellow)
      |>.withBlock (Block.single.withBorderStyle (Style.fgColor Color.blue))
    f := f.render tabs tabArea

  -- Content area
  if h : 1 < mainSections.length then
    let contentArea := mainSections[1]

    match state.activeTab with
    | 0 => -- Sparklines tab
      let sparkSections := vsplit contentArea [.fixed 5, .fixed 5, .fill]

      -- CPU Sparkline
      if hp : 0 < sparkSections.length then
        let cpuArea := sparkSections[0]
        let spark := Sparkline.new state.sparklineData
          |>.withStyle (Style.fgColor Color.green)
          |>.withBlock (Block.rounded.withTitle "CPU Usage" |>.withBorderStyle (Style.fgColor Color.green))
        f := f.render spark cpuArea

      -- Memory Sparkline
      if hp : 1 < sparkSections.length then
        let memArea := sparkSections[1]
        let spark := Sparkline.new state.sparklineData2
          |>.withStyle (Style.fgColor Color.cyan)
          |>.withBlock (Block.rounded.withTitle "Memory Usage" |>.withBorderStyle (Style.fgColor Color.cyan))
        f := f.render spark memArea

      -- Info text
      if hp : 2 < sparkSections.length then
        let infoArea := sparkSections[2]
        let info := Paragraph.fromLines [
          "Sparklines show real-time data as vertical bars.",
          "Each character represents one data point using Unicode blocks.",
          s!"Current tick: {state.tick}"
        ] |>.withStyle Style.dim
          |>.withBlock (Block.single.withTitle "Info")
        f := f.render info infoArea

    | 1 => -- Bar Chart tab
      let panels := hsplit contentArea [.percent 50, .fill]

      -- Vertical bar chart
      if hp : 0 < panels.length then
        let leftArea := panels[0]
        let chart := BarChart.new barChartData
          |>.withOrientation .vertical
          |>.withBarWidth 3
          |>.withBlock (Block.rounded.withTitle "Weekly Sales (Vertical)" |>.withBorderStyle (Style.fgColor Color.magenta))
        f := f.render chart leftArea

      -- Horizontal bar chart
      if hp : 1 < panels.length then
        let rightArea := panels[1]
        let chart := BarChart.new barChartData
          |>.withOrientation .horizontal
          |>.withBarWidth 1
          |>.withBlock (Block.rounded.withTitle "Weekly Sales (Horizontal)" |>.withBorderStyle (Style.fgColor Color.yellow))
        f := f.render chart rightArea

    | _ => -- Line Chart tab
      let chart := LineChart.new lineChartSeries
        |>.withXLabels ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"]
        |>.withYRange 0 100
        |>.withShowGrid true
        |>.withBlock (Block.double.withTitle "Revenue vs Costs" |>.withBorderStyle (Style.fgColor Color.cyan))
      f := f.render chart contentArea

  -- Status bar
  if h : 2 < mainSections.length then
    let statusArea := mainSections[2]
    let statusText := "Tab: Switch view | q: Quit"
    f := f.writeString (statusArea.x + 1) statusArea.y statusText Style.dim

  f

def update (state : ChartsState) (key : KeyEvent) : ChartsState × Bool :=
  match key.code with
  | .char 'q' => (state, true)
  | .tab =>
    ({ state with activeTab := (state.activeTab + 1) % 3 }, false)
  | .char '1' => ({ state with activeTab := 0 }, false)
  | .char '2' => ({ state with activeTab := 1 }, false)
  | .char '3' => ({ state with activeTab := 2 }, false)
  | .left =>
    ({ state with activeTab := if state.activeTab == 0 then 2 else state.activeTab - 1 }, false)
  | .right =>
    ({ state with activeTab := (state.activeTab + 1) % 3 }, false)
  | _ =>
    if key.isCtrlC || key.isCtrlQ then (state, true)
    else
      -- Update sparkline data on any other key (simulates real-time updates)
      let newTick := state.tick + 1
      let newValue1 := generateSparklineValue newTick 0
      let newValue2 := generateSparklineValue newTick 2.5

      let newData1 := (state.sparklineData ++ [newValue1]).drop (if state.sparklineData.length >= 50 then 1 else 0)
      let newData2 := (state.sparklineData2 ++ [newValue2]).drop (if state.sparklineData2.length >= 50 then 1 else 0)

      ({ state with
        tick := newTick,
        sparklineData := newData1,
        sparklineData2 := newData2
      }, false)

def main : IO Unit := do
  -- Initialize with some sparkline data
  let initialData := List.range 30 |>.map fun i => generateSparklineValue i 0
  let initialData2 := List.range 30 |>.map fun i => generateSparklineValue i 2.5

  let initialState : ChartsState := {
    sparklineData := initialData,
    sparklineData2 := initialData2,
    tick := 30
  }
  App.runApp initialState draw update
