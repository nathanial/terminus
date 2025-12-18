-- Dashboard: Multi-widget layout demo
-- Demonstrates tabs, list, table, gauges in a complex layout

import Terminus

open Terminus

structure DashboardState where
  activeTab : Nat := 0
  selectedItem : Nat := 0
  selectedRow : Nat := 0
  cpuUsage : Float := 0.45
  memUsage : Float := 0.72
  diskUsage : Float := 0.38
  netUsage : Float := 0.55
  tick : Nat := 0
  deriving Inhabited

def tabTitles : List String := ["Overview", "Processes", "Network"]

def listItems : List String := [
  "System Status",
  "Performance",
  "Storage",
  "Network",
  "Security",
  "Updates"
]

def tableData : List (List String) := [
  ["nginx", "1234", "0.5%", "128MB"],
  ["postgres", "2345", "2.1%", "512MB"],
  ["redis", "3456", "0.3%", "64MB"],
  ["node", "4567", "1.8%", "256MB"],
  ["docker", "5678", "3.2%", "1024MB"]
]

def draw (frame : Frame) (state : DashboardState) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  -- Main layout: tabs at top, content below
  let mainSections := vsplit area [.fixed 3, .fill]

  -- Tab bar
  if h : 0 < mainSections.length then
    let tabArea := mainSections[0]
    let tabs := Tabs.new tabTitles
      |>.withSelected state.activeTab
      |>.withSelectedStyle (Style.bold.withFg Color.yellow)
      |>.withBlock (Block.single.withBorderStyle (Style.fgColor Color.blue))
    f := f.render tabs tabArea

  -- Content area based on active tab
  if h : 1 < mainSections.length then
    let contentArea := mainSections[1]

    match state.activeTab with
    | 0 => -- Overview tab
      -- Split into left and right panels
      let panels := hsplit contentArea [.percent 40, .fill]

      -- Left panel: List
      if hp : 0 < panels.length then
        let leftArea := panels[0]
        let list := ListWidget.new listItems
          |>.withSelected state.selectedItem
          |>.withHighlight { style := Style.reversed.withFg Color.cyan, symbol := "> " }
          |>.withBlock (Block.rounded.withTitle "Menu" |>.withBorderStyle (Style.fgColor Color.green))
        f := f.render list leftArea

      -- Right panel: Gauges
      if hp : 1 < panels.length then
        let rightArea := panels[1]
        let gaugeBlock := Block.rounded.withTitle "System Resources" |>.withBorderStyle (Style.fgColor Color.magenta)
        f := f.render gaugeBlock rightArea

        let inner := gaugeBlock.innerArea rightArea
        let gaugeSections := vsplit inner [.fixed 2, .fixed 2, .fixed 2, .fixed 2, .fill]

        -- CPU Gauge
        if hg : 0 < gaugeSections.length then
          let cpuGauge := Gauge.new state.cpuUsage
            |>.withLabel "CPU"
            |>.withFilledStyle (Style.fgColor Color.green)
          f := f.render cpuGauge gaugeSections[0]

        -- Memory Gauge
        if hg : 1 < gaugeSections.length then
          let memGauge := Gauge.new state.memUsage
            |>.withLabel "Memory"
            |>.withFilledStyle (Style.fgColor Color.yellow)
          f := f.render memGauge gaugeSections[1]

        -- Disk Gauge
        if hg : 2 < gaugeSections.length then
          let diskGauge := Gauge.new state.diskUsage
            |>.withLabel "Disk"
            |>.withFilledStyle (Style.fgColor Color.cyan)
          f := f.render diskGauge gaugeSections[2]

        -- Network LineGauge (demonstrates the new widget)
        if hg : 3 < gaugeSections.length then
          let netGauge := LineGauge.new state.netUsage
            |>.withLabel "Network"
            |>.withFilledStyle (Style.fgColor Color.magenta)
            |>.withUnfilledStyle (Style.fgColor Color.gray)
            |>.withShowPercent
          f := f.render netGauge gaugeSections[3]

    | 1 => -- Processes tab
      let table := Table.new tableData
        |>.withHeader ["Name", "PID", "CPU", "Memory"]
        |>.withWidths [.ratio 2, .ratio 1, .ratio 1, .ratio 1]
        |>.withSelected state.selectedRow
        |>.withHeaderStyle (Style.bold.withFg Color.cyan)
        |>.withSelectedStyle Style.reversed
        |>.withBlock (Block.single.withTitle "Processes" |>.withBorderStyle (Style.fgColor Color.blue))
      f := f.render table contentArea

    | _ => -- Network tab (placeholder)
      -- Demonstrate Clear widget: clear area with a dark background before rendering
      let clearBg := Clear.new.withBg Color.black
      f := f.render clearBg contentArea

      let placeholder := Paragraph.fromLines [
        "",
        "Network statistics coming soon...",
        "",
        "This tab demonstrates the Clear widget",
        "which resets an area before rendering.",
        "(Background cleared to black)"
      ] |>.centered
        |>.withStyle (Style.fgColor Color.yellow)
        |>.withBlock (Block.double.withTitle "Network" |>.withBorderStyle (Style.fgColor Color.yellow))
      f := f.render placeholder contentArea

  -- Status bar at the very bottom
  let statusY := area.height - 1
  let statusText := s!"Tab: {state.activeTab + 1}/3 | Item: {state.selectedItem + 1} | Press 'q' to quit"
  f := f.writeString 1 statusY statusText Style.dim

  f

def update (state : DashboardState) (key : KeyEvent) : DashboardState × Bool :=
  match key.code with
  | .char 'q' => (state, true)
  | .tab =>
    ({ state with activeTab := (state.activeTab + 1) % 3 }, false)
  | .left =>
    ({ state with activeTab := if state.activeTab == 0 then 2 else state.activeTab - 1 }, false)
  | .right =>
    ({ state with activeTab := (state.activeTab + 1) % 3 }, false)
  | .up =>
    if state.activeTab == 0 then
      ({ state with selectedItem := if state.selectedItem == 0 then 0 else state.selectedItem - 1 }, false)
    else if state.activeTab == 1 then
      ({ state with selectedRow := if state.selectedRow == 0 then 0 else state.selectedRow - 1 }, false)
    else
      (state, false)
  | .down =>
    if state.activeTab == 0 then
      ({ state with selectedItem := Nat.min (state.selectedItem + 1) (listItems.length - 1) }, false)
    else if state.activeTab == 1 then
      ({ state with selectedRow := Nat.min (state.selectedRow + 1) (tableData.length - 1) }, false)
    else
      (state, false)
  | .char '1' => ({ state with activeTab := 0 }, false)
  | .char '2' => ({ state with activeTab := 1 }, false)
  | .char '3' => ({ state with activeTab := 2 }, false)
  | _ =>
    if key.isCtrlC || key.isCtrlQ then (state, true)
    else
      -- Simulate changing resource usage
      let newState := { state with
        tick := state.tick + 1
        cpuUsage := 0.3 + 0.4 * (Float.sin (state.tick.toFloat * 0.1)).abs
        memUsage := 0.6 + 0.2 * (Float.cos (state.tick.toFloat * 0.05)).abs
        netUsage := 0.2 + 0.6 * (Float.sin (state.tick.toFloat * 0.15)).abs
      }
      (newState, false)

def main : IO Unit := do
  App.runApp ({} : DashboardState) draw update
