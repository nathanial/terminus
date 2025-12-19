-- KitchenSink: Combined demo showcasing all example widgets
-- Use F1-F11 or Tab/Shift+Tab to switch demos, q/Esc to quit

import Terminus

open Terminus

namespace ChartsDemo

structure State where
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

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainSections := vsplit area [.fixed 3, .fill, .fixed 1]

  if h : 0 < mainSections.length then
    let tabArea := mainSections[0]
    let tabs := Tabs.new tabTitles
      |>.withSelected state.activeTab
      |>.withSelectedStyle (Style.bold.withFg Color.yellow)
      |>.withBlock (Block.single.withBorderStyle (Style.fgColor Color.blue))
    f := f.render tabs tabArea

  if h : 1 < mainSections.length then
    let contentArea := mainSections[1]

    match state.activeTab with
    | 0 =>
      let sparkSections := vsplit contentArea [.fixed 5, .fixed 5, .fill]

      if hp : 0 < sparkSections.length then
        let cpuArea := sparkSections[0]
        let spark := Sparkline.new state.sparklineData
          |>.withStyle (Style.fgColor Color.green)
          |>.withBlock (Block.rounded.withTitle "CPU Usage" |>.withBorderStyle (Style.fgColor Color.green))
        f := f.render spark cpuArea

      if hp : 1 < sparkSections.length then
        let memArea := sparkSections[1]
        let spark := Sparkline.new state.sparklineData2
          |>.withStyle (Style.fgColor Color.cyan)
          |>.withBlock (Block.rounded.withTitle "Memory Usage" |>.withBorderStyle (Style.fgColor Color.cyan))
        f := f.render spark memArea

      if hp : 2 < sparkSections.length then
        let infoArea := sparkSections[2]
        let info := Paragraph.fromLines [
          "Sparklines show real-time data as vertical bars.",
          "Each character represents one data point using Unicode blocks.",
          s!"Current tick: {state.tick}"
        ] |>.withStyle Style.dim
          |>.withBlock (Block.single.withTitle "Info")
        f := f.render info infoArea

    | 1 =>
      let panels := hsplit contentArea [.percent 50, .fill]

      if hp : 0 < panels.length then
        let leftArea := panels[0]
        let chart := BarChart.new barChartData
          |>.withOrientation .vertical
          |>.withBarWidth 3
          |>.withBlock (Block.rounded.withTitle "Weekly Sales (Vertical)" |>.withBorderStyle (Style.fgColor Color.magenta))
        f := f.render chart leftArea

      if hp : 1 < panels.length then
        let rightArea := panels[1]
        let chart := BarChart.new barChartData
          |>.withOrientation .horizontal
          |>.withBarWidth 1
          |>.withBlock (Block.rounded.withTitle "Weekly Sales (Horizontal)" |>.withBorderStyle (Style.fgColor Color.yellow))
        f := f.render chart rightArea

    | _ =>
      let chart := LineChart.new lineChartSeries
        |>.withXLabels ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct"]
        |>.withYRange 0 100
        |>.withShowGrid true
        |>.withBlock (Block.double.withTitle "Revenue vs Costs" |>.withBorderStyle (Style.fgColor Color.cyan))
      f := f.render chart contentArea

  if h : 2 < mainSections.length then
    let statusArea := mainSections[2]
    let statusText := "Tab: Switch view | q: Quit"
    f := f.writeString (statusArea.x + 1) statusArea.y statusText Style.dim

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  let newTick := state.tick + 1
  let newValue1 := generateSparklineValue newTick 0
  let newValue2 := generateSparklineValue newTick 2.5
  let newData1 := (state.sparklineData ++ [newValue1]).drop (if state.sparklineData.length >= 50 then 1 else 0)
  let newData2 := (state.sparklineData2 ++ [newValue2]).drop (if state.sparklineData2.length >= 50 then 1 else 0)
  let state := { state with
    tick := newTick,
    sparklineData := newData1,
    sparklineData2 := newData2
  }

  match key with
  | none => (state, false)
  | some k =>
    match k.code with
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
      if k.isCtrlC || k.isCtrlQ then (state, true)
      else (state, false)

def init : State :=
  let initialData := List.range 30 |>.map fun i => generateSparklineValue i 0
  let initialData2 := List.range 30 |>.map fun i => generateSparklineValue i 2.5
  { sparklineData := initialData, sparklineData2 := initialData2, tick := 30 }

end ChartsDemo

namespace CounterDemo

structure State where
  count : Int := 0
  maxCount : Int := 100
  deriving Inhabited

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainBlock := Block.double
    |>.withTitle "Counter Demo"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)

  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  let sections := vsplit inner [.fixed 3, .fixed 3, .fixed 1, .fill]

  if h : 0 < sections.length then
    let counterArea := sections[0]
    let counterText := s!"Count: {state.count}"
    let counterStyle := if state.count >= 0 then Style.bold.withFg Color.green
                        else Style.bold.withFg Color.red
    let counterBlock := Block.single.withTitle "Value"
    let counter := Paragraph.fromString counterText
      |>.centered
      |>.withStyle counterStyle
      |>.withBlock counterBlock
    f := f.render counter counterArea

  if h : 1 < sections.length then
    let gaugeArea := sections[1]
    let ratio := if state.maxCount == 0 then 0.0
                 else Float.ofInt (max 0 state.count) / Float.ofInt state.maxCount
    let gauge := Gauge.new ratio
      |>.withLabel "Progress"
      |>.withFilledStyle (Style.fgColor Color.green)
      |>.withUnfilledStyle (Style.fgColor Color.white)
      |>.withBlock (Block.single.withTitle "Gauge")
    f := f.render gauge gaugeArea

  if h : 3 < sections.length then
    let instrArea := sections[3]
    let instructions := Paragraph.fromLines [
      "Controls:",
      "  Up/+ : Increase count",
      "  Down/- : Decrease count",
      "  r   : Reset to 0",
      "  q   : Quit"
    ] |>.withStyle Style.dim
      |>.withBlock (Block.rounded.withTitle "Help")
    f := f.render instructions instrArea

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    match k.code with
    | .char 'q' => (state, true)
    | .char '+' | .up => ({ state with count := min (state.count + 1) state.maxCount }, false)
    | .char '-' | .down => ({ state with count := state.count - 1 }, false)
    | .char 'r' => ({ state with count := 0 }, false)
    | _ =>
      if k.isCtrlC || k.isCtrlQ then (state, true)
      else (state, false)

end CounterDemo

namespace DashboardDemo

structure State where
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

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainSections := vsplit area [.fixed 3, .fill]

  if h : 0 < mainSections.length then
    let tabArea := mainSections[0]
    let tabs := Tabs.new tabTitles
      |>.withSelected state.activeTab
      |>.withSelectedStyle (Style.bold.withFg Color.yellow)
      |>.withBlock (Block.single.withBorderStyle (Style.fgColor Color.blue))
    f := f.render tabs tabArea

  if h : 1 < mainSections.length then
    let contentArea := mainSections[1]

    match state.activeTab with
    | 0 =>
      let panels := hsplit contentArea [.percent 40, .fill]

      if hp : 0 < panels.length then
        let leftArea := panels[0]
        let list := ListWidget.new listItems
          |>.withSelected state.selectedItem
          |>.withHighlight { style := Style.reversed.withFg Color.cyan, symbol := "> " }
          |>.withBlock (Block.rounded.withTitle "Menu" |>.withBorderStyle (Style.fgColor Color.green))
        f := f.render list leftArea

      if hp : 1 < panels.length then
        let rightArea := panels[1]
        let gaugeBlock := Block.rounded.withTitle "System Resources" |>.withBorderStyle (Style.fgColor Color.magenta)
        f := f.render gaugeBlock rightArea

        let inner := gaugeBlock.innerArea rightArea
        let gaugeSections := vsplit inner [.fixed 2, .fixed 2, .fixed 2, .fixed 2, .fill]

        if hg : 0 < gaugeSections.length then
          let cpuGauge := Gauge.new state.cpuUsage
            |>.withLabel "CPU"
            |>.withFilledStyle (Style.fgColor Color.green)
          f := f.render cpuGauge gaugeSections[0]

        if hg : 1 < gaugeSections.length then
          let memGauge := Gauge.new state.memUsage
            |>.withLabel "Memory"
            |>.withFilledStyle (Style.fgColor Color.yellow)
          f := f.render memGauge gaugeSections[1]

        if hg : 2 < gaugeSections.length then
          let diskGauge := Gauge.new state.diskUsage
            |>.withLabel "Disk"
            |>.withFilledStyle (Style.fgColor Color.cyan)
          f := f.render diskGauge gaugeSections[2]

        if hg : 3 < gaugeSections.length then
          let netGauge := LineGauge.new state.netUsage
            |>.withLabel "Network"
            |>.withFilledStyle (Style.fgColor Color.magenta)
            |>.withUnfilledStyle (Style.fgColor Color.gray)
            |>.withShowPercent
          f := f.render netGauge gaugeSections[3]

    | 1 =>
      let table := Table.new tableData
        |>.withHeader ["Name", "PID", "CPU", "Memory"]
        |>.withWidths [.ratio 2, .ratio 1, .ratio 1, .ratio 1]
        |>.withSelected state.selectedRow
        |>.withHeaderStyle (Style.bold.withFg Color.cyan)
        |>.withSelectedStyle Style.reversed
        |>.withBlock (Block.single.withTitle "Processes" |>.withBorderStyle (Style.fgColor Color.blue))
      f := f.render table contentArea

    | _ =>
      let clearBg := Clear.new.withBg Color.black
      f := f.render clearBg contentArea

      let netBlock := Block.double.withTitle "Network" |>.withBorderStyle (Style.fgColor Color.yellow)
      f := f.render netBlock contentArea
      let inner := netBlock.innerArea contentArea

      let rows := vsplit inner [.fixed 1, .fixed 1, .fixed 1, .fixed 1, .fixed 1, .fixed 1, .fill]

      if hr : 0 < rows.length then
        let spinner := Spinner.dots.withFrame (state.tick / 4)
          |>.withLabel "Connecting to server..."
          |>.withStyle (Style.fgColor Color.cyan)
        f := f.render spinner rows[0]

      if hr : 1 < rows.length then
        let spinner := Spinner.line.withFrame (state.tick / 6)
          |>.withLabel "Fetching network stats..."
          |>.withStyle (Style.fgColor Color.green)
        f := f.render spinner rows[1]

      if hr : 2 < rows.length then
        let spinner := Spinner.arc.withFrame (state.tick / 5)
          |>.withLabel "Scanning ports..."
          |>.withStyle (Style.fgColor Color.magenta)
        f := f.render spinner rows[2]

      if hr : 3 < rows.length then
        let spinner := Spinner.arrows.withFrame (state.tick / 4)
          |>.withLabel "Downloading updates..."
          |>.withStyle (Style.fgColor Color.yellow)
        f := f.render spinner rows[3]

      if hr : 4 < rows.length then
        let spinner := Spinner.blocks.withFrame (state.tick / 5)
          |>.withLabel "Processing packets..."
          |>.withStyle (Style.fgColor Color.blue)
        f := f.render spinner rows[4]

      if hr : 5 < rows.length then
        let spinner := Spinner.growing.withFrame (state.tick / 3)
          |>.withLabel "Loading..."
          |>.withStyle (Style.fgColor Color.white)
        f := f.render spinner rows[5]

  let statusY := area.height - 1
  let statusText := s!"Tab: {state.activeTab + 1}/3 | Item: {state.selectedItem + 1} | Press 'q' to quit"
  f := f.writeString 1 statusY statusText Style.dim

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  let state := { state with
    tick := state.tick + 1
    cpuUsage := 0.3 + 0.4 * (Float.sin (state.tick.toFloat * 0.1)).abs
    memUsage := 0.6 + 0.2 * (Float.cos (state.tick.toFloat * 0.05)).abs
    netUsage := 0.2 + 0.6 * (Float.sin (state.tick.toFloat * 0.15)).abs
  }

  match key with
  | none => (state, false)
  | some k =>
    match k.code with
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
      if k.isCtrlC || k.isCtrlQ then (state, true)
      else (state, false)

end DashboardDemo

namespace FileExplorerDemo

/-- Sample file tree structure -/
def sampleTree : List TreeNode := [
  .branch "src" [
    .branch "components" [
      .leaf "Button.tsx",
      .leaf "Input.tsx",
      .leaf "Modal.tsx",
      .leaf "Dropdown.tsx",
      .leaf "Table.tsx"
    ] true,
    .branch "utils" [
      .leaf "format.ts",
      .leaf "validate.ts",
      .leaf "api.ts"
    ] false,
    .branch "hooks" [
      .leaf "useAuth.ts",
      .leaf "useForm.ts",
      .leaf "useQuery.ts"
    ] true,
    .leaf "index.ts",
    .leaf "App.tsx"
  ] true,
  .branch "tests" [
    .leaf "Button.test.tsx",
    .leaf "Input.test.tsx",
    .leaf "utils.test.ts"
  ] false,
  .branch "public" [
    .leaf "index.html",
    .leaf "favicon.ico"
  ] true,
  .leaf "package.json",
  .leaf "tsconfig.json",
  .leaf "README.md"
]

structure State where
  tree : Tree := Tree.new sampleTree
  showPopup : Bool := false
  confirmPopup : ConfirmPopup := ConfirmPopup.new "Delete this file?"
  deriving Inhabited

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainBlock := Block.double
    |>.withTitle "File Explorer"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  let sections := hsplit inner [.fill, .fixed 1]

  if h : 0 < sections.length then
    let treeArea := sections[0]
    let treeWidget := state.tree
      |>.withSelectedStyle (Style.reversed.withFg Color.cyan)
      |>.withBranchStyle (Style.bold.withFg Color.yellow)
      |>.withLeafStyle (Style.fgColor Color.white)
      |>.withPrefixStyle (Style.fgColor Color.gray)
    f := f.render treeWidget treeArea

  if h : 1 < sections.length then
    let scrollArea := sections[1]
    let visibleLines := state.tree.visibleLines
    let scrollbar := Scrollbar.vertical
        state.tree.selected
        visibleLines.length
        (inner.height - 2)
      |>.withThumbStyle (Style.fgColor Color.cyan)
      |>.withTrackStyle Style.dim
    f := f.render scrollbar scrollArea

  let statusY := area.y + area.height - 1
  let statusText := if state.showPopup then
    "Y: Confirm | N: Cancel"
  else
    "↑↓: Navigate | Enter: Toggle | d: Delete | q: Quit"
  f := f.writeString (area.x + 2) statusY statusText Style.dim

  match state.tree.getSelected with
  | some line =>
    let infoText := s!"Selected: {line.label}"
    let infoX := area.x + area.width - infoText.length - 2
    f := f.writeString infoX statusY infoText (Style.dim.withFg Color.cyan)
  | none => pure ()

  if state.showPopup then
    f := f.render state.confirmPopup area

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    if state.showPopup then
      match k.code with
      | .char 'y' | .char 'Y' | .enter =>
        if state.confirmPopup.selectedYes then
          ({ state with showPopup := false }, false)
        else
          ({ state with showPopup := false }, false)
      | .char 'n' | .char 'N' | .escape =>
        ({ state with showPopup := false }, false)
      | .left | .right | .tab =>
        ({ state with confirmPopup := state.confirmPopup.toggle }, false)
      | _ => (state, false)
    else
      match k.code with
      | .char 'q' => (state, true)
      | .up =>
        ({ state with tree := state.tree.selectPrev }, false)
      | .down =>
        ({ state with tree := state.tree.selectNext }, false)
      | .enter | .char ' ' =>
        ({ state with tree := state.tree.toggleSelected }, false)
      | .char 'd' | .char 'D' =>
        match state.tree.getSelected with
        | some line =>
          let popup := ConfirmPopup.new s!"Delete '{line.label}'?"
            |>.withYesLabel "Delete"
            |>.withNoLabel "Cancel"
            |>.selectNo
          ({ state with showPopup := true, confirmPopup := popup }, false)
        | none => (state, false)
      | .left =>
        match state.tree.getSelected with
        | some line =>
          if !line.isLeaf && line.isExpanded then
            ({ state with tree := state.tree.toggleSelected }, false)
          else
            (state, false)
        | none => (state, false)
      | .right =>
        match state.tree.getSelected with
        | some line =>
          if !line.isLeaf && !line.isExpanded then
            ({ state with tree := state.tree.toggleSelected }, false)
          else
            (state, false)
        | none => (state, false)
      | _ =>
        if k.isCtrlC || k.isCtrlQ then (state, true)
        else (state, false)

end FileExplorerDemo

namespace TextEditorDemo

inductive Focus where
  | filename
  | content
  | preview
  | calendar
  deriving BEq, Inhabited

structure State where
  filename : TextInput := TextInput.new.withValue "untitled.txt"
  content : TextArea := TextArea.fromLines [
    "Hello, world!",
    "",
    "This is a simple text editor demo.",
    "Use Tab to switch between widgets.",
    "",
    "Features:",
    "- Single-line text input (filename)",
    "- Multi-line text area (content)",
    "- Calendar widget (date picker)",
    "",
    "Try typing in each field!"
  ] |>.showNumbers
  calendar : Calendar := Calendar.new 2025 1 |>.withSelectedDay 15
  focus : Focus := .content
  previewOffsetX : Nat := 0
  previewOffsetY : Nat := 0
  deriving Inhabited

private def previewLines (state : State) : List String := Id.run do
  let mut i : Nat := 0
  let mut out : List String := []
  for line in state.content.lines.toList do
    let num := s!"{i + 1}"
    let padLen := if num.length >= 4 then 0 else 4 - num.length
    let pad := String.ofList (List.replicate padLen ' ')
    out := out ++ [s!"{pad}{num} │ {line}"]
    i := i + 1
  out

private def maxLineLen (lines : List String) : Nat :=
  lines.foldl (fun acc s => Nat.max acc s.length) 0

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainBlock := Block.double
    |>.withTitle "Simple Editor"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  let sections := vsplit inner [.fixed 3, .fill, .fixed 1]

  if h : 0 < sections.length then
    let filenameArea := sections[0]
    let isFocused := state.focus == .filename
    let borderStyle := if isFocused then Style.fgColor Color.yellow else Style.fgColor Color.white
    let input := { state.filename with focused := isFocused }
      |>.withPlaceholder "Enter filename..."
      |>.withBlock (Block.rounded.withTitle "Filename" |>.withBorderStyle borderStyle)
    f := f.render input filenameArea

  if h : 1 < sections.length then
    let contentSection := sections[1]
    let panels := hsplit contentSection [.fill, .fixed 32]

    if hp : 0 < panels.length then
      let textArea := panels[0]
      let isFocused := state.focus == .content
      let borderStyle := if isFocused then Style.fgColor Color.yellow else Style.fgColor Color.white
      let editor := { state.content with focused := isFocused }
        |>.withCursorStyle Style.reversed
        |>.withLineNumberStyle (Style.dim.withFg Color.cyan)
        |>.withBlock (Block.rounded.withTitle "Content" |>.withBorderStyle borderStyle)
      f := f.render editor textArea

    if hp : 1 < panels.length then
      let rightArea := panels[1]
      let rightSections := (Layout.vertical [.fill, .fixed 10])
        |>.withSpacing 1
        |>.split rightArea

      if hr : 0 < rightSections.length then
        let previewOuter := rightSections[0]
        let isFocused := state.focus == .preview
        let borderStyle := if isFocused then Style.fgColor Color.yellow else Style.fgColor Color.white
        let previewBlock := Block.rounded.withTitle "Preview (ScrollView)" |>.withBorderStyle borderStyle
        f := f.render previewBlock previewOuter
        let previewInner := previewBlock.innerArea previewOuter
        if !previewInner.isEmpty then
          let previewPanels := hsplit previewInner [.fill, .fixed 1]
          if hp2 : 0 < previewPanels.length then
            let scrollArea := previewPanels[0]
            let lines := previewLines state
            let contentW := maxLineLen lines
            let contentH := lines.length
            let para := Paragraph.fromLines lines |>.withStyle Style.dim
            let view := (ScrollView.new para)
              |>.withContentSize contentW contentH
              |>.withOffset state.previewOffsetX state.previewOffsetY
            f := f.render view scrollArea

            if hp2b : 1 < previewPanels.length then
              let barArea := previewPanels[1]
              let scroll := Scrollbar.vertical state.previewOffsetY contentH scrollArea.height
                |>.withThumbStyle (Style.fgColor Color.cyan)
                |>.withTrackStyle Style.dim
              f := f.render scroll barArea

      if hr : 1 < rightSections.length then
        let calendarArea := rightSections[1]
        let isFocused := state.focus == .calendar
        let borderStyle := if isFocused then Style.fgColor Color.yellow else Style.fgColor Color.white
        let cal := state.calendar
          |>.withSelectedStyle (if isFocused then Style.reversed else Style.bold)
          |>.withBlock (Block.rounded.withTitle "Date" |>.withBorderStyle borderStyle)
        f := f.render cal calendarArea

  if h : 2 < sections.length then
    let statusArea := sections[2]
    let focusName := match state.focus with
      | .filename => "Filename"
      | .content => "Content"
      | .preview => "Preview"
      | .calendar => "Calendar"

    let statusLeft := s!"Focus: {focusName}"
    let statusRight := s!"Ln {state.content.cursorRow + 1}, Col {state.content.cursorCol + 1}"
    let statusMiddle := "Tab: Switch | Esc: Quit | Preview: arrows/pgup/pgdn/home/end"

    f := f.writeString statusArea.x statusArea.y statusLeft (Style.dim.withFg Color.cyan)
    let middleX := statusArea.x + (statusArea.width - statusMiddle.length) / 2
    f := f.writeString middleX statusArea.y statusMiddle Style.dim
    let rightX := statusArea.x + statusArea.width - statusRight.length
    f := f.writeString rightX statusArea.y statusRight (Style.dim.withFg Color.green)

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    if k.code == .escape then (state, true)
    else match k.code with
    | .tab =>
      let nextFocus := match state.focus with
        | .filename => .content
        | .content => .preview
        | .preview => .calendar
        | .calendar => .filename
      ({ state with focus := nextFocus }, false)
    | _ =>
      match state.focus with
      | .filename =>
        let newFilename := state.filename.handleKey k
        ({ state with filename := newFilename }, false)
      | .content =>
        let newContent := state.content.handleKey k
        ({ state with content := newContent }, false)
      | .preview =>
        let maxY := if state.content.lines.size == 0 then 0 else state.content.lines.size - 1
        match k.code with
        | .up => ({ state with previewOffsetY := state.previewOffsetY - 1 }, false)
        | .down => ({ state with previewOffsetY := state.previewOffsetY + 1 }, false)
        | .left => ({ state with previewOffsetX := state.previewOffsetX - 1 }, false)
        | .right => ({ state with previewOffsetX := state.previewOffsetX + 1 }, false)
        | .pageUp => ({ state with previewOffsetY := state.previewOffsetY - 10 }, false)
        | .pageDown => ({ state with previewOffsetY := state.previewOffsetY + 10 }, false)
        | .home => ({ state with previewOffsetX := 0, previewOffsetY := 0 }, false)
        | .«end» => ({ state with previewOffsetY := maxY }, false)
        | _ => (state, false)
      | .calendar =>
        let newCalendar := match k.code with
          | .left => state.calendar.prevDay
          | .right => state.calendar.nextDay
          | .up => state.calendar.prevWeek
          | .down => state.calendar.nextWeek
          | .char 'h' => state.calendar.prevMonth
          | .char 'l' => state.calendar.nextMonth
          | _ => state.calendar
        ({ state with calendar := newCalendar }, false)

end TextEditorDemo

namespace MenuDemo

def menuItems : List MenuItem := [
  (MenuItem.new "File").withSubmenu [
    (MenuItem.new "New").withHotkey "Ctrl+N",
    (MenuItem.new "Open").withHotkey "Ctrl+O",
    MenuItem.separator,
    (MenuItem.new "Save").withHotkey "Ctrl+S",
    (MenuItem.new "Quit").withHotkey "Ctrl+Q"
  ],
  (MenuItem.new "Edit").withSubmenu [
    (MenuItem.new "Undo").withHotkey "Ctrl+Z",
    (MenuItem.new "Redo").withHotkey "Ctrl+Y",
    MenuItem.separator,
    (MenuItem.disabled "Cut").withHotkey "Ctrl+X",
    (MenuItem.new "Copy").withHotkey "Ctrl+C",
    (MenuItem.new "Paste").withHotkey "Ctrl+V"
  ],
  (MenuItem.new "View").withSubmenu [
    (MenuItem.new "Zoom").withSubmenu [
      (MenuItem.new "Zoom In").withHotkey "Ctrl+Plus",
      (MenuItem.new "Zoom Out").withHotkey "Ctrl+-",
      (MenuItem.new "Reset").withHotkey "Ctrl+0"
    ],
    (MenuItem.new "Fullscreen").withHotkey "F11"
  ],
  (MenuItem.new "Help").withSubmenu [
    (MenuItem.new "Documentation"),
    (MenuItem.new "About")
  ]
]

structure State where
  path : List Nat := [0]
  deriving Inhabited

def dropLast : List Nat → List Nat
  | [] => []
  | [_] => []
  | x :: xs => x :: dropLast xs

def lastIndex : List Nat → Nat
  | [] => 0
  | [x] => x
  | _ :: xs => lastIndex xs

def setLast : List Nat → Nat → List Nat
  | [], n => [n]
  | [_], n => [n]
  | x :: xs, n => x :: setLast xs n

def itemsAtPath (items : List MenuItem) : List Nat → List MenuItem
  | [] => items
  | idx :: rest =>
    match items[idx]? with
    | some item => itemsAtPath item.submenu rest
    | none => []

def currentItems (state : State) : List MenuItem :=
  itemsAtPath menuItems (dropLast state.path)

def currentIndex (state : State) : Nat :=
  lastIndex state.path

def isSelectable (item : MenuItem) : Bool :=
  !item.isSeparator

partial def moveIndex (items : List MenuItem) (idx : Nat) (forward : Bool) (fuel : Nat) : Nat :=
  if fuel == 0 || items.isEmpty then idx
  else
    let len := items.length
    let idx := min idx (len - 1)
    let next := if forward then
      if idx + 1 < len then idx + 1 else idx
    else
      if idx == 0 then 0 else idx - 1
    let item := items.getD next MenuItem.separator
    if isSelectable item then next else moveIndex items next forward (fuel - 1)

def moveSelection (state : State) (forward : Bool) : State :=
  let items := currentItems state
  let idx := currentIndex state
  let newIdx := moveIndex items idx forward items.length
  { state with path := setLast state.path newIdx }

def openSubmenu (state : State) : State :=
  let items := currentItems state
  let idx := currentIndex state
  let item := items.getD idx MenuItem.separator
  if item.hasSubmenu then
    { state with path := state.path ++ [0] }
  else
    state

def closeSubmenu (state : State) : State :=
  if state.path.length <= 1 then state else { state with path := dropLast state.path }

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainBlock := Block.double
    |>.withTitle "Menu"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  let menu := Menu.new menuItems
    |>.withSelectedPath state.path
    |>.withOpenOnSelect false
    |>.withBlock Block.single
    |>.withSubmenuBlock (some Block.single)
    |>.withHighlightStyle Style.reversed
    |>.withIndicator "▶"

  f := f.render menu inner

  let help := "↑↓: Move  →/Enter: Open  ←/Backspace: Close"
  if inner.height > 1 then
    let helpY := inner.y + inner.height - 1
    let helpX := inner.x
    f := f.writeString helpX helpY help Style.dim

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    match k.code with
    | .char 'q' => (state, true)
    | .up => (moveSelection state false, false)
    | .down => (moveSelection state true, false)
    | .right | .enter => (openSubmenu state, false)
    | .left | .backspace => (closeSubmenu state, false)
    | _ =>
      if k.isCtrlC || k.isCtrlQ then (state, true)
      else (state, false)

end MenuDemo

namespace ScrollViewDemo

structure State where
  offsetX : Nat := 0
  offsetY : Nat := 0
  deriving Inhabited

private def maxLineLen (lines : List String) : Nat :=
  lines.foldl (fun acc s => Nat.max acc s.length) 0

private def contentLines : List String :=
  let pattern := String.intercalate "" (List.replicate 12 "0123456789")
  (List.range 200).map fun i =>
    let row := s!"Row {i}"
    s!"{row} │ {pattern} │ {row} end"

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainBlock := Block.double
    |>.withTitle "ScrollView"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  let layout := (Layout.vertical [.fill, .fixed 1]).split inner
  if h : 0 < layout.length then
    let top := layout[0]

    let topPanels := hsplit top [.fill, .fixed 1]
    if hp : 0 < topPanels.length then
      let scrollArea := topPanels[0]
      let vbarArea := if hp2 : 1 < topPanels.length then topPanels[1] else { x := 0, y := 0, width := 0, height := 0 }

      let lines := contentLines
      let contentW := maxLineLen lines
      let contentH := lines.length
      let para := Paragraph.fromLines lines |>.withStyle Style.dim

      let view := (ScrollView.new para)
        |>.withContentSize contentW contentH
        |>.withOffset state.offsetX state.offsetY
      f := f.render view scrollArea

      if !vbarArea.isEmpty then
        let vscroll := Scrollbar.vertical state.offsetY contentH scrollArea.height
          |>.withThumbStyle (Style.fgColor Color.cyan)
          |>.withTrackStyle Style.dim
        f := f.render vscroll vbarArea

      if h2 : 1 < layout.length then
        let bottom := layout[1]
        if !bottom.isEmpty then
          let bottomPanels := hsplit bottom [.fill, .fixed 1]
          if hb : 0 < bottomPanels.length then
            let hbarArea := bottomPanels[0]
            let hscroll := Scrollbar.horizontal state.offsetX contentW scrollArea.width
              |>.withThumbStyle (Style.fgColor Color.cyan)
              |>.withTrackStyle Style.dim
            f := f.render hscroll hbarArea

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    match k.code with
    | .char 'q' => (state, true)
    | .up => ({ state with offsetY := state.offsetY - 1 }, false)
    | .down => ({ state with offsetY := state.offsetY + 1 }, false)
    | .left => ({ state with offsetX := state.offsetX - 1 }, false)
    | .right => ({ state with offsetX := state.offsetX + 1 }, false)
    | .pageUp => ({ state with offsetY := state.offsetY - 10 }, false)
    | .pageDown => ({ state with offsetY := state.offsetY + 10 }, false)
    | .home => ({ state with offsetX := 0, offsetY := 0 }, false)
    | .«end» => ({ state with offsetY := 1000000 }, false)
    | _ =>
      if k.isCtrlC || k.isCtrlQ then (state, true) else (state, false)

end ScrollViewDemo

namespace ControlsDemo

inductive Focus where
  | checkboxes
  | radios
  deriving BEq, Inhabited

structure CheckboxItem where
  label : String
  checked : Bool := false
  deriving Inhabited

structure State where
  focus : Focus := .checkboxes
  checkboxIndex : Nat := 0
  checkboxes : Array CheckboxItem := #[
    { label := "Enable notifications", checked := true },
    { label := "Dark mode", checked := false },
    { label := "Auto-update", checked := true }
  ]
  radios : RadioGroup :=
    RadioGroup.new ["Option A", "Option B", "Option C"]
      |>.withSelected 1
      |>.withSymbols "(•)" "( )"
  deriving Inhabited

def checkboxCount (s : State) : Nat := s.checkboxes.size

def selectCheckboxPrev (s : State) : State :=
  if s.checkboxIndex == 0 then s else { s with checkboxIndex := s.checkboxIndex - 1 }

def selectCheckboxNext (s : State) : State :=
  if s.checkboxIndex + 1 >= checkboxCount s then s else { s with checkboxIndex := s.checkboxIndex + 1 }

def toggleCheckbox (s : State) : State :=
  if h : s.checkboxIndex < s.checkboxes.size then
    let item := s.checkboxes.getD s.checkboxIndex { label := "", checked := false }
    let updated := { item with checked := !item.checked }
    { s with checkboxes := s.checkboxes.set! s.checkboxIndex updated }
  else
    s

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainBlock := Block.double
    |>.withTitle "Form Controls"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  let checkboxHeight := state.checkboxes.size + 2
  let radioHeight := state.radios.options.length + 2
  let sections := (Layout.vertical [.fixed checkboxHeight, .fixed radioHeight, .fill])
    |>.withSpacing 1
    |>.split inner

  if h : 0 < sections.length then
    let checkboxArea := sections[0]
    let borderStyle := if state.focus == .checkboxes then Style.fgColor Color.yellow else Style.fgColor Color.white
    let block := Block.rounded
      |>.withTitle "Checkboxes"
      |>.withBorderStyle borderStyle
    f := f.render block checkboxArea

    let innerCb := block.innerArea checkboxArea
    let mut row := innerCb.y
    let maxRows := min innerCb.height state.checkboxes.size
    for i in [0 : maxRows] do
      let item := state.checkboxes.getD i { label := "", checked := false }
      let isCursor := state.focus == .checkboxes && i == state.checkboxIndex
      let highlight := if isCursor then Style.reversed else {}
      let cb := (Checkbox.new item.label)
        |>.withChecked item.checked
        |>.withSymbols "☑" "☐"
        |>.withLabelStyle highlight
        |>.withSymbolStyle highlight
      f := f.render cb { x := innerCb.x, y := row, width := innerCb.width, height := 1 }
      row := row + 1

  if h : 1 < sections.length then
    let radioArea := sections[1]
    let borderStyle := if state.focus == .radios then Style.fgColor Color.yellow else Style.fgColor Color.white
    let block := Block.rounded
      |>.withTitle "Radio Buttons"
      |>.withBorderStyle borderStyle
    let selectedStyle := if state.focus == .radios then Style.bold.withFg Color.yellow else Style.bold.withFg Color.green
    let radio := state.radios
      |>.withBlock block
      |>.withSelectedStyle selectedStyle
      |>.withUnselectedStyle Style.dim
    f := f.render radio radioArea

  if h : 2 < sections.length then
    let helpArea := sections[2]
    let help := Paragraph.fromLines [
      "Up/Down: Move  Space: Toggle/Select",
      "c: Focus checkboxes  r: Focus radios  q: Quit"
    ] |>.withStyle Style.dim
    f := f.render help helpArea

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    match k.code with
    | .char 'q' => (state, true)
    | .char 'c' => ({ state with focus := .checkboxes }, false)
    | .char 'r' => ({ state with focus := .radios }, false)
    | .up | .char 'k' =>
      match state.focus with
      | .checkboxes => (selectCheckboxPrev state, false)
      | .radios => ({ state with radios := state.radios.selectPrev }, false)
    | .down | .char 'j' =>
      match state.focus with
      | .checkboxes => (selectCheckboxNext state, false)
      | .radios => ({ state with radios := state.radios.selectNext }, false)
    | .space | .enter =>
      match state.focus with
      | .checkboxes => (toggleCheckbox state, false)
      | .radios => ({ state with radios := state.radios.selectNext }, false)
    | _ => (state, false)

end ControlsDemo

namespace LoggerDemo

structure State where
  logger : Logger := (Logger.new
    |>.withBlock (Block.rounded.withTitle "Logs" |>.withBorderStyle (Style.fgColor Color.blue)))
  tick : Nat := 0
  deriving Inhabited

private def pad2 (n : Nat) : String :=
  if n < 10 then s!"0{n}" else s!"{n}"

private def formatTime (seconds : Nat) : String :=
  let s := seconds % 60
  let m := (seconds / 60) % 60
  let h := (seconds / 3600) % 24
  s!"{pad2 h}:{pad2 m}:{pad2 s}"

private def genLevel (tick : Nat) : LogLevel :=
  match tick % 5 with
  | 0 => .info
  | 1 => .debug
  | 2 => .warn
  | 3 => .error
  | _ => .trace

private def genMessage (tick : Nat) : String :=
  match tick % 8 with
  | 0 => "Server started"
  | 1 => "Connection from 10.0.0.42"
  | 2 => "GET /api/v1/items 200"
  | 3 => "Cache miss: user profile"
  | 4 => "Rate limit exceeded"
  | 5 => "Database timeout"
  | 6 => "Retrying request..."
  | _ => s!"Tick {tick}"

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame
  if area.isEmpty then return f

  let sections := vsplit area [.fill, .fixed 1]

  if h : 0 < sections.length then
    let main := sections[0]
    let panels := hsplit main [.fill, .fixed 30]
    if hp : 0 < panels.length then
      f := f.render state.logger panels[0]
    if hp : 1 < panels.length then
      let infoArea := panels[1]
      let minTxt :=
        match state.logger.filter.minLevel with
        | none => "none"
        | some lvl => lvl.label
      let followTxt := if state.logger.follow then "on" else "off"
      let info := Paragraph.fromLines [
        "Controls:",
        "  ↑/↓/PgUp/PgDn : scroll",
        "  Home/End      : top/bottom",
        "  f             : toggle follow",
        "  0-5           : min level",
        "  t             : timestamps",
        "  l             : level tags",
        "  c             : clear",
        "",
        s!"follow: {followTxt}",
        s!"minLevel: {minTxt}"
      ]
        |>.withStyle Style.dim
        |>.withBlock (Block.rounded.withTitle "Help" |>.withBorderStyle (Style.fgColor Color.cyan))
      f := f.render info infoArea

  if h : 1 < sections.length then
    let status := sections[1]
    let text := "Up/Down/PgUp/PgDn scroll | f follow | 0-5 filter | c clear"
    f := f.writeString (status.x + 1) status.y text Style.dim

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  let tick := state.tick + 1
  let state := { state with tick := tick }

  let state :=
    if tick % 18 == 0 then
      let seconds := tick / 60
      let entry := LogEntry.new (genLevel tick) (genMessage tick) (some (formatTime seconds))
      { state with logger := state.logger.add entry }
    else
      state

  match key with
  | none => (state, false)
  | some k =>
    match k.code with
    | .up => ({ state with logger := state.logger.scrollUp }, false)
    | .down => ({ state with logger := state.logger.scrollDown }, false)
    | .pageUp => ({ state with logger := state.logger.scrollUp 10 }, false)
    | .pageDown => ({ state with logger := state.logger.scrollDown 10 }, false)
    | .home => ({ state with logger := state.logger.home }, false)
    | .«end» => ({ state with logger := state.logger.withFollow true }, false)
    | .char 'f' => ({ state with logger := state.logger.toggleFollow }, false)
    | .char 't' => ({ state with logger := state.logger.withShowTimestamp (!state.logger.showTimestamp) }, false)
    | .char 'l' => ({ state with logger := state.logger.withShowLevel (!state.logger.showLevel) }, false)
    | .char 'c' => ({ state with logger := state.logger.clear }, false)
    | .char '0' => ({ state with logger := state.logger.withMinLevel none }, false)
    | .char '1' => ({ state with logger := state.logger.withMinLevel (some .trace) }, false)
    | .char '2' => ({ state with logger := state.logger.withMinLevel (some .debug) }, false)
    | .char '3' => ({ state with logger := state.logger.withMinLevel (some .info) }, false)
    | .char '4' => ({ state with logger := state.logger.withMinLevel (some .warn) }, false)
    | .char '5' => ({ state with logger := state.logger.withMinLevel (some .error) }, false)
    | _ => (state, false)

end LoggerDemo

def demoTitles : Array String :=
  #[
    "Hello",
    "BigText",
    "Charts",
    "Counter",
    "Menu",
    "ScrollView",
    "Controls",
    "Logger",
    "Dashboard",
    "File Explorer",
    "Text Editor"
  ]

def demoCount : Nat := demoTitles.size

structure KitchenState where
  active : Nat := 0
  charts : ChartsDemo.State := ChartsDemo.init
  counter : CounterDemo.State := {}
  menu : MenuDemo.State := {}
  scrollView : ScrollViewDemo.State := {}
  controls : ControlsDemo.State := {}
  logger : LoggerDemo.State := {}
  dashboard : DashboardDemo.State := {}
  explorer : FileExplorerDemo.State := {}
  editor : TextEditorDemo.State := {}
  deriving Inhabited


def clampDemo (idx : Nat) : Nat :=
  if demoCount == 0 then 0 else idx % demoCount


def selectPrev (idx : Nat) : Nat :=
  if demoCount == 0 then 0 else if idx == 0 then demoCount - 1 else idx - 1


def selectNext (idx : Nat) : Nat :=
  if demoCount == 0 then 0 else (idx + 1) % demoCount


def renderSubframe (frame : Frame) (area : Rect) (draw : Frame → α → Frame) (state : α) : Frame := Id.run do
  if area.isEmpty then return frame
  let subArea : Rect := { x := 0, y := 0, width := area.width, height := area.height }
  let subFrame := draw (Frame.new subArea) state
  let merged := frame.buffer.merge subFrame.buffer area.x area.y
  { frame with buffer := merged }


def drawHello (frame : Frame) (_ : Unit) : Frame := Id.run do
  let area := frame.area
  let mut f := frame
  if area.isEmpty then return f

  let blockWidth := min 40 area.width
  let blockHeight := min 5 area.height
  if blockWidth < 2 || blockHeight < 2 then return f

  let blockX := (area.width - blockWidth) / 2
  let blockY := (area.height - blockHeight) / 2

  let block := Block.rounded
    |>.withTitle "Terminus"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)

  let greeting := Paragraph.fromLines [
    "",
    "Hello, Terminus!",
    ""
  ] |>.centered
    |>.withStyle (Style.bold.withFg Color.green)
    |>.withBlock block

  let blockArea : Rect := { x := blockX, y := blockY, width := blockWidth, height := blockHeight }
  f := f.render greeting blockArea

  let subtitle := "KitchenSink demo"
  if area.height >= blockHeight + 2 then
    let subX := if area.width > subtitle.length then (area.width - subtitle.length) / 2 else 0
    let subY := min (area.height - 1) (blockY + blockHeight + 1)
    f := f.writeString subX subY subtitle Style.dim

  f


def drawBigText (frame : Frame) (_ : Unit) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let sections := (Layout.vertical [.fixed 10, .fixed 10, .fixed 6, .fill])
    |>.withSpacing 1
    |>.split area

  if h : 0 < sections.length then
    let block := Block.rounded
      |>.withTitle "Block Font"
      |>.withTitleStyle (Style.bold.withFg Color.cyan)
      |>.withBorderStyle (Style.fgColor Color.blue)
    let big := BigText.new "TERMINUS"
      |>.withFont BigFont.block
      |>.withStyle (Style.bold.withFg Color.cyan)
      |>.withSpacing 0
      |>.withBlock block
    f := f.render big sections[0]

  if h : 1 < sections.length then
    let block := Block.rounded
      |>.withTitle "Slant Font"
      |>.withTitleStyle (Style.bold.withFg Color.magenta)
      |>.withBorderStyle (Style.fgColor Color.magenta)
    let big := BigText.new "BIG TEXT"
      |>.withFont BigFont.slant
      |>.withStyle (Style.fgColor Color.magenta)
      |>.withSpacing 0
      |>.withBlock block
    f := f.render big sections[1]

  if h : 2 < sections.length then
    let block := Block.rounded
      |>.withTitle "Small Font"
      |>.withTitleStyle (Style.bold.withFg Color.green)
      |>.withBorderStyle (Style.fgColor Color.green)
    let big := BigText.new "lean 4"
      |>.withFont BigFont.small
      |>.withStyle (Style.fgColor Color.green)
      |>.withSpacing 0
      |>.withBlock block
    f := f.render big sections[2]

  f


def renderActiveDemo (frame : Frame) (state : KitchenState) (area : Rect) : Frame :=
  match clampDemo state.active with
  | 0 => renderSubframe frame area drawHello ()
  | 1 => renderSubframe frame area drawBigText ()
  | 2 => renderSubframe frame area ChartsDemo.draw state.charts
  | 3 => renderSubframe frame area CounterDemo.draw state.counter
  | 4 => renderSubframe frame area MenuDemo.draw state.menu
  | 5 => renderSubframe frame area ScrollViewDemo.draw state.scrollView
  | 6 => renderSubframe frame area ControlsDemo.draw state.controls
  | 7 => renderSubframe frame area LoggerDemo.draw state.logger
  | 8 => renderSubframe frame area DashboardDemo.draw state.dashboard
  | 9 => renderSubframe frame area FileExplorerDemo.draw state.explorer
  | _ => renderSubframe frame area TextEditorDemo.draw state.editor


def draw (frame : Frame) (state : KitchenState) : Frame := Id.run do
  let area := frame.area
  let mut f := frame
  if area.isEmpty then return f

  let headerHeight :=
    if area.height >= 6 then 4
    else if area.height >= 3 then 3
    else 0

  if headerHeight >= 3 then
    let sections := vsplit area [.fixed headerHeight, .fill]
    if h : 0 < sections.length then
      let headerArea := sections[0]
      let headerBlock := Block.single
        |>.withTitle "Kitchen Sink"
        |>.withTitleStyle (Style.bold.withFg Color.cyan)
        |>.withBorderStyle (Style.fgColor Color.blue)
      let tabs := Tabs.new demoTitles.toList
        |>.withSelected (clampDemo state.active)
        |>.withSelectedStyle (Style.bold.withFg Color.yellow)
        |>.withBlock headerBlock
      f := f.render tabs headerArea

      let inner := headerBlock.innerArea headerArea
      if inner.height > 1 then
        let help := "F1-F11: Switch | Tab/Shift+Tab: Switch | q/Esc: Quit"
        let helpX := inner.x + 1
        let helpY := inner.y + 1
        f := f.writeString helpX helpY help Style.dim

    if h : 1 < sections.length then
      f := renderActiveDemo f state sections[1]
  else
    f := renderActiveDemo f state area

  f


def updateActive (state : KitchenState) (key : Option KeyEvent) : KitchenState × Bool :=
  match clampDemo state.active with
  | 0 =>
    (state, false)
  | 1 =>
    (state, false)
  | 2 =>
    let (charts, quit) := ChartsDemo.update state.charts key
    ({ state with charts := charts }, quit)
  | 3 =>
    let (counter, quit) := CounterDemo.update state.counter key
    ({ state with counter := counter }, quit)
  | 4 =>
    let (menu, quit) := MenuDemo.update state.menu key
    ({ state with menu := menu }, quit)
  | 5 =>
    let (scrollView, quit) := ScrollViewDemo.update state.scrollView key
    ({ state with scrollView := scrollView }, quit)
  | 6 =>
    let (controls, quit) := ControlsDemo.update state.controls key
    ({ state with controls := controls }, quit)
  | 7 =>
    let (logger, quit) := LoggerDemo.update state.logger key
    ({ state with logger := logger }, quit)
  | 8 =>
    let (dashboard, quit) := DashboardDemo.update state.dashboard key
    ({ state with dashboard := dashboard }, quit)
  | 9 =>
    let (explorer, quit) := FileExplorerDemo.update state.explorer key
    ({ state with explorer := explorer }, quit)
  | _ =>
    let (editor, quit) := TextEditorDemo.update state.editor key
    ({ state with editor := editor }, quit)


def handleGlobalKeys (state : KitchenState) (key : Option KeyEvent) : KitchenState × Option KeyEvent × Bool :=
  match key with
  | none => (state, none, false)
  | some k =>
    if k.isCtrlC || k.isCtrlQ || k.code == .escape then
      (state, none, true)
    else
      match k.code with
      | .f n =>
        if n >= 1 && n <= demoCount then
          ({ state with active := n - 1 }, none, false)
        else
          (state, some k, false)
      | .tab =>
        if k.modifiers.shift then
          ({ state with active := selectPrev state.active }, none, false)
        else
          ({ state with active := selectNext state.active }, none, false)
      | _ =>
        (state, some k, false)


def update (state : KitchenState) (key : Option KeyEvent) : KitchenState × Bool :=
  let (state, key, quit) := handleGlobalKeys state key
  if quit then
    (state, true)
  else
    updateActive state key


def main : IO Unit := do
  App.runApp ({} : KitchenState) draw update
