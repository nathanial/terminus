# Terminus Widget Porting Plan

This document outlines the plan for porting remaining old-style widgets to the reactive framework.

## Current State

The reactive framework (`Terminus/Reactive/`) has completed phases 1-5 from `REACTIVE_PLAN.md`:

**Completed:**
- Core interactive: `TextInput`, `SelectableList`, `DynSelectableList`, `StringList`, `NumberedList`, `Overlay`, `Modal`
- Navigation: `Tabs`, `Tree`, `DynTree`
- Game support: `Grid`, `Animation` (AnimPhase, interpolation, pulse/cycle helpers)
- Editor: `TextArea`, `Form`, `LabeledInput`, `OptionSelector`
- Advanced: `ScrollView`, `Async`, `Layout`
- Display: `text'`, `dynText'`, headings, themed text, `progressBar'`, conditionals

**Remaining widgets in `Terminus/Widgets/`:**

| Widget | Category | Complexity | Priority |
|--------|----------|------------|----------|
| Checkbox | Input | Low | High |
| RadioGroup | Input | Low | High |
| Spinner | Feedback | Low | High |
| Menu | Navigation | Medium | High |
| Table | Data Display | Medium | High |
| Calendar | Navigation | Medium | Medium |
| Logger | Feedback | Medium | Medium |
| Notification | Feedback | Medium | Medium |
| Popup | Container | Low | Low |
| Scrollbar | Visual | Low | Low |
| Paragraph | Display | Low | Low |
| Gauge | Visualization | Low | Medium |
| LineGauge | Visualization | Low | Low |
| Sparkline | Visualization | Medium | Low |
| BarChart | Visualization | High | Low |
| LineChart | Visualization | High | Low |
| PieChart | Visualization | High | Low |
| Canvas | Drawing | High | Low |
| Image | Media | Medium | Low |
| BigText | Display | Low | Low |
| Clear | Utility | Low | Low |

---

## Conventions & Guardrails

These apply to all phases to keep behavior consistent with existing reactive widgets.

- **State + events:** Use `Reactive.newTriggerEvent` + `Reactive.holdDyn` for state. Avoid mutating `IO.Ref` without firing a trigger. If updates come from `IO` (logger/notifications/canvas), wrap trigger fires in `SpiderEnv.withFrame` (via `SpiderM.getEnv`) so updates land in a consistent frame.
- **Focus & input routing:** Register focusable widgets via `registerComponentW` and gate key handling via `useFocusedInputW` (or `useIsFocused`). If a provided focus name is empty, fall back to the auto-generated widget name.
- **Dynamic collections:** For `dyn*` widgets, clamp selection to bounds when data changes. If data becomes empty, selection becomes `none` and selection events should not fire. Prefer preserving selection by stable identity (label/key) when possible; otherwise preserve index.
- **Normalization & empty states:** Clamp numeric values to documented ranges (e.g., gauges 0..1). Treat NaN/Inf as 0. Render a clear empty state when datasets are empty.
- **Dates:** Reuse `Terminus.Widgets.Calendar` date math or define a small `Date`/`Weekday` type in Terminus. Normalize invalid dates (month 1–12, day 1–numDays).
- **Rendering:** Use `emit` for widgets whose tree is stable and driven by internal refs; use `emitDynamic` when the node tree depends on `Dynamic` values.

## Phase 6: Input Controls

**Goal:** Complete the input widget family with toggles and option selection.

### 6.1 Checkbox Widget

Toggle checkbox with label support.

```lean
structure CheckboxConfig where
  checkedSymbol : String := "[x]"
  uncheckedSymbol : String := "[ ]"
  labelSeparator : String := " "
  style : Style := {}
  checkedStyle : Style := { fg := .ansi .green }
  focusedStyle : Style := { fg := .ansi .cyan }

structure CheckboxResult where
  checked : Dynamic Spider Bool
  onToggle : Event Spider Bool  -- Fires new state

def checkbox' (name : String) (label : String) (initial : Bool := false)
    (config : CheckboxConfig := {}) : WidgetM CheckboxResult
```

**Features:**
- Space/Enter to toggle
- Focus-aware styling
- If `name` is empty, fall back to the auto-generated focus name
- Accessible labeling

### 6.2 RadioGroup Widget

Mutually exclusive option selection.

```lean
structure RadioConfig where
  selectedSymbol : String := "(o)"
  unselectedSymbol : String := "( )"
  labelSeparator : String := " "
  style : Style := {}
  selectedStyle : Style := { fg := .ansi .cyan }
  focusedStyle : Style := {}
  wrapAround : Bool := true
  maxVisible : Option Nat := none
  focusName : String := ""
  globalKeys : Bool := false

structure RadioResult where
  selectedIndex : Dynamic Spider (Option Nat)
  selectedLabel : Dynamic Spider (Option String)
  onSelect : Event Spider Nat

def radioGroup' (name : String) (options : Array String) (initial : Option Nat := none)
    (config : RadioConfig := {}) : WidgetM RadioResult

-- Dynamic version for changing options
def dynRadioGroup' (name : String) (options : Dynamic Spider (Array String))
    (initial : Option Nat := none) (config : RadioConfig := {}) : WidgetM RadioResult
```

**Features:**
- Arrow key navigation
- Enter to confirm selection
- Auto-scroll when exceeding visible height
- Selection clamps on option updates; if options are empty, selection is `none`
- `dynRadioGroup'` should preserve selection by label when possible

### Phase 6 Deliverables

- [ ] `Terminus/Reactive/Checkbox.lean` - Checkbox and RadioGroup widgets
- [ ] `TerminusTests/Reactive/CheckboxTests.lean` - Unit tests
- [ ] Update `examples/ReactiveInput.lean` with demos

---

## Phase 7: Feedback Widgets

**Goal:** Provide visual feedback for loading states and notifications.

### 7.1 Spinner Widget

Animated loading indicator with multiple styles.

```lean
inductive SpinnerStyle where
  | dots      -- Default braille dots
  | line      -- Line rotation
  | arc       -- Arc rotation
  | arrows    -- Arrow rotation
  | blocks    -- Quarter blocks
  | growing   -- Growing bar
  | ascii     -- Simple ASCII

structure SpinnerConfig where
  style : SpinnerStyle := .dots
  textStyle : Style := {}
  labelStyle : Style := { fg := .ansi .brightBlack }

structure SpinnerResult where
  frame : Dynamic Spider String  -- Current frame character

def spinner' (label : Option String := none) (config : SpinnerConfig := {})
    : WidgetM SpinnerResult

-- Spinner that auto-advances with useTick
def animatedSpinner' (label : Option String := none) (intervalMs : Nat := 80)
    (config : SpinnerConfig := {}) : WidgetM SpinnerResult
```

**Features:**
- 7 built-in animation styles
- Integrates with `useTick` for automatic animation
- Optional label display

### 7.2 Logger Widget

Scrolling log display with level-based styling.

```lean
inductive LogLevel where
  | debug | info | warn | error

structure LogEntry where
  level : LogLevel
  message : String
  timestamp : Option String := none

structure LoggerConfig where
  maxLines : Nat := 100
  showTimestamp : Bool := true
  showLevel : Bool := true
  debugStyle : Style := { fg := .ansi .brightBlack }
  infoStyle : Style := {}
  warnStyle : Style := { fg := .ansi .yellow }
  errorStyle : Style := { fg := .ansi .red }

structure LoggerResult where
  entries : Dynamic Spider (Array LogEntry)
  log : LogLevel -> String -> IO Unit
  clear : IO Unit

def logger' (config : LoggerConfig := {}) : WidgetM LoggerResult
```

**Features:**
- Auto-scroll to newest entries
- Level-based coloring
- Max line buffer with truncation
- Timestamp support
- `log`/`clear` are wrappers around trigger events; if called from background tasks, fire via `env.withFrame`

### 7.3 Notification Widget

Toast-style notifications with auto-dismiss.

```lean
-- Reuse `Terminus.NotificationLevel`/`NotificationPosition` from Widgets.Notification
inductive NotificationLevel where
  | info | success | warning | error

structure NotificationConfig where
  durationMs : Nat := 3000  -- Auto-dismiss after
  maxVisible : Nat := 3
  position : NotificationPosition := .topRight
  infoStyle : Style := { fg := .ansi .blue }
  successStyle : Style := { fg := .ansi .green }
  warningStyle : Style := { fg := .ansi .yellow }
  errorStyle : Style := { fg := .ansi .red }

structure NotificationResult where
  show : NotificationLevel -> String -> IO Unit
  dismiss : IO Unit
  dismissAll : IO Unit

def notifications' (config : NotificationConfig := {}) : WidgetM NotificationResult
```

**Features:**
- Multiple notification types with styling
- Auto-dismiss timer
- Stack multiple notifications
- Manual dismiss support
- Use `useTick`/`useElapsedMs` for timing; `show`/`dismiss` should fire trigger events (frame-wrapped)

### Phase 7 Deliverables

- [ ] `Terminus/Reactive/Spinner.lean` - Spinner widget
- [ ] `Terminus/Reactive/Logger.lean` - Logger widget
- [ ] `Terminus/Reactive/Notification.lean` - Notification widget
- [ ] `TerminusTests/Reactive/FeedbackTests.lean` - Unit tests
- [ ] `examples/ReactiveFeedback.lean` - Demo app

---

## Phase 8: Navigation & Data Display

**Goal:** Enable complex navigation patterns and tabular data display.

### 8.1 Menu Widget

Dropdown/context menu with nested submenus.

```lean
structure MenuItem where
  label : String
  shortcut : Option String := none
  disabled : Bool := false
  submenu : Option (Array MenuItem) := none

structure MenuConfig where
  style : Style := {}
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  disabledStyle : Style := { fg := .ansi .brightBlack }
  shortcutStyle : Style := { fg := .ansi .brightBlack }
  borderType : BorderType := .rounded
  focusName : String := ""
  globalKeys : Bool := false

structure MenuResult where
  selectedPath : Dynamic Spider (Array Nat)
  onSelect : Event Spider (Array Nat)  -- Path to selected item
  onCancel : Event Spider Unit

def menu' (items : Array MenuItem) (visible : Dynamic Spider Bool)
    (config : MenuConfig := {}) : WidgetM MenuResult

-- Context menu at position
def contextMenu' (items : Array MenuItem) (position : Dynamic Spider (Nat × Nat))
    (visible : Dynamic Spider Bool) (config : MenuConfig := {}) : WidgetM MenuResult
```

**Features:**
- Nested submenu support
- Keyboard shortcuts display
- Disabled item support
- Arrow key + Enter navigation
- Escape to close
- Disabled items are skipped by navigation and do not emit `onSelect`
- Focus-aware input (`focusName`/`globalKeys`)

### 8.2 Table Widget

Data table with headers, sorting, and selection.

```lean
structure TableColumn where
  header : String
  width : Option Nat := none  -- Auto-size if none
  align : Alignment := .left
  headerStyle : Style := { bold := true }

structure TableConfig where
  columns : Array TableColumn
  style : Style := {}
  headerStyle : Style := { bold := true, bg := .ansi .brightBlack }
  selectedStyle : Style := { bg := .ansi .blue }
  borderType : BorderType := .single
  showHeader : Bool := true
  maxHeight : Option Nat := none
  focusName : String := ""
  globalKeys : Bool := false

structure TableResult (row : Type) where
  selectedIndex : Dynamic Spider (Option Nat)
  selectedRow : Dynamic Spider (Option row)
  onSelect : Event Spider row
  onSort : Event Spider (Nat × Bool)  -- column index, ascending

def table' [ToString row] (rows : Array row) (getCell : row -> Nat -> String)
    (config : TableConfig) : WidgetM (TableResult row)

def dynTable' [ToString row] (rows : Dynamic Spider (Array row))
    (getCell : row -> Nat -> String) (config : TableConfig) : WidgetM (TableResult row)
```

**Features:**
- Column headers with alignment
- Row selection with highlighting
- Scrolling for large datasets
- Dynamic column widths
- Sort indicator support
- Selection clamps when rows update; empty tables yield `selectedIndex = none`
- Sorting is caller-owned; widget only emits `onSort`
- Focus-aware input (`focusName`/`globalKeys`)

### 8.3 Calendar Widget

Monthly calendar with date selection.

```lean
inductive Weekday where
  | sunday | monday | tuesday | wednesday | thursday | friday | saturday

structure YearMonth where
  year : Nat
  month : Nat  -- 1-12

structure Date where
  year : Nat
  month : Nat  -- 1-12
  day : Nat    -- 1..daysInMonth

structure CalendarConfig where
  style : Style := {}
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  todayStyle : Style := { fg := .ansi .cyan, bold := true }
  weekendStyle : Style := { fg := .ansi .brightBlack }
  headerStyle : Style := { bold := true }
  firstDayOfWeek : Weekday := .sunday
  today : Option Date := none
  clampInvalid : Bool := true
  focusName : String := ""
  globalKeys : Bool := false

structure CalendarResult where
  selectedDate : Dynamic Spider (Option Date)
  currentMonth : Dynamic Spider YearMonth
  onSelect : Event Spider Date
  onMonthChange : Event Spider YearMonth

def calendar' (initialMonth : YearMonth) (initialSelectedDay : Option Nat := none)
    (config : CalendarConfig := {}) : WidgetM CalendarResult
```

**Features:**
- Month/year navigation (arrows, Page Up/Down)
- Today highlighting
- Weekend styling
- Date selection with Enter
- First day of week configuration
- Invalid dates are normalized/clamped using the same rules as `Terminus.Widgets.Calendar`
- Focus-aware input (`focusName`/`globalKeys`)

### Phase 8 Deliverables

- [ ] `Terminus/Reactive/Menu.lean` - Menu widget
- [ ] `Terminus/Reactive/Table.lean` - Table widget
- [ ] `Terminus/Reactive/Calendar.lean` - Calendar widget
- [ ] `TerminusTests/Reactive/NavigationTests.lean` - Unit tests
- [ ] `examples/ReactiveData.lean` - Demo app

---

## Phase 9: Data Visualization

**Goal:** Port charting and visualization widgets.

### 9.1 Gauge Widget

Horizontal progress/value gauge.

```lean
structure GaugeConfig where
  width : Nat := 40
  filledChar : Char := '█'
  emptyChar : Char := '░'
  filledStyle : Style := { fg := .ansi .green }
  emptyStyle : Style := { fg := .ansi .brightBlack }
  showValue : Bool := true
  minValue : Float := 0.0
  maxValue : Float := 1.0
  clamp : Bool := true
  valueFormat : Float -> String := fun v => s!"{(v * 100).toUInt32}%" -- v is normalized

def gauge' (value : Float) (config : GaugeConfig := {}) : WidgetM Unit
def dynGauge' (value : Dynamic Spider Float) (config : GaugeConfig := {}) : WidgetM Unit
```

**Notes:** Values are normalized with `minValue`/`maxValue`; if `clamp` is true, out-of-range values are clamped and NaN/Inf treated as 0.

### 9.2 LineGauge Widget

Minimal single-line gauge.

```lean
structure LineGaugeConfig where
  width : Nat := 20
  style : Style := { fg := .ansi .cyan }
  emptyStyle : Style := { fg := .ansi .brightBlack }
  chars : Array Char := #[' ', '▏', '▎', '▍', '▌', '▋', '▊', '▉', '█']
  minValue : Float := 0.0
  maxValue : Float := 1.0
  clamp : Bool := true

def lineGauge' (value : Float) (config : LineGaugeConfig := {}) : WidgetM Unit
def dynLineGauge' (value : Dynamic Spider Float) (config : LineGaugeConfig := {}) : WidgetM Unit
```

**Notes:** Uses the same normalization/clamping rules as `Gauge`.

### 9.3 Sparkline Widget

Inline mini-chart using Unicode blocks.

```lean
structure SparklineConfig where
  width : Option Nat := none  -- Auto-size to data length
  style : Style := { fg := .ansi .cyan }
  blocks : Array Char := #[' ', '▁', '▂', '▃', '▄', '▅', '▆', '▇', '█']
  minValue : Option Float := none
  maxValue : Option Float := none

def sparkline' (data : Array Float) (config : SparklineConfig := {}) : WidgetM Unit
def dynSparkline' (data : Dynamic Spider (Array Float)) (config : SparklineConfig := {})
    : WidgetM Unit
```

**Notes:** If `minValue`/`maxValue` are not provided, scale to the dataset range. Empty data renders an empty node.

### 9.4 BarChart Widget

Vertical or horizontal bar chart.

```lean
inductive BarOrientation where
  | vertical | horizontal

structure BarData where
  label : String
  value : Float
  style : Option Style := none

structure BarChartConfig where
  orientation : BarOrientation := .vertical
  barWidth : Nat := 3
  gap : Nat := 1
  showLabels : Bool := true
  showValues : Bool := true
  style : Style := { fg := .ansi .blue }
  labelStyle : Style := {}
  valueFormat : Float -> String := toString

def barChart' (data : Array BarData) (config : BarChartConfig := {}) : WidgetM Unit
def dynBarChart' (data : Dynamic Spider (Array BarData)) (config : BarChartConfig := {})
    : WidgetM Unit
```

**Notes:** Negative values are clamped to 0 by default; scaling uses the max visible value.

### 9.5 LineChart Widget

Line graph with Braille rendering.

```lean
structure DataSeries where
  label : String
  data : Array Float
  style : Style := { fg := .ansi .cyan }

structure LineChartConfig where
  width : Nat := 60
  height : Nat := 10
  showAxes : Bool := true
  showLegend : Bool := true
  axisStyle : Style := { fg := .ansi .brightBlack }
  minY : Option Float := none  -- Auto-scale if none
  maxY : Option Float := none

def lineChart' (series : Array DataSeries) (config : LineChartConfig := {}) : WidgetM Unit
def dynLineChart' (series : Dynamic Spider (Array DataSeries)) (config : LineChartConfig := {})
    : WidgetM Unit
```

### 9.6 PieChart Widget

Pie/donut chart with optional labels.

```lean
structure PieSlice where
  label : String
  value : Float
  style : Style

structure PieChartConfig where
  radius : Nat := 8
  donut : Bool := false
  donutWidth : Nat := 3
  showLabels : Bool := true
  showPercentage : Bool := true
  labelStyle : Style := {}

def pieChart' (slices : Array PieSlice) (config : PieChartConfig := {}) : WidgetM Unit
def dynPieChart' (slices : Dynamic Spider (Array PieSlice)) (config : PieChartConfig := {})
    : WidgetM Unit
```

**Notes:** Slices with non-positive values are ignored; empty/zero-total data renders an empty chart.

### Phase 9 Deliverables

- [ ] `Terminus/Reactive/Gauge.lean` - Gauge and LineGauge
- [ ] `Terminus/Reactive/Sparkline.lean` - Sparkline widget
- [ ] `Terminus/Reactive/Charts.lean` - BarChart, LineChart, PieChart
- [ ] `TerminusTests/Reactive/ChartTests.lean` - Unit tests
- [ ] `examples/ReactiveCharts.lean` - Demo app

---

## Phase 10: Canvas and Media

**Goal:** Port low-level drawing and media display widgets.

### 10.1 Canvas Widget

Free-form Braille drawing canvas.

```lean
structure CanvasConfig where
  width : Nat
  height : Nat
  style : Style := { fg := .ansi .white }
  background : Option Style := none

structure CanvasResult where
  -- Drawing operations (set pixels in Braille grid)
  setPixel : Nat -> Nat -> IO Unit
  clearPixel : Nat -> Nat -> IO Unit
  drawLine : (Nat × Nat) -> (Nat × Nat) -> IO Unit
  drawRect : (Nat × Nat) -> Nat -> Nat -> IO Unit
  fillRect : (Nat × Nat) -> Nat -> Nat -> IO Unit
  clear : IO Unit

def canvas' (config : CanvasConfig) : WidgetM CanvasResult
```

**Features:**
- Pixel-level drawing via Braille characters
- Line, rectangle primitives
- Fill operations
- Dynamic updates
- Pixel coordinates are in the Braille dot grid (width*2 × height*4); out-of-bounds draws are no-ops
- Reuse `Terminus.Widgets.Canvas.BrailleGrid` helpers to avoid duplicate dot math

### 10.2 Image Widget

Terminal image display using iTerm2/Sixel protocols.

```lean
inductive ImageProtocol where
  | iterm2 | sixel

structure ImageConfig where
  width : Option Nat := none   -- Auto from image if none
  height : Option Nat := none
  protocol : ImageProtocol := .iterm2
  preserveAspect : Bool := true
  name : Option String := none
  background : Style := {}
  altText : String := "Image"

def image' (path : System.FilePath) (config : ImageConfig := {}) : WidgetM Unit
def imageFromBase64' (data : String) (config : ImageConfig := {}) : WidgetM Unit
```

**Features:**
- Multiple terminal protocol support
- Aspect ratio preservation
- Size constraints
- Fallback to `altText` when the protocol is unsupported or the image fails to load

### 10.3 BigText Widget

Large ASCII art text using font patterns.

```lean
inductive BigTextFont where
  | block | slant | small

structure BigTextConfig where
  font : BigTextFont := .block
  style : Style := {}
  alignment : Alignment := .left
  spacing : Nat := 1
  on : Char := '#'
  off : Option Char := some ' '
  offStyle : Style := {}

def bigText' (text : String) (config : BigTextConfig := {}) : WidgetM Unit
```

**Notes:** Fonts should map to `Terminus.Widgets.BigText.BigFont` (`block`, `slant`, `small`).

### Phase 10 Deliverables

- [ ] `Terminus/Reactive/Canvas.lean` - Canvas widget
- [ ] `Terminus/Reactive/Image.lean` - Image widget
- [ ] `Terminus/Reactive/BigText.lean` - BigText widget
- [ ] `TerminusTests/Reactive/MediaTests.lean` - Unit tests
- [ ] `examples/ReactiveMedia.lean` - Demo app

---

## Phase 11: Utility Widgets

**Goal:** Port remaining utility widgets.

### 11.1 Scrollbar Widget

Visual scroll position indicator.

```lean
structure ScrollbarConfig where
  orientation : Orientation := .vertical
  trackChar : Char := '│'
  thumbChar : Char := '█'
  trackStyle : Style := { fg := .ansi .brightBlack }
  thumbStyle : Style := { fg := .ansi .white }

def scrollbar' (position : Dynamic Spider Float) (viewportRatio : Dynamic Spider Float)
    (config : ScrollbarConfig := {}) : WidgetM Unit
```

### 11.2 Paragraph Widget

Multi-line text with word wrapping.

```lean
structure ParagraphConfig where
  style : Style := {}
  alignment : Alignment := .left
  wrap : Bool := true

def paragraph' (text : String) (config : ParagraphConfig := {}) : WidgetM Unit
def dynParagraph' (text : Dynamic Spider String) (config : ParagraphConfig := {})
    : WidgetM Unit
```

### 11.3 Popup Widget

Simple centered dialog overlay.

```lean
structure PopupConfig where
  title : Option String := none
  width : Option Nat := none   -- Auto-size if none
  height : Option Nat := none
  borderType : BorderType := .rounded
  style : Style := {}
  titleStyle : Style := { bold := true }

def popup' (visible : Dynamic Spider Bool) (config : PopupConfig := {})
    (content : WidgetM a) : WidgetM a
```

### 11.4 Clear Widget

Area clearing utility.

```lean
def clear' (style : Style := {}) : WidgetM Unit
```

### Phase 11 Deliverables

- [ ] `Terminus/Reactive/Scrollbar.lean` - Scrollbar widget
- [ ] `Terminus/Reactive/Paragraph.lean` - Paragraph widget
- [ ] `Terminus/Reactive/Popup.lean` - Popup widget
- [ ] `Terminus/Reactive/Clear.lean` - Clear widget
- [ ] `TerminusTests/Reactive/UtilityTests.lean` - Unit tests

---

## Summary: Phases and Priorities

| Phase | Widgets | Priority | Effort |
|-------|---------|----------|--------|
| **6** | Checkbox, RadioGroup | High | Low |
| **7** | Spinner, Logger, Notification | High | Medium |
| **8** | Menu, Table, Calendar | High | Medium |
| **9** | Gauge, LineGauge, Sparkline, BarChart, LineChart, PieChart | Medium | High |
| **10** | Canvas, Image, BigText | Low | High |
| **11** | Scrollbar, Paragraph, Popup, Clear | Low | Low |

**Recommended order:** 6 -> 7 -> 8 -> 11 -> 9 -> 10

This prioritizes high-impact interactive widgets first, then utilities, then visualization (which is less commonly needed for typical TUI apps).

---

## Testing Strategy

Each phase includes:

1. **Unit tests** - Component behavior in isolation using `LoopDeps` mock
2. **Integration tests** - Components working together
3. **Demo app** - Visual verification in `examples/`

Test patterns (from existing tests):
- Mock event sources with `TerminusEvents` and `LoopDeps`
- Buffer capture for visual assertions
- Event injection for interaction testing
- Use `Common.lean` test helpers
- Time-based widgets (spinner/notifications) should be driven by mock tick events
- Dynamic collections should include tests for data shrink/empty/clamp behavior

---

## Migration Notes

When porting widgets:

1. **State becomes Dynamic** - Mutable state fields become `Dynamic Spider T`
2. **Events become Event** - Callbacks become `Event Spider T`
3. **handleEvent becomes hooks** - Replace `handleEvent` with `useKey`, `useChar`, etc.
4. **render becomes emit** - Replace `Widget.render` with `emit (pure node)`
5. **Block containers** - Use `block'` or `titledBlock'` from Containers
6. **Focus** - Use `useFocusedInput` for focus-aware behavior
7. **IO-triggered updates** - Fire trigger events via `SpiderEnv.withFrame` when updating from `IO`

Example transformation:

```lean
-- Old style
structure Checkbox where
  checked : Bool

instance : Widget Checkbox where
  render c area buf := ...
  handleEvent c event := match event with
    | .key k => if k.code == .space then c.toggle else c
    | _ => c

-- New style
def checkbox' (name : String) (label : String) (initial : Bool) : WidgetM CheckboxResult := do
  let events ← getEventsW
  let widgetName ← registerComponentW "checkbox" (isInput := true)
  let focusedInput ← useFocusedInputW

  let (toggleEvent, fireToggle) ← newTriggerEvent (t := Spider) (a := Bool)
  let checkedDyn ← holdDyn initial toggleEvent

  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    let focus ← focusedInput.sample
    let isFocused := focus == some (if name.isEmpty then widgetName else name)
    if isFocused && (kd.event.code == .space || kd.event.code == .enter) then
      let cur ← checkedDyn.sample
      fireToggle (!cur)

  emitDynamic do
    let isChecked ← checkedDyn.sample
    let text := if isChecked then "[x]" else "[ ]"
    pure (RNode.text (text ++ " " ++ label) {})

  pure { checked := checkedDyn, onToggle := toggleEvent }
```
