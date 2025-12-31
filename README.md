# Terminus

A ratatui-style terminal UI library for Lean 4.

Terminus provides widgets, layouts, and styling for building terminal user interfaces with immediate-mode rendering.

## Features

- **Full Color Support**: 16 ANSI colors, 256 indexed colors, and RGB true color
- **Raw Terminal Mode**: Character-by-character input via FFI to termios
- **Mouse Support**: Click, scroll, and drag events with modifier key detection
- **Widget System**: 25+ widgets including charts, inputs, trees, forms, and more
- **Flexible Layouts**: Constraints (fixed, percent, ratio, fill) with vertical/horizontal splits
- **Efficient Rendering**: Buffer diffing for minimal terminal updates
- **Image Support**: Display images via iTerm2 inline images protocol

## Requirements

- Lean 4 v4.26.0
- Unix-like system (macOS, Linux) for terminal FFI

## Installation

Add Terminus to your `lakefile.lean`:

```lean
require terminus from git "https://github.com/your-username/terminus" @ "main"
```

Or clone and build locally:

```bash
git clone https://github.com/your-username/terminus
cd terminus
lake build
```

## Quick Start

```lean
import Terminus

open Terminus

def main : IO Unit := do
  Terminal.withTerminal fun term => do
    let frame := Frame.new term.area

    let block := Block.rounded
      |>.withTitle "Hello"
      |>.withBorderStyle (Style.fgColor Color.blue)

    let text := Paragraph.fromString "Hello, Terminus!"
      |>.centered
      |>.withBlock block

    let frame := frame.render text term.area
    let term := term.setBuffer frame.buffer
    let _ ← term.draw
    let _ ← Events.read
```

## Examples

Build and run the examples:

```bash
lake build

# Basic examples
lake exe hello        # Basic hello world
lake exe counter      # Interactive counter with gauge
lake exe dashboard    # Multi-widget layout demo

# Advanced examples
lake exe charts       # Data visualization with charts
lake exe fileexplorer # Tree view with scrollbar and popups
lake exe texteditor   # Text input and editing widgets
lake exe form         # Form widgets with validation
lake exe menu         # Dropdown menu widget
lake exe piechart     # Pie and donut charts
lake exe image        # Image display (iTerm2/WezTerm)
lake exe mouse        # Mouse event handling demo
lake exe bigtext      # Large ASCII art text
lake exe logger       # Scrolling log widget
lake exe kitchensink  # Comprehensive widget showcase
```

### HelloWorld

Displays a centered greeting with a styled border.

```bash
lake exe hello
```

### Counter

Interactive counter demonstrating:
- Keyboard input handling (arrow keys, +/-)
- Gauge widget for progress display
- Layout splitting

```bash
lake exe counter
```

**Controls:** `↑`/`↓` or `+`/`-` to change value, `q` to quit

### Dashboard

Complex multi-widget demo featuring:
- Tab navigation
- List widget with selection
- Table widget
- Multiple gauges
- Responsive layout

```bash
lake exe dashboard
```

**Controls:** `Tab` to switch sections, `↑`/`↓` to navigate, `q` to quit

### Charts

Data visualization demo showcasing:
- **Sparkline**: Inline mini-charts using Unicode block characters
- **BarChart**: Vertical bar chart with labels and values
- **LineChart**: Multi-series line graph with Braille rendering

```bash
lake exe charts
```

**Controls:** `Tab` to cycle chart types, `q` to quit

### FileExplorer

Tree-based file browser demonstrating:
- **Tree**: Hierarchical view with expand/collapse
- **Scrollbar**: Visual scroll position indicator
- **Popup**: Confirmation dialogs

```bash
lake exe fileexplorer
```

**Controls:** `↑`/`↓` to navigate, `Enter` to expand/collapse, `d` to delete (with confirmation), `q` to quit

### TextEditor

Text editing demo featuring:
- **TextInput**: Single-line input field with cursor
- **TextArea**: Multi-line text editor with line numbers
- **Calendar**: Monthly calendar with date selection

```bash
lake exe texteditor
```

**Controls:** `Tab` to switch focus, type to edit, arrow keys to navigate, `Esc` to quit

### MouseDemo

Interactive mouse event demonstration:
- Click, scroll, and drag detection
- Modifier key tracking (Ctrl, Alt, Shift)
- Event history display

```bash
lake exe mouse
```

**Controls:** Click, scroll, or drag anywhere to see events, `q` to quit

### KitchenSink

Comprehensive demo showcasing all widgets with tab-based navigation through different categories.

```bash
lake exe kitchensink
```

**Controls:** `Tab` to switch categories, widget-specific controls within each section

## Architecture

### Core Types

- `Color` - 16/256/RGB color specification
- `Style` - Foreground, background, and text modifiers
- `Cell` - Single styled character
- `Rect` - Rectangular region
- `Buffer` - 2D grid of cells

### Widgets

All widgets implement the `Widget` typeclass:

```lean
class Widget (α : Type) where
  render : α → Rect → Buffer → Buffer
```

Available widgets:

**Basic Widgets:**
- `Block` - Container with borders and title
- `Paragraph` - Multi-line text with alignment and wrapping
- `ListWidget` - Selectable list with scrolling
- `Table` - Data table with headers
- `Gauge` - Horizontal progress bar
- `LineGauge` - Minimal line-based progress indicator
- `Tabs` - Tab bar for navigation
- `BigText` - Large ASCII art text rendering
- `Clear` - Area clearing widget

**Chart Widgets:**
- `Sparkline` - Inline mini-chart using Unicode blocks (`▁▂▃▄▅▆▇█`)
- `BarChart` - Vertical/horizontal bar chart with labels
- `LineChart` - Line graph with axes (Braille rendering)
- `PieChart` - Pie and donut charts
- `Canvas` - Free-form drawing with Braille sub-cell resolution

**Input Widgets:**
- `TextInput` - Single-line text input with cursor
- `TextArea` - Multi-line text editor with line numbers
- `Checkbox` - Toggle checkbox widget
- `Form` - Form container with field management

**Navigation Widgets:**
- `Tree` - Hierarchical tree view with expand/collapse
- `Menu` - Dropdown/context menu
- `Calendar` - Monthly calendar with date selection
- `Scrollbar` - Visual scroll position indicator
- `ScrollView` - Scrollable content container
- `Popup` - Centered overlay dialog box

**Feedback Widgets:**
- `Spinner` - Animated loading indicator (7 built-in styles)
- `Logger` - Scrolling log display
- `Image` - Terminal image display (iTerm2 protocol)

### Layout

Split areas using constraints:

```lean
-- Vertical split: 3 rows fixed, rest fills
let sections := vsplit area [.fixed 3, .fill]

-- Horizontal split: 40% left, 60% right
let panels := hsplit area [.percent 40, .fill]
```

Constraint types:
- `.fixed n` - Exactly n cells
- `.percent p` - Percentage of available space
- `.ratio n d` - Ratio relative to other ratio constraints
- `.min n` / `.max n` - Minimum/maximum bounds
- `.fill` - Take remaining space

### Input

```lean
-- Non-blocking poll for keyboard and mouse events
let event ← Events.poll
match event with
| .key keyEvent => handleKey keyEvent
| .mouse mouseEvent => handleMouse mouseEvent
| .resize w h => handleResize w h
| .none => pure ()

-- Blocking read (keyboard only)
let key ← Events.read
if key.isCtrlQ then quit
```

Mouse events include button (left/middle/right/scroll), action (press/release/motion), position, and modifier keys.

### App Framework

For interactive applications:

```lean
def draw (frame : Frame) (state : MyState) : Frame := ...
def update (state : MyState) (key : KeyEvent) : MyState × Bool := ...

def main : IO Unit := do
  App.runApp initialState draw update
```

## Project Structure

```
terminus/
├── lakefile.lean           # Build configuration
├── lean-toolchain          # Lean version (v4.26.0)
├── Terminus.lean           # Library entry point
├── Terminus/
│   ├── Core/
│   │   ├── Style.lean      # Colors, modifiers, Orientation
│   │   ├── Cell.lean       # Styled character
│   │   ├── Rect.lean       # Rectangular regions
│   │   ├── Buffer.lean     # Cell grid
│   │   └── Base64.lean     # Base64 encoding (for images)
│   ├── Backend/
│   │   ├── Ansi.lean       # ANSI escape codes
│   │   ├── Raw.lean        # FFI bindings
│   │   ├── Terminal.lean   # Terminal I/O
│   │   ├── TerminalEffect.lean  # Terminal effect typeclass
│   │   └── TerminalMock.lean    # Mock for testing
│   ├── Input/
│   │   ├── Key.lean        # Key and mouse event types
│   │   └── Events.lean     # Event polling and parsing
│   ├── Layout/
│   │   ├── Constraint.lean # Layout constraints
│   │   └── Layout.lean     # Layout solver
│   ├── Widgets/            # 25+ widgets
│   │   ├── Widget.lean     # Widget typeclass
│   │   ├── Block.lean      # Border container
│   │   ├── Paragraph.lean  # Text widget
│   │   ├── List.lean       # Selectable list
│   │   ├── Table.lean      # Data table
│   │   ├── Gauge.lean      # Progress bar
│   │   ├── LineGauge.lean  # Line progress indicator
│   │   ├── Tabs.lean       # Tab bar
│   │   ├── BigText.lean    # ASCII art text
│   │   ├── Sparkline.lean  # Mini inline chart
│   │   ├── BarChart.lean   # Bar chart
│   │   ├── LineChart.lean  # Line graph
│   │   ├── PieChart.lean   # Pie/donut chart
│   │   ├── Canvas.lean     # Braille drawing
│   │   ├── TextInput.lean  # Single-line input
│   │   ├── TextArea.lean   # Multi-line editor
│   │   ├── Checkbox.lean   # Toggle checkbox
│   │   ├── Form.lean       # Form container
│   │   ├── Tree.lean       # Tree view
│   │   ├── Menu.lean       # Dropdown menu
│   │   ├── Calendar.lean   # Calendar widget
│   │   ├── Scrollbar.lean  # Scroll indicator
│   │   ├── ScrollView.lean # Scrollable container
│   │   ├── Popup.lean      # Dialog overlay
│   │   ├── Spinner.lean    # Loading indicator
│   │   ├── Logger.lean     # Log display
│   │   ├── Image.lean      # Image display
│   │   └── Clear.lean      # Area clearing
│   └── Frame.lean          # Rendering frame
├── ffi/
│   └── terminus.c          # C termios bindings
├── Tests/
│   └── Main.lean           # Test suite
└── examples/
    ├── HelloWorld.lean     # Basic example
    ├── Counter.lean        # Interactive counter
    ├── Dashboard.lean      # Multi-widget demo
    ├── Charts.lean         # Chart widgets demo
    ├── FileExplorer.lean   # Tree/popup demo
    ├── TextEditor.lean     # Input widgets demo
    ├── Form.lean           # Form widgets demo
    ├── Menu.lean           # Menu widget demo
    ├── PieChart.lean       # Pie chart demo
    ├── Image.lean          # Image display demo
    ├── MouseDemo.lean      # Mouse events demo
    ├── BigText.lean        # ASCII art demo
    ├── Logger.lean         # Logger widget demo
    └── KitchenSink.lean    # Comprehensive showcase
```

## License

MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

Inspired by [ratatui](https://github.com/ratatui/ratatui), the Rust TUI library.
