# Terminus

A ratatui-style terminal UI library for Lean 4.

Terminus provides widgets, layouts, and styling for building terminal user interfaces with immediate-mode rendering.

## Features

- **Full Color Support**: 16 ANSI colors, 256 indexed colors, and RGB true color
- **Raw Terminal Mode**: Character-by-character input via FFI to termios
- **Widget System**: 16 widgets including charts, inputs, trees, and more
- **Flexible Layouts**: Constraints (fixed, percent, ratio, fill) with vertical/horizontal splits
- **Efficient Rendering**: Buffer diffing for minimal terminal updates

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
- `Tabs` - Tab bar for navigation

**Chart Widgets:**
- `Sparkline` - Inline mini-chart using Unicode blocks (`▁▂▃▄▅▆▇█`)
- `BarChart` - Vertical/horizontal bar chart with labels
- `LineChart` - Line graph with axes (Braille rendering)
- `Canvas` - Free-form drawing with Braille sub-cell resolution

**Input Widgets:**
- `TextInput` - Single-line text input with cursor
- `TextArea` - Multi-line text editor with line numbers

**Navigation Widgets:**
- `Tree` - Hierarchical tree view with expand/collapse
- `Calendar` - Monthly calendar with date selection
- `Scrollbar` - Visual scroll position indicator
- `Popup` - Centered overlay dialog box

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
-- Non-blocking poll
let event ← Events.poll
match event with
| .key keyEvent => handleKey keyEvent
| .none => pure ()

-- Blocking read
let key ← Events.read
if key.isCtrlQ then quit
```

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
│   │   └── Buffer.lean     # Cell grid
│   ├── Backend/
│   │   ├── Ansi.lean       # ANSI escape codes
│   │   ├── Raw.lean        # FFI bindings
│   │   └── Terminal.lean   # Terminal I/O
│   ├── Input/
│   │   ├── Key.lean        # Key event types
│   │   └── Events.lean     # Event polling
│   ├── Layout/
│   │   ├── Constraint.lean # Layout constraints
│   │   └── Layout.lean     # Layout solver
│   ├── Widgets/
│   │   ├── Widget.lean     # Widget typeclass
│   │   ├── Block.lean      # Border container
│   │   ├── Paragraph.lean  # Text widget
│   │   ├── List.lean       # Selectable list
│   │   ├── Table.lean      # Data table
│   │   ├── Gauge.lean      # Progress bar
│   │   ├── Tabs.lean       # Tab bar
│   │   ├── Sparkline.lean  # Mini inline chart
│   │   ├── Scrollbar.lean  # Scroll indicator
│   │   ├── Popup.lean      # Dialog overlay
│   │   ├── BarChart.lean   # Bar chart
│   │   ├── Tree.lean       # Tree view
│   │   ├── Calendar.lean   # Calendar widget
│   │   ├── TextInput.lean  # Single-line input
│   │   ├── Canvas.lean     # Braille drawing
│   │   ├── LineChart.lean  # Line graph
│   │   └── TextArea.lean   # Multi-line editor
│   └── Frame.lean          # Rendering frame
├── ffi/
│   └── terminus.c          # C termios bindings
└── examples/
    ├── HelloWorld.lean     # Basic example
    ├── Counter.lean        # Interactive counter
    ├── Dashboard.lean      # Multi-widget demo
    ├── Charts.lean         # Chart widgets demo
    ├── FileExplorer.lean   # Tree/popup demo
    └── TextEditor.lean     # Input widgets demo
```

## License

MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

Inspired by [ratatui](https://github.com/ratatui/ratatui), the Rust TUI library.
