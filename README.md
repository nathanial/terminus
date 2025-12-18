# Terminus

A ratatui-style terminal UI library for Lean 4.

Terminus provides widgets, layouts, and styling for building terminal user interfaces with immediate-mode rendering.

## Features

- **Full Color Support**: 16 ANSI colors, 256 indexed colors, and RGB true color
- **Raw Terminal Mode**: Character-by-character input via FFI to termios
- **Widget System**: Block, Paragraph, List, Table, Gauge, Tabs
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
.lake/build/bin/hello      # Basic hello world
.lake/build/bin/counter    # Interactive counter with gauge
.lake/build/bin/dashboard  # Multi-widget layout demo
```

### HelloWorld

Displays a centered greeting with a styled border.

### Counter

Interactive counter demonstrating:
- Keyboard input handling (arrow keys, +/-)
- Gauge widget for progress display
- Layout splitting

### Dashboard

Complex multi-widget demo featuring:
- Tab navigation
- List widget with selection
- Table widget
- Multiple gauges
- Responsive layout

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
- `Block` - Container with borders and title
- `Paragraph` - Multi-line text with alignment and wrapping
- `ListWidget` - Selectable list with scrolling
- `Table` - Data table with headers
- `Gauge` - Horizontal progress bar
- `Tabs` - Tab bar for navigation

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
│   │   ├── Style.lean      # Colors and modifiers
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
│   │   └── Tabs.lean       # Tab bar
│   └── Frame.lean          # Rendering frame
├── ffi/
│   └── terminus.c          # C termios bindings
└── examples/
    ├── HelloWorld.lean
    ├── Counter.lean
    └── Dashboard.lean
```

## License

MIT License - see [LICENSE](LICENSE) for details.

## Acknowledgments

Inspired by [ratatui](https://github.com/ratatui/ratatui), the Rust TUI library.
