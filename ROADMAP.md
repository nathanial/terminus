# Terminus Roadmap

This document outlines potential improvements, new features, code cleanup opportunities, and technical debt for the Terminus terminal UI library.

---

## Completed Features

### Mouse Support ✓

Full mouse event handling implemented including click, scroll, and drag operations.

**Implementation:**
- `MouseEvent`, `MouseButton`, `MouseAction` types in `Terminus/Input/Key.lean`
- SGR mouse protocol parsing in `Terminus/Input/Events.lean`
- Demo application: `examples/MouseDemo.lean`

---

### Window Resize Events ✓

Terminal resize events are now supported via the `Event.resize` variant.

**Implementation:**
- `Event.resize (width height : Nat)` in `Terminus/Input/Key.lean`

---

### Spinner Widget ✓

Animated loading indicator with multiple built-in styles.

**Implementation:**
- `Terminus/Widgets/Spinner.lean` with styles: dots, line, arc, arrows, blocks, growing, ascii

---

### Progress Bar Variants ✓

Multiple progress indicator styles are now available.

**Implementation:**
- `Gauge` - standard horizontal progress bar
- `LineGauge` - minimal line-based gauge
- `Spinner` - animated loading indicators

---

### Image Widget ✓

Terminal image display using iTerm2 inline images protocol.

**Implementation:**
- `Terminus/Widgets/Image.lean` with `fromBytes` and `fromPath` constructors
- Demo: `examples/Image.lean`

---

### Additional Widgets ✓

Many new widgets have been added since the initial release:

- **BigText** - Large ASCII art text rendering (`Terminus/Widgets/BigText.lean`)
- **Checkbox** - Toggle checkbox widget (`Terminus/Widgets/Checkbox.lean`)
- **Clear** - Area clearing widget (`Terminus/Widgets/Clear.lean`)
- **Form** - Form container with field management (`Terminus/Widgets/Form.lean`)
- **Logger** - Scrolling log display (`Terminus/Widgets/Logger.lean`)
- **Menu** - Dropdown/context menu (`Terminus/Widgets/Menu.lean`)
- **PieChart** - Pie and donut charts (`Terminus/Widgets/PieChart.lean`)
- **ScrollView** - Scrollable content container (`Terminus/Widgets/ScrollView.lean`)

---

### Hyperlink Support ✓

Clickable terminal hyperlinks using OSC 8 escape sequences.

**Implementation:**
- `Cell.hyperlink` field for per-cell URL storage
- `Ansi.hyperlinkStart` / `Ansi.hyperlinkEnd` escape sequences
- `Buffer.writeLink` / `Frame.writeLink` helper functions
- Automatic hyperlink state tracking in `Terminal.draw` and `Terminal.flush`
- Demo in `examples/HelloWorld.lean`

**Supported terminals:** iTerm2, Windows Terminal, GNOME Terminal, WezTerm, and others.

---

### Widget Tests (Partial) ✓

Added test coverage for core modules and widget rendering.

**Implementation:**
- Cell tests (empty, new, styled, hyperlinks)
- Buffer tests (new, set, get, writeString, writeLink, clear, diff)
- Rect tests (isEmpty, area)
- Style tests (default, modifiers, merge)
- Layout tests (hsplit, vsplit, constraints)
- Widget rendering tests (Block, Paragraph, Gauge, Tabs, ListWidget, TextInput, TextArea, Notification, BarChart, Table, Checkbox, RadioGroup, Spinner, Calendar, Menu, LineChart, PieChart, Tree, Popup, Sparkline, LineGauge)
- Test helper: `renderWidget` for buffer-based widget testing

**Current coverage:** 234 tests total (21 widgets tested)

---

### Clipboard Integration ✓

Full clipboard support using OSC 52 escape sequences.

**Implementation:**
- OSC 52 escape sequences in `Terminus/Backend/Ansi.lean` (`clipboardWrite`, `clipboardQuery`, `clipboardClear`)
- Base64 encode/decode in `Terminus/Core/Base64.lean`
- `ClipboardCommand` and `ClipboardSelection` types in `Terminus/Backend/Commands.lean`
- `TerminalCommand.clipboard` variant for clipboard operations
- TextInput selection support: `selectAll`, `hasSelection`, `selectedText`, `copy`, `cut`, `paste`
- TextArea multi-line selection support: `selectAll`, `selectionRange`, `selectedText`, `copy`, `cut`, `paste`
- Selection highlighting in both TextInput and TextArea widgets
- Alt+A (select all), Alt+C (copy), Alt+X (cut) keyboard shortcuts
- 21 clipboard-related tests

**Supported terminals:** iTerm2, Windows Terminal, xterm, and others with OSC 52 support.

---

### Notification/Toast Widget ✓

Toast-style notification widget for temporary messages with stacking support.

**Implementation:**
- `NotificationPosition` enum (topLeft, topCenter, topRight, bottomLeft, bottomCenter, bottomRight)
- `NotificationLevel` enum (info, success, warning, error) with preset styles
- `Notification` structure with multi-line support, auto-sizing, and configurable borders
- `NotificationStack` for managing multiple stacked notifications
- Corner-based positioning with margin and spacing configuration
- Widget instances for both single notifications and stacks
- 13 notification tests

**Usage:**
```lean
-- Single notification
let notif := Notification.success "File saved!"

-- Stacked notifications
let stack := NotificationStack.atPosition .topRight
  |>.push (Notification.info "Processing...")
  |>.push (Notification.warning "Low memory")
```

**Note:** Timeouts are managed by the application (widgets are stateless).

---

## Feature Proposals

### [Priority: Medium] Async Event Polling

**Description:** Implement non-blocking async I/O for event polling using Lean's Task system, allowing applications to perform background work while waiting for input.

**Rationale:** The current synchronous polling model blocks the main thread. Async support would enable more sophisticated applications with background tasks.

**Affected Files:**
- `ffi/terminus.c` (select/poll-based reading)
- `Terminus/Input/Events.lean`
- `Terminus/Backend/TerminalEffect.lean`

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] Cross-Platform Windows Support

**Description:** Add Windows Console API support as an alternative backend to the Unix termios-based implementation.

**Rationale:** Currently terminus only works on Unix-like systems. Windows support would significantly expand the library's utility.

**Affected Files:**
- `ffi/terminus.c` (add Windows conditionals)
- New file: `ffi/terminus_win.c` (Windows-specific implementation)
- `lakefile.lean` (conditional compilation)

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] Composable Widget Typeclass

**Description:** Introduce a Widget typeclass with methods for measuring preferred size, handling input events, and rendering. This would enable more sophisticated layout algorithms and event routing.

**Rationale:** The current `Widget` class only has a `render` method. A richer interface would enable:
- Automatic size calculation for layouts
- Event bubbling and focus management
- Widget introspection

**Affected Files:**
- `Terminus/Widgets/Widget.lean`
- All widget files would need to implement new methods

**Estimated Effort:** Large

**Dependencies:** None

---

### [Priority: Medium] Focus Management System

**Description:** Implement a focus management system for navigating between interactive widgets using Tab/Shift+Tab or arrow keys.

**Rationale:** Currently each application must manually track focus state. A centralized focus system would reduce boilerplate and provide consistent behavior across applications.

**Affected Files:**
- New file: `Terminus/Core/Focus.lean`
- `Terminus/Widgets/Widget.lean` (focusable trait)
- Interactive widgets (TextInput, TextArea, etc.)

**Estimated Effort:** Medium

**Dependencies:** Composable Widget Typeclass

---

### [Priority: Medium] Text Wrapping Modes

**Description:** Implement multiple text wrapping modes for Paragraph widget: word wrap, character wrap, no wrap with horizontal scroll.

**Rationale:** The current Paragraph implementation has basic text handling. Proper word wrapping would improve text display quality.

**Affected Files:**
- `Terminus/Widgets/Paragraph.lean`
- `Terminus/Core/` (new text utilities)

**Estimated Effort:** Medium

**Dependencies:** None

---

### [Priority: Low] Markdown Widget

**Description:** Create a widget that renders basic Markdown (headers, bold, italic, code blocks, lists) with appropriate styling.

**Rationale:** Many applications display documentation or formatted text. A Markdown widget would provide rich text rendering with minimal effort.

**Affected Files:**
- New file: `Terminus/Widgets/Markdown.lean`
- `Terminus/Core/Style.lean` (for styling)

**Estimated Effort:** Medium

**Dependencies:** Text Wrapping Modes

---

### [Priority: Low] Sixel Graphics Support

**Description:** Add Sixel graphics protocol support for displaying images in compatible terminals.

**Rationale:** While iTerm2 protocol is supported via Image widget, Sixel has broader terminal support (xterm, mlterm, foot).

**Affected Files:**
- New file: `Terminus/Widgets/Sixel.lean` or extend Image widget
- `Terminus/Core/Base64.lean` (encoding utilities)

**Estimated Effort:** Medium

**Dependencies:** None

---

## Code Improvements

### [Priority: High] Type-Safe Buffer Indexing

**Current State:** Buffer access in `Terminus/Core/Buffer.lean` uses `cells[idx]!` which can panic on out-of-bounds access.

**Proposed Change:** Use bounds-checked access with proper error handling or proof-carrying code to ensure safe indexing.

**Benefits:** Prevents runtime panics, improves reliability.

**Affected Files:**
- `Terminus/Core/Buffer.lean`

**Estimated Effort:** Medium

---

### [Priority: High] Unicode Width Calculation

**Current State:** Wide characters (CJK, emoji) are treated as single-width in cell positioning, causing display issues.

**Proposed Change:** Implement Unicode width calculation (wcwidth equivalent) and handle double-width characters properly in Buffer operations.

**Benefits:** Correct display of international text and emoji.

**Affected Files:**
- `Terminus/Core/Cell.lean`
- `Terminus/Core/Buffer.lean`
- New file: `Terminus/Core/Unicode.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Style Merging Improvements

**Current State:** `Style.merge` in `Terminus/Core/Style.lean` has simple override semantics that may not match user expectations.

**Proposed Change:** Implement more nuanced style merging with explicit inheritance rules, possibly using a `StyleDiff` type.

**Benefits:** More predictable style composition for complex UIs.

**Affected Files:**
- `Terminus/Core/Style.lean`

**Estimated Effort:** Small

---

### [Priority: Medium] Buffer Diff Optimization

**Current State:** The `diff` function in Buffer.lean iterates through all cells, but could be optimized for common patterns.

**Proposed Change:** Implement dirty region tracking or use a more efficient diffing algorithm (e.g., row-level checksums).

**Benefits:** Reduced rendering overhead for large terminals.

**Affected Files:**
- `Terminus/Core/Buffer.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Layout Algorithm Optimization

**Current State:** The layout algorithm in `Terminus/Layout/Layout.lean` recalculates constraints on every split.

**Proposed Change:** Cache constraint calculations and implement incremental layout updates.

**Benefits:** Better performance for complex nested layouts.

**Affected Files:**
- `Terminus/Layout/Layout.lean`
- `Terminus/Layout/Constraint.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Escape Sequence Parser State Machine

**Current State:** Escape sequence parsing in `Terminus/Input/Events.lean` uses nested pattern matching which is hard to extend.

**Proposed Change:** Implement a proper state machine parser that can handle arbitrary escape sequences, including CSI parameters.

**Benefits:** More robust input handling, easier to add new key sequences.

**Affected Files:**
- `Terminus/Input/Events.lean`

**Estimated Effort:** Medium

---

### [Priority: Medium] Centralized Error Handling

**Current State:** FFI functions return IO errors but error messages are hardcoded strings.

**Proposed Change:** Define a `TerminalError` inductive type with structured error information.

**Benefits:** Better error handling, testability, and user feedback.

**Affected Files:**
- New file: `Terminus/Core/Error.lean`
- `Terminus/Backend/Raw.lean`
- `ffi/terminus.c`

**Estimated Effort:** Small

---

### [Priority: Low] Canvas Anti-Aliasing

**Current State:** The BrailleGrid in Canvas uses simple pixel-level rendering without anti-aliasing.

**Proposed Change:** Implement anti-aliased line drawing using sub-pixel coverage calculation.

**Benefits:** Smoother lines in charts and canvas drawings.

**Affected Files:**
- `Terminus/Widgets/Canvas.lean`

**Estimated Effort:** Medium

---

### [Priority: Low] Float.sin/cos Performance

**Current State:** Several widgets (charts, animations) use Float.sin/cos which may be slower than lookup tables for animation use cases.

**Proposed Change:** Implement fast approximate trig functions or lookup tables for animation-quality rendering.

**Benefits:** Better performance for animated UIs.

**Affected Files:**
- `examples/KitchenSink.lean`
- Chart widgets

**Estimated Effort:** Small

---

## Code Cleanup

### [Priority: Low] Remaining Widget Tests

**Issue:** 21 of 29 widgets now have tests (72% coverage). Remaining untested widgets include Canvas, ScrollView, Scrollbar, BigText, Logger, Form, Image, and Clear.

**Location:**
- `Tests/Main.lean`

**Remaining widgets to test:**
- Data visualization: Canvas
- Navigation: ScrollView, Scrollbar
- Specialty: BigText, Logger, Form, Image, Clear

**Estimated Effort:** Small

---

### [Priority: Medium] Inconsistent Float Conversion

**Issue:** Various widgets use different patterns for Float to Nat conversion: `toUInt32.toNat`, `Int.ofNat`, direct `.toNat`. This is inconsistent and some conversions may lose precision or behave unexpectedly with negative values.

**Location:**
- `Terminus/Widgets/Canvas.lean` (lines 250-275)
- `Terminus/Widgets/LineChart.lean` (lines 227-230)
- `Terminus/Widgets/BarChart.lean` (line 96)

**Action Required:**
1. Define a consistent helper function for Float to Nat conversion
2. Handle negative values explicitly
3. Apply consistently across all widgets

**Estimated Effort:** Small

---

### [Priority: Medium] Duplicated innerArea Pattern

**Issue:** Every widget has the same boilerplate for handling optional blocks and computing inner areas.

**Location:** All widget files in `Terminus/Widgets/`

**Action Required:**
1. Extract common pattern into a helper function
2. Consider a Widget wrapper that handles block rendering

**Estimated Effort:** Small

---

### [Priority: Medium] Magic Numbers in Widgets

**Issue:** Several widgets contain hardcoded numbers without explanation.

**Location:**
- `Terminus/Widgets/LineChart.lean` (line 102: `height < 3`)
- `Terminus/Widgets/PieChart.lean` (line 55: donut ratio 0.9 max)
- `Terminus/Widgets/BarChart.lean` (line 141: label width 10)

**Action Required:**
1. Replace magic numbers with named constants
2. Add documentation explaining the rationale for each value

**Estimated Effort:** Small

---

### [Priority: Medium] Documentation Gaps

**Issue:** Many public functions lack documentation comments.

**Location:**
- Most widget files
- `Terminus/Core/Buffer.lean`
- `Terminus/Layout/Layout.lean`

**Action Required:**
1. Add doc-strings to all public functions and types
2. Include examples in documentation for key APIs
3. Document expected behavior for edge cases

**Estimated Effort:** Medium

---

### [Priority: Low] Unused Imports

**Issue:** Some files may have unused imports.

**Location:** Various files (requires static analysis)

**Action Required:**
1. Review imports in each file
2. Remove unused imports

**Estimated Effort:** Small

---

### [Priority: Low] Consistent Naming Conventions

**Issue:** Some naming inconsistencies exist:
- `withX` vs `setX` for builder methods
- `new` vs module-level constructors

**Location:** Throughout widget files

**Action Required:**
1. Establish naming convention in documentation
2. Refactor for consistency where feasible (may require deprecation warnings)

**Estimated Effort:** Small

---

### [Priority: Low] Example Code Organization

**Issue:** KitchenSink.lean is very large (~1700 lines) and could be split into separate files.

**Location:**
- `examples/KitchenSink.lean`

**Action Required:**
1. Split into separate demo modules
2. Create a shared state management abstraction

**Estimated Effort:** Small

---

## Architectural Considerations

### [Priority: High] Backend Abstraction

**Issue:** The current backend implementation is tightly coupled to Unix termios. A proper backend abstraction would make it easier to:
- Add Windows support
- Create test backends
- Support alternative rendering targets

**Proposed Change:** Define a `Backend` typeclass with operations for:
- Raw mode management
- Terminal size queries
- Input reading
- Output writing

**Affected Files:**
- `Terminus/Backend/Terminal.lean`
- `Terminus/Backend/TerminalIO.lean`
- `Terminus/Backend/TerminalMock.lean`

---

### [Priority: Medium] Widget State Management

**Issue:** Interactive widgets (TextInput, TextArea, Tree, etc.) manage their own state but there's no unified pattern for state updates and event handling.

**Proposed Change:** Consider an Elm-like architecture with:
- Immutable widget state
- Update functions that return new state
- Event system for inter-widget communication

---

### [Priority: Medium] Layered Rendering

**Issue:** Popups and overlays are rendered inline with other widgets, which can cause Z-ordering issues.

**Proposed Change:** Implement a layered rendering system with:
- Background layer (normal widgets)
- Overlay layer (popups, dropdowns)
- Top layer (tooltips, notifications)

---

## Testing Improvements

### [Priority: High] Expand Test Coverage

**Current Coverage:**
- TerminalEffect mock (raw mode, size, I/O): Well tested
- Key/escape sequence parsing: Well tested
- Event polling: Basic tests

**Missing Coverage:**
- Widget rendering
- Layout calculations
- Style merging
- Buffer operations
- Canvas/chart drawing

**Action Required:**
1. Create buffer comparison utilities for widget tests
2. Add unit tests for each widget
3. Add property-based tests for layout algorithms

---

### [Priority: Medium] Visual Regression Testing

**Proposed:** Create a system for comparing rendered buffers against golden files, enabling detection of rendering regressions.

---

### [Priority: Low] Performance Benchmarks

**Proposed:** Add benchmarks for:
- Buffer diff performance
- Layout calculation speed
- Widget rendering throughput
