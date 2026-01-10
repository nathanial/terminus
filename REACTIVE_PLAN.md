# Terminus Reactive Implementation Plan

This document outlines the widgets and functionality needed to port existing terminus-based apps to the reactive framework.

## Current State

The reactive framework (`Terminus/Reactive/`) provides:

**Display Components:**
- `text'`, `styledText'`, `dynText'` - Static and dynamic text
- `heading1'`, `heading2'`, `heading3'` - Themed headings
- `bodyText'`, `caption'`, `primaryText'` - Themed text variants
- `spacer'` - Empty space
- `progressBar'`, `dynProgressBar'` - Progress bars
- `when'`, `ifThenElse'`, `emitDynamic`, `dynWidget` - Conditional rendering

**Containers:**
- `row'`, `column'` - Flexbox layout
- `block'`, `titledBlock'` - Bordered containers
- `padded'`, `centered'` - Utility wrappers

**Hooks:**
- Key events: `useKeyEvent`, `useKey`, `useChar`, `useEnter`, `useEscape`, `useAnyChar`, `useArrowKeys`
- Mouse events: `useMouseEvent`, `useLeftClick`, `useRightClick`, `useScroll`, `useMousePosition`
- System: `useResize`, `useTick`, `useFrame`, `useElapsedMs`
- Focus: `useFocusedInput`, `setFocus`, `useIsFocused`
- State: `useCounter`, `useToggle`, `useLatest`

---

## Apps to Port

| App | Category | Key Features |
|-----|----------|--------------|
| **Blockfall** | Game | Tetris clone with animations, gravity, overlays |
| **Twenty48** | Game | 2048 with sliding/merging animations |
| **Minefield** | Game | Minesweeper with timer, cursor, grid |
| **Lighthouse** | Tool | Entity browser with tabs, panes, lists |
| **Enchiridion** | Tool | Novel writing with multi-panel editor, streaming AI |
| **Tracker** | Tool | Issue tracking with tree views, forms |
| **Timekeeper** | Tool | Time tracking with timers, forms, reports |

---

## Phase 1: Core Interactive Components

**Goal:** Enable basic interactive applications with text input and selection.

### 1.1 TextInput Widget

Single-line text input with cursor and focus management.

```lean
structure TextInputConfig where
  placeholder : String := ""
  maxLength : Option Nat := none
  style : Style := {}
  focusedStyle : Style := { fg := .ansi .cyan }

structure TextInputResult where
  value : Dynamic Spider String
  onSubmit : Event Spider String  -- Fires on Enter
  onCancel : Event Spider Unit    -- Fires on Escape

def textInput' (name : String) (initial : String := "")
    (config : TextInputConfig := {}) : WidgetM TextInputResult
```

**Features needed:**
- Cursor position tracking and rendering
- Character insertion/deletion (printable chars, backspace, delete)
- Cursor movement (left, right, home, end)
- Focus-aware styling
- Submit (Enter) and cancel (Escape) events

**Blocks:** Enchiridion, Tracker, Timekeeper

### 1.2 SelectableList Widget

Keyboard-navigable list with selection.

```lean
structure ListConfig where
  style : Style := {}
  selectedStyle : Style := { bg := .ansi .blue, fg := .ansi .white }
  maxVisible : Option Nat := none  -- For scrolling

structure ListResult (α : Type) where
  selectedIndex : Dynamic Spider Nat
  selectedItem : Dynamic Spider (Option α)
  onSelect : Event Spider α  -- Fires on Enter

def selectableList' [ToString α] (items : Array α) (initial : Nat := 0)
    (config : ListConfig := {}) : WidgetM (ListResult α)

-- Dynamic version for lists that change
def dynSelectableList' [ToString α] (items : Dynamic Spider (Array α))
    (initial : Nat := 0) (config : ListConfig := {}) : WidgetM (ListResult α)
```

**Features needed:**
- Arrow key navigation (up/down, j/k)
- Selection highlighting
- Enter to confirm selection
- Scroll viewport when list exceeds maxVisible

**Blocks:** Lighthouse, Tracker, Timekeeper

### 1.3 Overlay/Modal Container

Layer content on top of existing UI.

```lean
structure OverlayConfig where
  centerVertical : Bool := true
  centerHorizontal : Bool := true
  backdrop : Option Style := some { bg := .ansi .black }  -- Dim background

def overlay' (visible : Dynamic Spider Bool) (config : OverlayConfig := {})
    (content : WidgetM α) : WidgetM α

-- Convenience for modal dialogs
def modal' (visible : Dynamic Spider Bool) (title : String) (theme : Theme)
    (content : WidgetM α) : WidgetM α
```

**Features needed:**
- Conditional rendering based on visibility Dynamic
- Z-order rendering (overlay renders after base content)
- Optional backdrop/dimming
- Centering within viewport

**Blocks:** All games (pause/game-over), Enchiridion, Timekeeper

### Phase 1 Deliverables

- [ ] `Terminus/Reactive/Input.lean` - TextInput widget
- [ ] `Terminus/Reactive/List.lean` - SelectableList widget
- [ ] `Terminus/Reactive/Overlay.lean` - Overlay/Modal containers
- [ ] `examples/ReactiveInput.lean` - Demo app
- [ ] `Tests/ReactiveInputTests.lean` - Unit tests

---

## Phase 2: Navigation Components

**Goal:** Enable multi-view applications with tabs and tree navigation.

### 2.1 Tabs Widget

Tab selector with keyboard switching.

```lean
structure TabsConfig where
  style : Style := {}
  activeStyle : Style := { fg := .ansi .cyan, bold := true }
  separator : String := " | "

structure TabsResult where
  activeTab : Dynamic Spider Nat
  onTabChange : Event Spider Nat

def tabs' (labels : Array String) (initial : Nat := 0)
    (config : TabsConfig := {}) : WidgetM TabsResult

-- With number key shortcuts (1-9)
def numberedTabs' (labels : Array String) (initial : Nat := 0)
    (config : TabsConfig := {}) : WidgetM TabsResult
```

**Features needed:**
- Visual tab display with active indicator
- Tab/Shift+Tab navigation
- Number key shortcuts (1, 2, 3...)
- Tab change events

**Used by:** Lighthouse, Tracker, Timekeeper

### 2.2 Tree Widget

Expandable/collapsible tree structure.

```lean
structure TreeNode (α : Type) where
  value : α
  children : Array (TreeNode α) := #[]
  expanded : Bool := true

structure TreeConfig where
  style : Style := {}
  selectedStyle : Style := { bg := .ansi .blue }
  expandedIcon : String := "▼"
  collapsedIcon : String := "▶"
  leafIcon : String := "•"
  indent : Nat := 2

structure TreeResult (α : Type) where
  selectedPath : Dynamic Spider (Array Nat)  -- Path of indices
  selectedNode : Dynamic Spider (Option α)
  onSelect : Event Spider α
  onToggle : Event Spider (Array Nat)  -- Path that was toggled

def tree' [ToString α] (root : TreeNode α) (config : TreeConfig := {})
    : WidgetM (TreeResult α)

def dynTree' [ToString α] (root : Dynamic Spider (TreeNode α))
    (config : TreeConfig := {}) : WidgetM (TreeResult α)
```

**Features needed:**
- Visual tree with indentation
- Expand/collapse icons
- Arrow key navigation (up/down for siblings, left to collapse, right to expand)
- Enter/Space to toggle or select

**Used by:** Tracker, Enchiridion

### Phase 2 Deliverables

- [ ] `Terminus/Reactive/Tabs.lean` - Tabs widget
- [ ] `Terminus/Reactive/Tree.lean` - Tree widget
- [ ] `examples/ReactiveNav.lean` - Navigation demo
- [ ] `Tests/ReactiveNavTests.lean` - Unit tests

---

## Phase 3: Game Support

**Goal:** Enable game applications with grid rendering and animations.

### 3.1 Grid Widget

Fixed-size grid for game boards.

```lean
structure GridConfig where
  cellWidth : Nat := 2   -- Characters per cell
  cellHeight : Nat := 1  -- Lines per cell
  borderType : BorderType := .none
  borderStyle : Style := {}

def grid' (width height : Nat)
    (renderCell : Nat → Nat → WidgetM Unit)  -- x, y -> cell content
    (config : GridConfig := {}) : WidgetM Unit

-- Dynamic version where cells update reactively
def dynGrid' (width height : Nat)
    (cellContent : Dynamic Spider (Nat → Nat → RNode))
    (config : GridConfig := {}) : WidgetM Unit
```

**Features needed:**
- Fixed cell dimensions
- Cell-by-cell rendering with custom content
- Optional border around grid
- Coordinate system matching game logic

**Used by:** Blockfall, Twenty48, Minefield

### 3.2 Animation Utilities

Helpers for frame-based animations.

```lean
-- Interpolate between values over time
def useAnimation (duration : Nat) (from to : Float)
    (trigger : Event Spider Unit) : ReactiveTermM (Dynamic Spider Float)

-- Multi-phase animation state machine
inductive AnimPhase where
  | idle
  | phase (name : String) (progress : Float)

def useAnimationPhases (phases : Array (String × Nat))  -- name, duration ms
    (trigger : Event Spider String)  -- trigger phase by name
    : ReactiveTermM (Dynamic Spider AnimPhase)

-- Convenience for common patterns
def usePulse (intervalMs : Nat) : ReactiveTermM (Dynamic Spider Bool)
def useCycle (periodMs : Nat) : ReactiveTermM (Dynamic Spider Float)  -- 0.0 to 1.0
```

**Features needed:**
- Time-based interpolation
- Phase state machine for multi-step animations
- Integration with `useTick` for frame timing

**Used by:** Blockfall (drop trail, lock flash), Twenty48 (slide, merge, spawn)

### 3.3 Cursor/Highlight Support

Highlight specific cells or regions.

```lean
structure CursorConfig where
  style : Style := { bg := .ansi .cyan }
  blinkMs : Option Nat := none  -- Optional blink interval

def useCursor (initial : (Nat × Nat))
    : ReactiveTermM (Dynamic Spider (Nat × Nat) × (Nat × Nat → IO Unit))

-- Highlight a cell in a grid
def highlightCell' (pos : Dynamic Spider (Nat × Nat)) (config : CursorConfig := {})
    (content : WidgetM Unit) : WidgetM Unit
```

**Used by:** Minefield, Lighthouse, Tracker

### Phase 3 Deliverables

- [ ] `Terminus/Reactive/Grid.lean` - Grid widget
- [ ] `Terminus/Reactive/Animation.lean` - Animation utilities
- [ ] `examples/ReactiveGame.lean` - Simple game demo
- [ ] `Tests/ReactiveGameTests.lean` - Unit tests

---

## Phase 4: Advanced Editor Components

**Goal:** Enable rich text editing applications.

### 4.1 TextArea Widget

Multi-line text editor with scrolling.

```lean
structure TextAreaConfig where
  showLineNumbers : Bool := true
  lineNumberStyle : Style := { fg := .ansi .brightBlack }
  style : Style := {}
  focusedStyle : Style := {}
  wrapLines : Bool := false

structure TextAreaResult where
  content : Dynamic Spider String
  cursorPos : Dynamic Spider (Nat × Nat)  -- line, column
  onChange : Event Spider String
  onSubmit : Event Spider String  -- Ctrl+Enter

def textArea' (name : String) (initial : String := "")
    (config : TextAreaConfig := {}) : WidgetM TextAreaResult
```

**Features needed:**
- Multi-line text storage and rendering
- Cursor movement (arrows, home, end, page up/down)
- Line wrapping (optional)
- Scrolling viewport
- Line numbers
- Insert/delete at cursor

**Used by:** Enchiridion

### 4.2 Form Components

Structured form with validation.

```lean
structure FormFieldConfig where
  label : String
  required : Bool := false
  validate : String → Option String  -- Returns error message if invalid

structure FormResult where
  values : Dynamic Spider (Array (String × String))
  isValid : Dynamic Spider Bool
  onSubmit : Event Spider (Array (String × String))

def form' (fields : Array FormFieldConfig) (onSubmit : Event Spider Unit)
    : WidgetM FormResult

-- Individual field helpers
def labeledInput' (label : String) (name : String) (initial : String := "")
    : WidgetM (Dynamic Spider String)

def optionSelector' (label : String) (options : Array String) (initial : Nat := 0)
    : WidgetM (Dynamic Spider Nat)
```

**Used by:** Tracker, Timekeeper

### 4.3 Confirmation Dialog

Standard yes/no confirmation.

```lean
structure ConfirmResult where
  confirmed : Event Spider Unit
  cancelled : Event Spider Unit

def confirmDialog' (message : String) (visible : Dynamic Spider Bool)
    (theme : Theme) : WidgetM ConfirmResult
```

**Used by:** Enchiridion, Timekeeper

### Phase 4 Deliverables

- [ ] `Terminus/Reactive/TextArea.lean` - Multi-line editor
- [ ] `Terminus/Reactive/Form.lean` - Form components
- [ ] `Terminus/Reactive/Dialog.lean` - Confirmation dialogs
- [ ] `examples/ReactiveEditor.lean` - Editor demo
- [ ] `Tests/ReactiveEditorTests.lean` - Unit tests

---

## Phase 5: Streaming and Advanced Patterns

**Goal:** Support async operations and advanced UI patterns.

### 5.1 Async/Streaming Support

Handle long-running operations with progress.

```lean
-- Run IO action and stream results
def useAsync (action : IO α) (trigger : Event Spider Unit)
    : ReactiveTermM (Dynamic Spider (Option α) × Dynamic Spider Bool)  -- result, loading

-- Stream chunks as they arrive
def useStream (stream : IO (Array String)) (trigger : Event Spider Unit)
    : ReactiveTermM (Dynamic Spider (Array String))
```

**Used by:** Enchiridion (AI streaming)

### 5.2 Multi-Pane Layout

Split layouts with resizable panes.

```lean
structure PaneConfig where
  minSize : Nat := 10
  initialSize : Nat := 50  -- Percentage or fixed

def horizontalSplit' (leftSize : Nat)
    (left : WidgetM α) (right : WidgetM β) : WidgetM (α × β)

def verticalSplit' (topSize : Nat)
    (top : WidgetM α) (bottom : WidgetM β) : WidgetM (α × β)
```

**Used by:** Lighthouse, Enchiridion

### Phase 5 Deliverables

- [ ] `Terminus/Reactive/Async.lean` - Async utilities
- [ ] `Terminus/Reactive/Layout.lean` - Split layouts
- [ ] `examples/ReactiveAsync.lean` - Streaming demo

---

## Summary: App Readiness by Phase

| App | Phase 1 | Phase 2 | Phase 3 | Phase 4 | Phase 5 |
|-----|---------|---------|---------|---------|---------|
| **Blockfall** | | | Grid, Animation | | |
| **Twenty48** | | | Grid, Animation | | |
| **Minefield** | | | Grid, Cursor | | |
| **Lighthouse** | List | Tabs | | | Split |
| **Enchiridion** | TextInput, Modal | Tree | | TextArea | Stream, Split |
| **Tracker** | TextInput, List | Tabs, Tree | | Form | |
| **Timekeeper** | TextInput, List, Modal | Tabs | | Form | |

**Minimum to port first app:**
- Phase 1 (TextInput, List, Modal) enables: Timekeeper (simplest tool app)
- Phase 3 (Grid) enables: Minefield (simplest game)

---

## Testing Strategy

Each phase includes:
1. **Unit tests** - Component behavior in isolation
2. **Integration tests** - Components working together
3. **Demo app** - Visual verification of rendering

Test patterns:
- Mock event sources with `LoopDeps`
- Buffer capture for visual assertions
- Event injection for interaction testing

---

## Migration Path

For each app:
1. Create `examples/<App>Reactive.lean` alongside existing app
2. Port incrementally (start with static display, add interactivity)
3. Compare behavior with original
4. Once complete, replace original with reactive version
