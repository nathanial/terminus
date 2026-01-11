# Terminus Legacy Widget Removal Plan

This document outlines the plan to remove legacy (non-reactive) widgets from Terminus, keeping only the FRP-based reactive framework.

## Goal

Remove all legacy widget code, keeping only:
- Core infrastructure (Style, Buffer, Terminal, Input, etc.)
- Reactive framework (`Terminus/Reactive/`)
- Reactive examples and tests

## Files to KEEP

### Core Infrastructure
```
Terminus/Core/Base64.lean
Terminus/Core/Cell.lean
Terminus/Core/Buffer.lean
Terminus/Core/Focus.lean
Terminus/Core/Rect.lean
Terminus/Core/Sixel.lean
Terminus/Core/Style.lean
Terminus/Core/Unicode.lean
```

### Backend
```
Terminus/Backend/Ansi.lean
Terminus/Backend/Commands.lean
Terminus/Backend/Raw.lean
Terminus/Backend/Terminal.lean
Terminus/Backend/TerminalEffect.lean
Terminus/Backend/TerminalIO.lean
Terminus/Backend/TerminalMock.lean
```

### Input
```
Terminus/Input/Events.lean
Terminus/Input/Key.lean
```

### Layout
```
Terminus/Layout/Constraint.lean
Terminus/Layout/Layout.lean
```

### Frame
```
Terminus/Frame.lean
```

### Reactive (all 25 files)
```
Terminus/Reactive.lean
Terminus/Reactive/*.lean
Terminus/Reactive/Demos/*.lean
```

### Examples (reactive only)
```
examples/ReactiveAsync.lean
examples/ReactiveDemo.lean
examples/ReactiveEditor.lean
examples/ReactiveGame.lean
examples/ReactiveInput.lean
examples/ReactiveNav.lean
```

### Tests to Keep
```
TerminusTests/RawModeTests.lean
TerminusTests/TerminalSizeTests.lean
TerminusTests/InputTests.lean
TerminusTests/OutputTests.lean
TerminusTests/CellTests.lean
TerminusTests/BufferTests.lean
TerminusTests/RectTests.lean
TerminusTests/StyleTests.lean
TerminusTests/LayoutTests.lean
TerminusTests/Reactive/*.lean (all 15 files)
```

## Files to DELETE

### Widgets (29 files)
```
Terminus/Widgets/Widget.lean
Terminus/Widgets/Clear.lean
Terminus/Widgets/Block.lean
Terminus/Widgets/Paragraph.lean
Terminus/Widgets/List.lean
Terminus/Widgets/Table.lean
Terminus/Widgets/Gauge.lean
Terminus/Widgets/LineGauge.lean
Terminus/Widgets/Tabs.lean
Terminus/Widgets/Sparkline.lean
Terminus/Widgets/Scrollbar.lean
Terminus/Widgets/Spinner.lean
Terminus/Widgets/Popup.lean
Terminus/Widgets/BarChart.lean
Terminus/Widgets/Tree.lean
Terminus/Widgets/Calendar.lean
Terminus/Widgets/TextInput.lean
Terminus/Widgets/Canvas.lean
Terminus/Widgets/LineChart.lean
Terminus/Widgets/TextArea.lean
Terminus/Widgets/BigText.lean
Terminus/Widgets/Checkbox.lean
Terminus/Widgets/Menu.lean
Terminus/Widgets/PieChart.lean
Terminus/Widgets/ScrollView.lean
Terminus/Widgets/Logger.lean
Terminus/Widgets/Image.lean
Terminus/Widgets/Form.lean
Terminus/Widgets/Notification.lean
Terminus/Widgets/RadioGroup.lean
```

### Legacy Examples (14 files)
```
examples/BigText.lean
examples/Charts.lean
examples/Counter.lean
examples/Dashboard.lean
examples/FileExplorer.lean
examples/Form.lean
examples/HelloWorld.lean
examples/Image.lean
examples/KitchenSink.lean
examples/Logger.lean
examples/Menu.lean
examples/MouseDemo.lean
examples/PieChart.lean
examples/TextEditor.lean
```

### Legacy Tests (widget-specific)
```
TerminusTests/WidgetTests.lean
TerminusTests/TableTests.lean
TerminusTests/BarChartTests.lean
TerminusTests/CheckboxTests.lean
TerminusTests/RadioGroupTests.lean
TerminusTests/SpinnerTests.lean
TerminusTests/FormTests.lean
TerminusTests/FocusTests.lean
TerminusTests/LineChartTests.lean
TerminusTests/PieChartTests.lean
TerminusTests/SparklineTests.lean
TerminusTests/LineGaugeTests.lean
TerminusTests/CalendarTests.lean
TerminusTests/MenuTests.lean
TerminusTests/TreeTests.lean
TerminusTests/PopupTests.lean
TerminusTests/ClearTests.lean
TerminusTests/ScrollbarTests.lean
TerminusTests/ScrollViewTests.lean
TerminusTests/LoggerTests.lean
TerminusTests/BigTextTests.lean
TerminusTests/CanvasTests.lean
TerminusTests/ImageTests.lean
```

## Files to MODIFY

### Terminus.lean
Remove all widget imports, keep only core imports.

### lakefile.lean
Remove all legacy executable targets, keep only reactive demos.

### TerminusTests/Main.lean
Remove all legacy test imports, keep only core + reactive tests.

## Execution Order

1. Delete `Terminus/Widgets/` directory
2. Delete legacy examples
3. Delete legacy tests
4. Update `Terminus.lean`
5. Update `lakefile.lean`
6. Update `TerminusTests/Main.lean`
7. Build and test
8. Commit changes

## Verification

After cleanup:
```bash
lake build
lake test
lake exe reactive_demo
```

All reactive demos should still work:
- `lake exe reactive_demo`
- `lake exe reactive_input`
- `lake exe reactive_nav`
- `lake exe reactive_game`
- `lake exe reactive_editor`
- `lake exe reactive_async`
