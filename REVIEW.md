# Review Findings

## High
- Raw mode teardown isn’t exception-safe: `Terminus/Backend/TerminalEffect.lean:31` doesn’t use `try/finally`, and `Terminus/Backend/Terminal.lean:224` plus `Terminus/Frame.lean:153` run setup/`Terminal.new` before entering the `try`, so failures can leave raw mode or the alt screen active.

## Medium
- Command de-dup keys are too weak: clipboard keys only include text length (`Terminus/Backend/Commands.lean:84`), and image keys for `.path` ignore file content (`Terminus/Backend/Commands.lean:22`), so `applyCommands` can skip updates when content changes but key doesn’t (`Terminus/Backend/Terminal.lean:127`).
- `Events.available` consumes a byte (`Terminus/Input/Events.lean:217`), so calling it drops input and can corrupt escape-sequence parsing.
- Layout sizing semantics can overflow/ignore constraints: `.min` uses `Nat.max` (`Terminus/Layout/Constraint.lean:32`), `.max` is treated like fill and ratio denom is ignored (`Terminus/Layout/Layout.lean:85`), and fixed/percent columns can exceed total width in tables (`Terminus/Widgets/Table.lean:115`).

## Low
- Line-number padding can overflow into the text area when the line number is wider than `lineNumberWidth` (negative padding yields a longer string) (`Terminus/Widgets/TextArea.lean:498`).
- Build/test emits unused-variable warnings in several widgets/tests (e.g., `Terminus/Widgets/Popup.lean:101`, `Terminus/Widgets/BarChart.lean:88`, `Tests/Main.lean:585`), which can mask real lint findings over time.
