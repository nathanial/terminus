-- TerminusTests.Reactive.ReproOverlayRendering: Reproduction of overlay z-ordering issue

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.ReproOverlayRendering

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Overlay Z-Ordering Reproduction"

test "Overlay should be rendered ON TOP of subsequent siblings" := do
  runSpider do
    let (events, _) ← createInputs
    let theme := Theme.dark
    -- Make overlay always visible
    let (visEvent, _trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn true visEvent

    let (_, render) ← (runWidget do
      -- Container
      column' (gap := 0) {} do
        -- 1. The Overlay Node
        -- Base is empty (height 0).
        -- Overlay content will be centered.
        -- In a 10x5 screen:
        -- Center y = 2.
        overlayWhen' visible {} do
           text' "OVERLAY" theme.bodyStyle

        -- 2. Spacer to push sibling down to y=2
        -- Overlay occupies y=0 (height 1).
        -- We want Sibling at y=2.
        -- So we need height 1 spacer at y=1.
        spacer' 1 1

        -- 3. The Sibling Node
        -- Should render at y=2.
        -- "SIBLING"
        text' "SIBLING" theme.bodyStyle

      pure ()
    ).run events

    let node ← SpiderM.liftIO render.sample
    -- Render to a 10x5 buffer
    -- "OVERLAY" is length 7. "SIBLING" is length 7.
    -- Both centered horizontally? No.
    -- Overlay is centered horizontally: (10 - 7) / 2 = 1. x=1.
    -- Sibling: column, x=0.
    -- Overlap: "OVERLAY" is at x=1..7. "SIBLING" is at x=0..6.
    -- They overlap significantly.
    -- At (x=1, y=2), Overlay has 'O'. Sibling has 'I'.
    -- If Overlay is on top, we see 'O'.
    -- If Sibling is on top (bug), we see 'I'.

    let buf := Terminus.Reactive.renderOnly node 10 5

    -- Check (1, 2)
    let cell := buf.get 1 2
    let char := cell.char

    -- In the buggy version, this assertion should FAIL (showing 'I' instead of 'O')
    SpiderM.liftIO (ensure (char == 'O') s!"expected 'O' from OVERLAY at (1,2) but got '{char}' (likely from SIBLING)")



end TerminusTests.Reactive.ReproOverlayRendering
