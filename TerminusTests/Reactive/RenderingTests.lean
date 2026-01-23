-- TerminusTests.Reactive.RenderingTests: RNode rendering and dynamic content tests

import Crucible
import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import TerminusTests.Reactive.Common

namespace TerminusTests.Reactive.RenderingTests

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host
open TerminusTests.Reactive.Common

testSuite "Reactive Rendering Tests"

-- ============================================================================
-- RNode Rendering Tests
-- ============================================================================

test "render text node to buffer" := do
  let node := RNode.text "Hello" {}
  let buf := Terminus.Reactive.renderOnly node 20 5
  -- Text should appear at position (0, 0)
  (buf.get 0 0).char ≡ 'H'
  (buf.get 1 0).char ≡ 'e'
  (buf.get 2 0).char ≡ 'l'
  (buf.get 3 0).char ≡ 'l'
  (buf.get 4 0).char ≡ 'o'

test "render column with multiple text nodes" := do
  let children := #[
    RNode.text "Line1" {},
    RNode.text "Line2" {}
  ]
  let node := RNode.column 0 {} children
  let buf := Terminus.Reactive.renderOnly node 20 5
  -- Line1 at row 0
  (buf.get 0 0).char ≡ 'L'
  (buf.get 1 0).char ≡ 'i'
  -- Line2 at row 1
  (buf.get 0 1).char ≡ 'L'
  (buf.get 1 1).char ≡ 'i'

test "render row with multiple text nodes" := do
  let children := #[
    RNode.text "A" {},
    RNode.text "B" {}
  ]
  let node := RNode.row 0 {} children
  let buf := Terminus.Reactive.renderOnly node 20 5
  -- A at column 0
  (buf.get 0 0).char ≡ 'A'
  -- B at column 1
  (buf.get 1 0).char ≡ 'B'

test "render empty node produces empty buffer" := do
  let node := RNode.empty
  let buf := Terminus.Reactive.renderOnly node 10 5
  (buf.get 0 0).char ≡ ' '
  (buf.get 5 2).char ≡ ' '

-- ============================================================================
-- Dynamic Sampling Tests
-- ============================================================================

test "ComponentRender samples stable dynamic" := do
  runSpider do
    let render ← Dynamic.pureM (RNode.text "Stable" {})
    let node1 ← SpiderM.liftIO render.sample
    let node2 ← SpiderM.liftIO render.sample
    match node1, node2 with
    | .text c1 _, .text c2 _ =>
      SpiderM.liftIO (c1 ≡ "Stable")
      SpiderM.liftIO (c2 ≡ "Stable")
    | _, _ => SpiderM.liftIO (ensure false "expected text nodes")

test "column combines static and dynamic children" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 event

    let (_, render) ← (runWidget do
      column' (gap := 0) (style := {}) do
        text' "Static" {}
        let node ← countDyn.map' (fun n => RNode.text s!"Value: {n}" {})
        emit node
    ).run events

    let node1 ← SpiderM.liftIO render.sample
    SpiderM.liftIO (trigger ())
    let node2 ← SpiderM.liftIO render.sample

    match node1, node2 with
    | .column _ _ children1, .column _ _ children2 =>
      if h1 : 1 < children1.size then
        if h2 : 1 < children2.size then
          match children1[1], children2[1] with
          | .text c1 _, .text c2 _ =>
            SpiderM.liftIO (c1 ≡ "Value: 0")
            SpiderM.liftIO (c2 ≡ "Value: 1")
          | _, _ => SpiderM.liftIO (ensure false "expected text nodes")
        else SpiderM.liftIO (ensure false "expected 2 children in node2")
      else SpiderM.liftIO (ensure false "expected 2 children in node1")
    | _, _ => SpiderM.liftIO (ensure false "expected column nodes")

-- ============================================================================
-- Integration with Reactive library
-- ============================================================================

test "foldDyn updates value on event" := do
  runSpider do
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 event

    -- Initial value
    let v0 ← SpiderM.liftIO countDyn.sample
    SpiderM.liftIO (v0 ≡ 0)

    -- Fire event
    SpiderM.liftIO (trigger ())
    let v1 ← SpiderM.liftIO countDyn.sample
    SpiderM.liftIO (v1 ≡ 1)

    -- Fire again
    SpiderM.liftIO (trigger ())
    let v2 ← SpiderM.liftIO countDyn.sample
    SpiderM.liftIO (v2 ≡ 2)

test "holdDyn holds last event value" := do
  runSpider do
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := String)
    let textDyn ← Reactive.holdDyn "initial" event

    -- Initial value
    let v0 ← SpiderM.liftIO textDyn.sample
    SpiderM.liftIO (v0 ≡ "initial")

    -- Fire event with new value
    SpiderM.liftIO (trigger "updated")
    let v1 ← SpiderM.liftIO textDyn.sample
    SpiderM.liftIO (v1 ≡ "updated")

-- ============================================================================
-- Dynamic content tests - THE KEY TESTS
-- ============================================================================

test "emitDynamic samples dynamic on each render call" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 event

    let (_, render) ← (runWidget do
      let node ← countDyn.map' (fun n => RNode.text s!"Count: {n}" {})
      emit node
    ).run events

    -- First render
    let node1 ← SpiderM.liftIO render.sample
    match node1 with
    | .text content _ => SpiderM.liftIO (content ≡ "Count: 0")
    | _ => SpiderM.liftIO (ensure false "expected text node")

    -- Trigger update
    SpiderM.liftIO (trigger ())

    -- Second render (should reflect update)
    let node2 ← SpiderM.liftIO render.sample
    match node2 with
    | .text content _ => SpiderM.liftIO (content ≡ "Count: 1")
    | _ => SpiderM.liftIO (ensure false "expected text node")

test "emitDynamic inside column samples on each render" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 event

    let (_, render) ← (runWidget do
      column' (gap := 0) (style := {}) do
        text' "Header" {}
        let node ← countDyn.map' (fun n => RNode.text s!"Dynamic: {n}" {})
        emit node
    ).run events

    -- First render
    let node1 ← SpiderM.liftIO render.sample
    -- Trigger update
    SpiderM.liftIO (trigger ())
    -- Second render
    let node2 ← SpiderM.liftIO render.sample

    -- Extract the dynamic child from each
    match node1, node2 with
    | .column _ _ children1, .column _ _ children2 =>
      if h1 : 1 < children1.size then
        if h2 : 1 < children2.size then
          match children1[1], children2[1] with
          | .text c1 _, .text c2 _ =>
            SpiderM.liftIO (c1 ≡ "Dynamic: 0")
            SpiderM.liftIO (c2 ≡ "Dynamic: 1")
          | _, _ => SpiderM.liftIO (ensure false "expected text nodes")
        else SpiderM.liftIO (ensure false "expected 2 children in node2")
      else SpiderM.liftIO (ensure false "expected 2 children in node1")
    | _, _ => SpiderM.liftIO (ensure false "expected column nodes")

test "Dynamic value reflected in emitDynamic render" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 event

    let (_, render) ← (runWidget do
      let node ← countDyn.map' (fun n => RNode.text s!"Count: {n}" {})
      emit node
    ).run events

    -- Initial render
    let node1 ← SpiderM.liftIO render.sample
    match node1 with
    | .text content _ => SpiderM.liftIO (content ≡ "Count: 0")
    | _ => SpiderM.liftIO (ensure false "expected text node")

    -- Fire event to update Dynamic
    SpiderM.liftIO (trigger ())

    -- Render again - should show updated value
    let node2 ← SpiderM.liftIO render.sample
    match node2 with
    | .text content _ => SpiderM.liftIO (content ≡ "Count: 1")
    | _ => SpiderM.liftIO (ensure false "expected text node")

-- ============================================================================
-- Buffer rendering with dynamic content
-- ============================================================================

test "buffer content changes when dynamic value changes" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 event

    let (_, render) ← (runWidget do
      let node ← countDyn.map' (fun n => RNode.text s!"{n}" {})
      emit node
    ).run events

    -- First render to buffer
    let node1 ← SpiderM.liftIO render.sample
    let buf1 := Terminus.Reactive.renderOnly node1 10 5

    -- Fire event
    SpiderM.liftIO (trigger ())

    -- Second render to buffer
    let node2 ← SpiderM.liftIO render.sample
    let buf2 := Terminus.Reactive.renderOnly node2 10 5

    -- Buffers should differ
    let cell1 := buf1.get 0 0
    let cell2 := buf2.get 0 0
    SpiderM.liftIO (cell1.char ≡ '0')
    SpiderM.liftIO (cell2.char ≡ '1')

-- ============================================================================
-- Progress bar rendering regression tests
-- ============================================================================

test "dynProgressBar updates buffer when progress changes" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Float)
    let progressDyn ← Reactive.holdDyn 0.0 event

    let (_, render) ← (runWidget do
      dynProgressBar' progressDyn {
        width := 5
        filledChar := '#'
        emptyChar := '-'
        showPercentage := false
      }
    ).run events

    let node1 ← SpiderM.liftIO render.sample
    let buf1 := Terminus.Reactive.renderOnly node1 10 2

    SpiderM.liftIO (trigger 0.6)

    let node2 ← SpiderM.liftIO render.sample
    let buf2 := Terminus.Reactive.renderOnly node2 10 2

    -- Initial progress = 0.0 -> "-----"
    (buf1.get 0 0).char ≡ '-'
    (buf1.get 1 0).char ≡ '-'
    (buf1.get 2 0).char ≡ '-'
    (buf1.get 3 0).char ≡ '-'
    (buf1.get 4 0).char ≡ '-'

    -- Progress = 0.6 -> filledCount = 3 -> "###--"
    (buf2.get 0 0).char ≡ '#'
    (buf2.get 1 0).char ≡ '#'
    (buf2.get 2 0).char ≡ '#'
    (buf2.get 3 0).char ≡ '-'
    (buf2.get 4 0).char ≡ '-'

-- ============================================================================
-- Row with dynamic content buffer tests
-- ============================================================================

test "row' with emitDynamic updates buffer" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 event

    let (_, render) ← (runWidget do
      row' (gap := 1) (style := {}) do
        text' "Count:" {}
        let node ← countDyn.map' (fun n => RNode.text s!"{n}" {})
        emit node
    ).run events

    let node1 ← SpiderM.liftIO render.sample
    let buf1 := Terminus.Reactive.renderOnly node1 20 2

    -- Check initial buffer has "Count:" and "0"
    (buf1.get 0 0).char ≡ 'C'
    (buf1.get 6 0).char ≡ ' '  -- gap
    (buf1.get 7 0).char ≡ '0'

    SpiderM.liftIO (trigger ())

    let node2 ← SpiderM.liftIO render.sample
    let buf2 := Terminus.Reactive.renderOnly node2 20 2

    -- Check updated buffer has "1"
    (buf2.get 7 0).char ≡ '1'

    -- Verify buffers differ
    let diff := Buffer.diff buf1 buf2
    SpiderM.liftIO (ensure (!diff.isEmpty) "expected buffer to differ")

test "column' with row' containing emitDynamic updates buffer" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 event

    let (_, render) ← (runWidget do
      column' (gap := 1) (style := {}) do
        text' "Header" {}
        row' (gap := 1) (style := {}) do
          text' "Count:" {}
          let node ← countDyn.map' (fun n => RNode.text s!"{n}" {})
          emit node
    ).run events

    let node1 ← SpiderM.liftIO render.sample
    let buf1 := Terminus.Reactive.renderOnly node1 20 5

    -- Header on row 0
    (buf1.get 0 0).char ≡ 'H'

    SpiderM.liftIO (trigger ())

    let node2 ← SpiderM.liftIO render.sample
    let buf2 := Terminus.Reactive.renderOnly node2 20 5

    -- Verify buffers differ
    let diff := Buffer.diff buf1 buf2
    SpiderM.liftIO (ensure (!diff.isEmpty) "expected buffer to differ after count change")



end TerminusTests.Reactive.RenderingTests
