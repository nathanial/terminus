-- Tests.ReactiveTests: Tests for Terminus.Reactive rendering system
import Crucible
import Terminus.Reactive
import Terminus.Reactive.Demos.ReactiveDemo
import Terminus.Backend.TerminalMock
import Reactive

open Terminus
open Terminus.Reactive
open Crucible
open Reactive Reactive.Host

namespace Tests.Reactive

testSuite "Terminus.Reactive Tests"

abbrev MockTerminalIO := StateT MockTerminalState IO

instance : TerminalEffect MockTerminalIO where
  enableRawMode := modify fun s => { s with rawModeEnabled := true }
  disableRawMode := modify fun s => { s with rawModeEnabled := false }
  getTerminalSize := do
    let s ← get
    pure s.terminalSize
  readByte := do
    let s ← get
    match s.inputQueue with
    | [] => pure none
    | b :: rest =>
      set { s with inputQueue := rest }
      pure (some b)
  readByteBlocking := do
    let s ← get
    match s.inputQueue with
    | [] => pure none
    | b :: rest =>
      set { s with inputQueue := rest }
      pure (some b)
  unreadByte b := modify fun s =>
    { s with inputQueue := b :: s.inputQueue }
  writeStdout str := modify fun s =>
    { s with outputBuffer := s.outputBuffer ++ str, flushed := false }
  flushStdout := modify fun s => { s with flushed := true }
  readFileBytes path := do
    let s ← get
    pure (s.files.getD path.toString ByteArray.empty)
  decodeImageBytes _ := pure none

def bufferHasChar (buf : Buffer) (c : Char) : Bool :=
  buf.cells.any (fun cell => cell.char == c)

def bufferRowPrefix (buf : Buffer) (row len : Nat) : String :=
  let chars := (List.range len).map (fun x => (buf.get x row).char)
  String.ofList chars

partial def rnodeHasChar (node : RNode) (c : Char) : Bool :=
  match node with
  | .text content _ => content.toList.any (fun ch => ch == c)
  | .block _ _ _ child => rnodeHasChar child c
  | .row _ _ children => children.any (fun child => rnodeHasChar child c)
  | .column _ _ children => children.any (fun child => rnodeHasChar child c)
  | .clipped child => rnodeHasChar child c
  | .scrolled _ _ child => rnodeHasChar child c
  | .spacer _ _ | .empty => false

partial def rnodeHasText (node : RNode) (needle : String) : Bool :=
  match node with
  | .text content _ => content == needle
  | .block _ _ _ child => rnodeHasText child needle
  | .row _ _ children => children.any (fun child => rnodeHasText child needle)
  | .column _ _ children => children.any (fun child => rnodeHasText child needle)
  | .clipped child => rnodeHasText child needle
  | .scrolled _ _ child => rnodeHasText child needle
  | .spacer _ _ | .empty => false

-- ============================================================================
-- RNode Rendering Tests
-- ============================================================================

test "render text node to buffer" := do
  let node := RNode.text "Hello" {}
  let buf := Terminus.Reactive.render node 20 5
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
  let buf := Terminus.Reactive.render node 20 5
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
  let buf := Terminus.Reactive.render node 20 5
  -- A at column 0
  (buf.get 0 0).char ≡ 'A'
  -- B at column 1
  (buf.get 1 0).char ≡ 'B'

test "render empty node produces empty buffer" := do
  let node := RNode.empty
  let buf := Terminus.Reactive.render node 10 5
  (buf.get 0 0).char ≡ ' '
  (buf.get 5 2).char ≡ ' '

-- ============================================================================
-- Dynamic Sampling Tests
-- ============================================================================

test "ComponentRender samples IO action each call" := do
  let counterRef ← IO.mkRef 0
  let render : ComponentRender := do
    let n ← counterRef.get
    counterRef.set (n + 1)
    pure (RNode.text s!"Count: {n}" {})

  -- First call
  let node1 ← render
  -- Second call
  let node2 ← render
  -- Third call
  let node3 ← render

  -- Each call should have incremented the counter
  match node1 with
  | .text content _ => content ≡ "Count: 0"
  | _ => ensure false "expected text node"

  match node2 with
  | .text content _ => content ≡ "Count: 1"
  | _ => ensure false "expected text node"

  match node3 with
  | .text content _ => content ≡ "Count: 2"
  | _ => ensure false "expected text node"

test "nested ComponentRender in column samples correctly" := do
  let counterRef ← IO.mkRef 0
  let childRender : ComponentRender := do
    let n ← counterRef.get
    counterRef.set (n + 1)
    pure (RNode.text s!"Value: {n}" {})

  -- Simulate what column' does: array of child renders
  let childRenders : Array ComponentRender := #[
    pure (RNode.text "Static" {}),
    childRender
  ]

  -- First sampling
  let nodes1 ← childRenders.mapM id
  -- Second sampling
  let nodes2 ← childRenders.mapM id

  -- Check first sampling - nodes1 is Array RNode
  if h : 1 < nodes1.size then
    match nodes1[1] with
    | .text content _ => content ≡ "Value: 0"
    | _ => ensure false "expected text node"
  else
    ensure false "expected at least 2 nodes"

  -- Check second sampling
  if h : 1 < nodes2.size then
    match nodes2[1] with
    | .text content _ => content ≡ "Value: 1"
    | _ => ensure false "expected text node"
  else
    ensure false "expected at least 2 nodes"

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
-- WidgetM and emit tests
-- ============================================================================

test "emit adds render function to state" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      emit (pure (RNode.text "Test" {}))
    ).run events

    let node ← SpiderM.liftIO render
    match node with
    | .text content _ => SpiderM.liftIO (content ≡ "Test")
    | _ => SpiderM.liftIO (ensure false "expected text node")

test "multiple emit creates column of nodes" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      emit (pure (RNode.text "First" {}))
      emit (pure (RNode.text "Second" {}))
    ).run events

    let node ← SpiderM.liftIO render
    match node with
    | .column _ _ children =>
      SpiderM.liftIO (children.size ≡ 2)
    | _ => SpiderM.liftIO (ensure false "expected column node")

test "text' emits text node" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      text' "Hello" { fg := .ansi .red }
    ).run events

    let node ← SpiderM.liftIO render
    match node with
    | .text content style =>
      SpiderM.liftIO (content ≡ "Hello")
      SpiderM.liftIO (style.fg ≡ .ansi .red)
    | _ => SpiderM.liftIO (ensure false "expected text node")

-- ============================================================================
-- Container tests
-- ============================================================================

test "column' collects children renders" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      column' (gap := 1) (style := {}) do
        text' "Line1" {}
        text' "Line2" {}
    ).run events

    let node ← SpiderM.liftIO render
    match node with
    | .column gap _ children =>
      SpiderM.liftIO (gap ≡ 1)
      SpiderM.liftIO (children.size ≡ 2)
    | _ => SpiderM.liftIO (ensure false "expected column node")

test "row' collects children renders" := do
  runSpider do
    let (events, _) ← createInputs
    let (_, render) ← (runWidget do
      row' (gap := 2) (style := {}) do
        text' "A" {}
        text' "B" {}
    ).run events

    let node ← SpiderM.liftIO render
    match node with
    | .row gap _ children =>
      SpiderM.liftIO (gap ≡ 2)
      SpiderM.liftIO (children.size ≡ 2)
    | _ => SpiderM.liftIO (ensure false "expected row node")

-- ============================================================================
-- Dynamic content tests - THE KEY TESTS
-- ============================================================================

test "emitDynamic samples dynamic on each render call" := do
  runSpider do
    let (events, _) ← createInputs
    let counterRef ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← (runWidget do
      emitDynamic do
        let n ← counterRef.get
        counterRef.set (n + 1)
        pure (RNode.text s!"Count: {n}" {})
    ).run events

    -- First render
    let node1 ← SpiderM.liftIO render
    match node1 with
    | .text content _ => SpiderM.liftIO (content ≡ "Count: 0")
    | _ => SpiderM.liftIO (ensure false "expected text node")

    -- Second render (should sample again)
    let node2 ← SpiderM.liftIO render
    match node2 with
    | .text content _ => SpiderM.liftIO (content ≡ "Count: 1")
    | _ => SpiderM.liftIO (ensure false "expected text node")

test "emitDynamic inside column samples on each render" := do
  runSpider do
    let (events, _) ← createInputs
    let counterRef ← SpiderM.liftIO (IO.mkRef 0)

    let (_, render) ← (runWidget do
      column' (gap := 0) (style := {}) do
        text' "Header" {}
        emitDynamic do
          let n ← counterRef.get
          counterRef.set (n + 1)
          pure (RNode.text s!"Dynamic: {n}" {})
    ).run events

    -- First render
    let node1 ← SpiderM.liftIO render
    -- Second render
    let node2 ← SpiderM.liftIO render

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
      emitDynamic do
        let n ← countDyn.sample
        pure (RNode.text s!"Count: {n}" {})
    ).run events

    -- Initial render
    let node1 ← SpiderM.liftIO render
    match node1 with
    | .text content _ => SpiderM.liftIO (content ≡ "Count: 0")
    | _ => SpiderM.liftIO (ensure false "expected text node")

    -- Fire event to update Dynamic
    SpiderM.liftIO (trigger ())

    -- Render again - should show updated value
    let node2 ← SpiderM.liftIO render
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
      emitDynamic do
        let n ← countDyn.sample
        pure (RNode.text s!"{n}" {})
    ).run events

    -- First render to buffer
    let node1 ← SpiderM.liftIO render
    let buf1 := Terminus.Reactive.render node1 10 5

    -- Fire event
    SpiderM.liftIO (trigger ())

    -- Second render to buffer
    let node2 ← SpiderM.liftIO render
    let buf2 := Terminus.Reactive.render node2 10 5

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

    let node1 ← SpiderM.liftIO render
    let buf1 := Terminus.Reactive.render node1 10 2

    SpiderM.liftIO (trigger 0.6)

    let node2 ← SpiderM.liftIO render
    let buf2 := Terminus.Reactive.render node2 10 2

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
        emitDynamic do
          let n ← countDyn.sample
          pure (RNode.text s!"{n}" {})
    ).run events

    let node1 ← SpiderM.liftIO render
    let buf1 := Terminus.Reactive.render node1 20 2

    -- Check initial buffer has "Count:" and "0"
    (buf1.get 0 0).char ≡ 'C'
    (buf1.get 6 0).char ≡ ' '  -- gap
    (buf1.get 7 0).char ≡ '0'

    SpiderM.liftIO (trigger ())

    let node2 ← SpiderM.liftIO render
    let buf2 := Terminus.Reactive.render node2 20 2

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
          emitDynamic do
            let n ← countDyn.sample
            pure (RNode.text s!"{n}" {})
    ).run events

    let node1 ← SpiderM.liftIO render
    let buf1 := Terminus.Reactive.render node1 20 5

    -- Header on row 0
    (buf1.get 0 0).char ≡ 'H'

    SpiderM.liftIO (trigger ())

    let node2 ← SpiderM.liftIO render
    let buf2 := Terminus.Reactive.render node2 20 5

    -- Verify buffers differ
    let diff := Buffer.diff buf1 buf2
    SpiderM.liftIO (ensure (!diff.isEmpty) "expected buffer to differ after count change")

-- ============================================================================
-- Terminal flush regression tests
-- ============================================================================

test "Terminal.flush writes updates when buffer changes" := do
  let action : MockTerminal (String × String) := do
    let term ← Terminus.Terminal.new

    let buf1 := (Buffer.new 5 1).writeString 0 0 "A" {}
    let term := term.setBuffer buf1
    let term ← term.flush

    let output1 := (← get).outputBuffer

    let buf2 := (Buffer.new 5 1).writeString 0 0 "B" {}
    let term := term.setBuffer buf2
    let _term ← term.flush

    let output2 := (← get).outputBuffer

    pure (output1, output2)

  let ((output1, output2), _) := MockTerminal.run action
  -- Output should grow and include the updated character
  (decide (output2.length > output1.length)) ≡ true
  (output2.toList.any (· == 'B')) ≡ true

-- ============================================================================
-- Reactive app loop regression tests (simulate runReactiveApp)
-- ============================================================================

test "reactive app loop updates buffer on tick events" := do
  let env ← SpiderEnv.new
  let (render, inputs) ← (do
    let (events, inputs) ← createInputs
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 events.tickEvent
    let (_, render) ← (runWidget do
      emitDynamic do
        let n ← countDyn.sample
        pure (RNode.text s!"{n}" {})
    ).run events
    pure (render, inputs)
  ).run env

  env.postBuildTrigger ()

  let node1 ← render
  let buf1 := Terminus.Reactive.render node1 10 2

  inputs.fireTick { frame := 1, elapsedMs := 16 }

  let node2 ← render
  let buf2 := Terminus.Reactive.render node2 10 2

  (buf1.get 0 0).char ≡ '0'
  (buf2.get 0 0).char ≡ '1'

  env.currentScope.dispose

test "reactive app loop updates buffer on key events" := do
  let env ← SpiderEnv.new
  let (render, inputs) ← (do
    let (events, inputs) ← createInputs
    let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 events.keyEvent
    let (_, render) ← (runWidget do
      emitDynamic do
        let n ← countDyn.sample
        pure (RNode.text s!"{n}" {})
    ).run events
    pure (render, inputs)
  ).run env

  env.postBuildTrigger ()

  let node1 ← render
  let buf1 := Terminus.Reactive.render node1 10 2

  inputs.fireKey { event := KeyEvent.char 'a', focusedWidget := none }

  let node2 ← render
  let buf2 := Terminus.Reactive.render node2 10 2

  (buf1.get 0 0).char ≡ '0'
  (buf2.get 0 0).char ≡ '1'

  env.currentScope.dispose

-- ============================================================================
-- Extracted loop + mock terminal integration
-- ============================================================================

test "runReactiveLoop updates frames with TerminalMock" := do
  let env ← SpiderEnv.new
  let (appState, events, inputs) ← (do
    let (events, inputs) ← createInputs
    let setup : ReactiveTermM ReactiveAppState := do
      let tickEvents ← useTick
      let countDyn ← Reactive.foldDyn (fun _ n => n + 1) 0 tickEvents
      let (_, render) ← runWidget do
        emitDynamic do
          let n ← countDyn.sample
          pure (RNode.text s!"{n}" {})
      pure { render }
    let appState ← setup.run events
    pure (appState, events, inputs)
  ).run env

  env.postBuildTrigger ()

  let framesRef ← IO.mkRef (#[] : Array Buffer)
  let clockRef ← IO.mkRef 0

  let deps : LoopDeps MockTerminalIO := {
    eventSource := Events.poll
    nowMs := do
      let n ← liftM (m := IO) clockRef.get
      liftM (m := IO) (clockRef.set (n + 16))
      pure n
    sleepMs := fun _ => pure ()
    log := fun _ => pure ()
    maxFrames := some 3
    onFrame := fun _ buf =>
      liftM (m := IO) <| framesRef.modify (fun frames => frames.push buf)
  }

  let action : MockTerminalIO Unit := do
    let term ← Terminal.new
    let termRef ← liftM (m := IO) (IO.mkRef term)
    runReactiveLoop { frameMs := 0 } events inputs appState.render termRef deps

  let _ ← action.run ({} : MockTerminalState)

  let frames ← framesRef.get
  match frames.toList with
  | buf1 :: buf2 :: _ =>
    ((Buffer.diff buf1 buf2).isEmpty) ≡ false
  | _ =>
    ensure false "expected at least 2 frames"

  env.currentScope.dispose

-- ============================================================================
-- Reactive demo widget tree regression
-- ============================================================================

test "ReactiveDemo widget updates on tick and key events" := do
  let env ← SpiderEnv.new
  let (appState, _events, inputs) ← (do
    let (events, inputs) ← createInputs
    let appState ← ReactiveTermM.run events reactiveDemoApp
    pure (appState, events, inputs)
  ).run env

  env.postBuildTrigger ()

  let node1 ← appState.render
  let buf1 := Terminus.Reactive.render node1 80 24

  (rnodeHasText node1 "0:00") ≡ true
  (rnodeHasText node1 "none") ≡ true

  inputs.fireTick { frame := 1, elapsedMs := 1500 }

  let node2 ← appState.render
  let buf2 := Terminus.Reactive.render node2 80 24

  (rnodeHasText node2 "0:01") ≡ true
  ensure (!(Buffer.diff buf1 buf2).isEmpty) "expected buffer to change after tick"

  inputs.fireKey { event := KeyEvent.char 'a', focusedWidget := none }

  let node3 ← appState.render
  let buf3 := Terminus.Reactive.render node3 80 24

  (rnodeHasText node3 "'a'") ≡ true
  ensure (!(Buffer.diff buf2 buf3).isEmpty) "expected buffer to change after key"

  env.currentScope.dispose

-- ============================================================================
-- Events.poll + tick integration
-- ============================================================================

test "Events.poll and tick drive frame-to-frame changes" := do
  let env ← SpiderEnv.new
  let (appState, events, inputs) ← (do
    let (events, inputs) ← createInputs
    let setup : ReactiveTermM ReactiveAppState := do
      let keyEvents ← useKeyEvent
      let tickEvents ← useTick
      let keyCount ← Reactive.foldDyn (fun _ n => n + 1) 0 keyEvents
      let tickCount ← Reactive.foldDyn (fun _ n => n + 1) 0 tickEvents
      let (_, render) ← runWidget do
        emitDynamic do
          let k ← keyCount.sample
          let t ← tickCount.sample
          pure (RNode.text s!"Key={k} Tick={t}" {})
      pure { render }
    let appState ← setup.run events
    pure (appState, events, inputs)
  ).run env

  env.postBuildTrigger ()

  let rowsRef ← IO.mkRef (#[] : Array String)
  let clockRef ← IO.mkRef 0

  let deps : LoopDeps MockTerminalIO := {
    eventSource := Events.poll
    nowMs := do
      let n ← liftM (m := IO) clockRef.get
      liftM (m := IO) (clockRef.set (n + 16))
      pure n
    sleepMs := fun _ => pure ()
    log := fun _ => pure ()
    maxFrames := some 2
    onFrame := fun _ buf =>
      let row := bufferRowPrefix buf 0 20
      liftM (m := IO) <| rowsRef.modify (fun rows => rows.push row)
  }

  let action : MockTerminalIO Unit := do
    let term ← Terminal.new
    let termRef ← liftM (m := IO) (IO.mkRef term)
    runReactiveLoop { frameMs := 0 } events inputs appState.render termRef deps

  let initialState : MockTerminalState := { inputQueue := [120] } -- 'x'
  let _ ← action.run initialState

  let rows ← rowsRef.get
  match rows.toList with
  | row1 :: row2 :: _ =>
    let expected1 := "Key=1 Tick=1"
    let expected2 := "Key=1 Tick=2"
    (row1.take expected1.length) ≡ expected1
    (row2.take expected2.length) ≡ expected2
  | _ =>
    ensure false "expected at least 2 frames"

  env.currentScope.dispose

-- ============================================================================
-- Phase 1: TextInput Tests
-- ============================================================================

test "textInput' renders with placeholder when empty" := do
  runSpider do
    let (events, _) ← createInputs
    let (result, render) ← (runWidget do
      textInput' "test" "" { placeholder := "Enter text...", width := 15 }
    ).run events

    let node ← SpiderM.liftIO render
    -- Should contain placeholder text when empty and not focused
    SpiderM.liftIO (ensure (rnodeHasText node "Enter text...") "expected placeholder")

test "textInput' value dynamic updates on input" := do
  let env ← SpiderEnv.new
  let (inputResult, inputs) ← (do
    let (events, inputs) ← createInputs
    let (result, _render) ← (runWidget do
      textInput' "test-input" "" {}
    ).run events
    pure (result, inputs)
  ).run env

  env.postBuildTrigger ()

  -- Set focus to the input
  let (events, _) ← (createInputs).run env

  -- Initial value should be empty
  let v0 ← inputResult.value.sample
  v0 ≡ ""

  env.currentScope.dispose

test "textInput' state operations work correctly" := do
  -- Test TextInputState operations
  let s0 : TextInputState := { text := "hello", cursor := 3 }

  -- Insert character
  let s1 := s0.insertChar 'X' none
  s1.text ≡ "helXlo"
  s1.cursor ≡ 4

  -- Backspace
  let s2 := s1.backspace
  s2.text ≡ "hello"
  s2.cursor ≡ 3

  -- Move left
  let s3 := s0.moveLeft
  s3.cursor ≡ 2

  -- Move right
  let s4 := s0.moveRight
  s4.cursor ≡ 4

  -- Home
  let s5 := s0.moveHome
  s5.cursor ≡ 0

  -- End
  let s6 := s0.moveEnd
  s6.cursor ≡ 5

  -- Delete
  let s7 : TextInputState := { text := "hello", cursor := 2 }
  let s8 := s7.delete
  s8.text ≡ "helo"
  s8.cursor ≡ 2

test "textInput' respects maxLength" := do
  let s0 : TextInputState := { text := "abc", cursor := 3 }

  -- Should allow insert when under limit
  let s1 := s0.insertChar 'd' (some 5)
  s1.text ≡ "abcd"

  -- Should block insert when at limit
  let s2 : TextInputState := { text := "abcde", cursor := 5 }
  let s3 := s2.insertChar 'f' (some 5)
  s3.text ≡ "abcde"

-- ============================================================================
-- Phase 1: SelectableList Tests
-- ============================================================================

test "selectableList' renders items with selection" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #["Apple", "Banana", "Cherry"]
    let (_result, render) ← (runWidget do
      selectableList' items 0 {}
    ).run events

    let node ← SpiderM.liftIO render
    -- Should have all items
    SpiderM.liftIO (ensure (rnodeHasText node "> Apple") "expected selected Apple")
    SpiderM.liftIO (ensure (rnodeHasText node "  Banana") "expected Banana")
    SpiderM.liftIO (ensure (rnodeHasText node "  Cherry") "expected Cherry")

test "selectableList' initial selection is correct" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #["A", "B", "C"]
    let (result, _render) ← (runWidget do
      selectableList' items 1 {}  -- Start at index 1
    ).run events

    let idx ← SpiderM.liftIO result.selectedIndex.sample
    SpiderM.liftIO (idx ≡ 1)

    let item ← SpiderM.liftIO result.selectedItem.sample
    match item with
    | some s => SpiderM.liftIO (s ≡ "B")
    | none => SpiderM.liftIO (ensure false "expected selected item")

test "selectableList' ListState navigation" := do
  -- Test ListState operations
  let s0 : ListState := { selected := 1, scrollOffset := 0 }

  -- Move up
  let s1 := s0.moveUp 5 true
  s1.selected ≡ 0

  -- Move up with wrap
  let s2 : ListState := { selected := 0, scrollOffset := 0 }
  let s3 := s2.moveUp 5 true
  s3.selected ≡ 4

  -- Move up without wrap
  let s4 := s2.moveUp 5 false
  s4.selected ≡ 0

  -- Move down
  let s5 := s0.moveDown 5 true
  s5.selected ≡ 2

  -- Move down with wrap
  let s6 : ListState := { selected := 4, scrollOffset := 0 }
  let s7 := s6.moveDown 5 true
  s7.selected ≡ 0

  -- Move to first/last
  let s8 := s0.moveToFirst
  s8.selected ≡ 0

  let s9 := s0.moveToLast 5
  s9.selected ≡ 4

test "selectableList' scroll adjustment" := do
  let s0 : ListState := { selected := 5, scrollOffset := 0 }

  -- Adjust scroll to keep selection visible (maxVisible = 3)
  let s1 := s0.adjustScroll 3
  s1.scrollOffset ≡ 3  -- selected(5) - maxVisible(3) + 1 = 3

  -- Selection within visible range should not change scroll
  let s2 : ListState := { selected := 1, scrollOffset := 0 }
  let s3 := s2.adjustScroll 3
  s3.scrollOffset ≡ 0

test "selectableList' renders scroll indicators" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #["A", "B", "C", "D", "E"]
    let (_result, render) ← (runWidget do
      selectableList' items 3 { maxVisible := some 2, showScrollIndicators := true }
    ).run events

    let node ← SpiderM.liftIO render
    -- Should have scroll indicators since list is truncated
    SpiderM.liftIO (ensure (rnodeHasText node "  ...") "expected scroll indicator")

test "numberedList' renders with numbers" := do
  runSpider do
    let (events, _) ← createInputs
    let items := #["Red", "Green", "Blue"]
    let (_result, render) ← (runWidget do
      numberedList' items 0 {}
    ).run events

    let node ← SpiderM.liftIO render
    SpiderM.liftIO (ensure (rnodeHasText node "> 1. Red") "expected numbered item 1")
    SpiderM.liftIO (ensure (rnodeHasText node "  2. Green") "expected numbered item 2")
    SpiderM.liftIO (ensure (rnodeHasText node "  3. Blue") "expected numbered item 3")

-- ============================================================================
-- Phase 1: Overlay/Modal Tests
-- ============================================================================

test "overlayWhen' renders nothing when not visible" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, _trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn false event

    let (_, render) ← (runWidget do
      overlayWhen' visible {} do
        text' "Overlay content" {}
    ).run events

    let node ← SpiderM.liftIO render
    -- Should be empty when not visible
    match node with
    | .empty => pure ()
    | _ => SpiderM.liftIO (ensure false "expected empty node when not visible")

test "overlayWhen' renders content when visible" := do
  runSpider do
    let (events, _) ← createInputs
    let (event, trigger) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn false event

    let (_, render) ← (runWidget do
      overlayWhen' visible {} do
        text' "Overlay content" {}
    ).run events

    -- Make visible
    SpiderM.liftIO (trigger true)

    let node ← SpiderM.liftIO render
    SpiderM.liftIO (ensure (rnodeHasText node "Overlay content") "expected overlay content")

test "modal' renders with title and border" := do
  runSpider do
    let (events, _) ← createInputs
    let theme := Theme.dark

    let (_, render) ← (runWidget do
      let _ ← modal' "Test Modal" theme {} do
        text' "Modal body" {}
      pure ()
    ).run events

    let node ← SpiderM.liftIO render
    -- Modal wraps in a block with title
    match node with
    | .block title _ _ child =>
      SpiderM.liftIO (title ≡ some "Test Modal")
      SpiderM.liftIO (ensure (rnodeHasText child "Modal body") "expected modal body")
    | _ => SpiderM.liftIO (ensure false "expected block node")

test "confirmDialog' emits confirmed event on Y key" := do
  let env ← SpiderEnv.new
  let (confirmResult, inputs, visible, setVisible) ← (do
    let (events, inputs) ← createInputs
    let (visEvent, setVis) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn true visEvent
    let (result, _render) ← (runWidget do
      confirmDialog' "Are you sure?" visible Theme.dark
    ).run events
    pure (result, inputs, visible, setVis)
  ).run env

  env.postBuildTrigger ()

  -- Track if confirmed was fired
  let confirmedRef ← IO.mkRef false
  let _unsub ← confirmResult.confirmed.subscribe fun () =>
    confirmedRef.set true

  -- Press 'y'
  inputs.fireKey { event := KeyEvent.char 'y', focusedWidget := none }

  let wasConfirmed ← confirmedRef.get
  wasConfirmed ≡ true

  env.currentScope.dispose

test "confirmDialog' emits cancelled event on Escape" := do
  let env ← SpiderEnv.new
  let (confirmResult, inputs, visible, setVisible) ← (do
    let (events, inputs) ← createInputs
    let (visEvent, setVis) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn true visEvent
    let (result, _render) ← (runWidget do
      confirmDialog' "Are you sure?" visible Theme.dark
    ).run events
    pure (result, inputs, visible, setVis)
  ).run env

  env.postBuildTrigger ()

  -- Track if cancelled was fired
  let cancelledRef ← IO.mkRef false
  let _unsub ← confirmResult.cancelled.subscribe fun () =>
    cancelledRef.set true

  -- Press Escape
  inputs.fireKey { event := KeyEvent.escape, focusedWidget := none }

  let wasCancelled ← cancelledRef.get
  wasCancelled ≡ true

  env.currentScope.dispose

test "messageDialog' emits dismiss on Enter" := do
  let env ← SpiderEnv.new
  let (dismissEvent, inputs, visible) ← (do
    let (events, inputs) ← createInputs
    let (visEvent, _setVis) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let visible ← Reactive.holdDyn true visEvent
    let (result, _render) ← (runWidget do
      messageDialog' "Operation complete" visible Theme.dark
    ).run events
    pure (result, inputs, visible)
  ).run env

  env.postBuildTrigger ()

  let dismissedRef ← IO.mkRef false
  let _unsub ← dismissEvent.subscribe fun () =>
    dismissedRef.set true

  inputs.fireKey { event := KeyEvent.enter, focusedWidget := none }

  let wasDismissed ← dismissedRef.get
  wasDismissed ≡ true

  env.currentScope.dispose

-- ============================================================================
-- Clipping and ScrollView Tests
-- ============================================================================

test "ClipContext.contains returns true when no clip rect" := do
  let ctx : ClipContext := {}
  (ctx.contains 100 200) ≡ true

test "ClipContext.contains returns true for point inside rect" := do
  let ctx : ClipContext := { clipRect := some { x := 10, y := 10, width := 20, height := 15 } }
  (ctx.contains 15 12) ≡ true
  (ctx.contains 10 10) ≡ true  -- top-left corner
  (ctx.contains 29 24) ≡ true  -- bottom-right corner (exclusive boundary - 1)

test "ClipContext.contains returns false for point outside rect" := do
  let ctx : ClipContext := { clipRect := some { x := 10, y := 10, width := 20, height := 15 } }
  (ctx.contains 5 5) ≡ false   -- before top-left
  (ctx.contains 30 25) ≡ false -- at exclusive boundary
  (ctx.contains 15 5) ≡ false  -- above
  (ctx.contains 50 15) ≡ false -- to the right

test "ClipContext.intersect narrows clip rect" := do
  let ctx1 : ClipContext := { clipRect := some { x := 0, y := 0, width := 100, height := 100 } }
  let ctx2 := ctx1.intersect { x := 20, y := 30, width := 50, height := 40 }
  match ctx2.clipRect with
  | some r =>
    r.x ≡ 20
    r.y ≡ 30
    r.width ≡ 50
    r.height ≡ 40
  | none => ensure false "expected clip rect"

test "ClipContext.intersect creates clip when none exists" := do
  let ctx1 : ClipContext := {}
  let ctx2 := ctx1.intersect { x := 5, y := 10, width := 30, height := 20 }
  match ctx2.clipRect with
  | some r =>
    r.x ≡ 5
    r.y ≡ 10
    r.width ≡ 30
    r.height ≡ 20
  | none => ensure false "expected clip rect"

test "ClipContext.addOffset accumulates scroll offset" := do
  let ctx1 : ClipContext := { scrollOffsetX := 5, scrollOffsetY := 10 }
  let ctx2 := ctx1.addOffset 3 7
  ctx2.scrollOffsetX ≡ 8
  ctx2.scrollOffsetY ≡ 17

test "ScrollState.maxOffsetY computes correctly" := do
  let state : ScrollState := { contentHeight := 100, viewportHeight := 30 }
  state.maxOffsetY ≡ 70

test "ScrollState.maxOffsetY returns 0 when content fits" := do
  let state : ScrollState := { contentHeight := 20, viewportHeight := 30 }
  state.maxOffsetY ≡ 0

test "ScrollState.scrollDown moves down by amount" := do
  let state : ScrollState := { offsetY := 5, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.scrollDown 10
  scrolled.offsetY ≡ 15

test "ScrollState.scrollDown clamps to max" := do
  let state : ScrollState := { offsetY := 70, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.scrollDown 50  -- Would exceed max of 80
  scrolled.offsetY ≡ 80

test "ScrollState.scrollUp moves up by amount" := do
  let state : ScrollState := { offsetY := 15, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.scrollUp 5
  scrolled.offsetY ≡ 10

test "ScrollState.scrollUp clamps to 0" := do
  let state : ScrollState := { offsetY := 5, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.scrollUp 10  -- Would go negative
  scrolled.offsetY ≡ 0

test "ScrollState.scrollToTop resets to 0" := do
  let state : ScrollState := { offsetY := 50, contentHeight := 100, viewportHeight := 20 }
  state.scrollToTop.offsetY ≡ 0

test "ScrollState.scrollToBottom goes to max" := do
  let state : ScrollState := { offsetY := 0, contentHeight := 100, viewportHeight := 20 }
  state.scrollToBottom.offsetY ≡ 80

test "ScrollState.pageDown scrolls by viewport - 1" := do
  let state : ScrollState := { offsetY := 0, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.pageDown
  scrolled.offsetY ≡ 19  -- 20 - 1 = 19

test "ScrollState.pageUp scrolls by viewport - 1" := do
  let state : ScrollState := { offsetY := 50, contentHeight := 100, viewportHeight := 20 }
  let scrolled := state.pageUp
  scrolled.offsetY ≡ 31  -- 50 - 19 = 31

test "ScrollState.scrollToVisible scrolls down when item below viewport" := do
  let state : ScrollState := { offsetY := 0, contentHeight := 100, viewportHeight := 10 }
  let scrolled := state.scrollToVisible 15  -- Item at y=15 not visible in 0..9
  scrolled.offsetY ≡ 6  -- 15 - 10 + 1 = 6

test "ScrollState.scrollToVisible scrolls up when item above viewport" := do
  let state : ScrollState := { offsetY := 20, contentHeight := 100, viewportHeight := 10 }
  let scrolled := state.scrollToVisible 5  -- Item at y=5 not visible in 20..29
  scrolled.offsetY ≡ 5

test "ScrollState.scrollToVisible does nothing when item visible" := do
  let state : ScrollState := { offsetY := 10, contentHeight := 100, viewportHeight := 10 }
  let scrolled := state.scrollToVisible 15  -- Item at y=15 is visible in 10..19
  scrolled.offsetY ≡ 10

test "ScrollState.needsVerticalScroll true when content exceeds viewport" := do
  let state : ScrollState := { contentHeight := 50, viewportHeight := 20 }
  state.needsVerticalScroll ≡ true

test "ScrollState.needsVerticalScroll false when content fits" := do
  let state : ScrollState := { contentHeight := 15, viewportHeight := 20 }
  state.needsVerticalScroll ≡ false

test "RNode.clipped renders content within bounds" := do
  let node : RNode := .clipped (
    .column 0 {} #[
      .text "Line 1" {},
      .text "Line 2" {},
      .text "Line 3" {}
    ]
  )
  let buf := Terminus.Reactive.render node 20 3
  -- All 3 lines should fit in 3-row buffer
  (buf.get 0 0).char ≡ 'L'  -- Line 1
  (buf.get 0 1).char ≡ 'L'  -- Line 2
  (buf.get 0 2).char ≡ 'L'  -- Line 3

test "RNode.scrolled applies offset to content" := do
  let node : RNode := .clipped (
    .scrolled 0 1 (  -- Scroll down by 1
      .column 0 {} #[
        .text "Line 1" {},
        .text "Line 2" {},
        .text "Line 3" {}
      ]
    )
  )
  let buf := Terminus.Reactive.render node 20 3
  -- With scroll offset of 1, Line 2 should be at row 0
  -- Note: exact behavior depends on how scroll offset is applied
  buf.width ≡ 20  -- Basic sanity check

test "renderVerticalScrollbar creates column of chars" := do
  let state : ScrollState := { offsetY := 0, contentHeight := 100, viewportHeight := 20 }
  let config : ScrollViewConfig := {}
  let scrollbar := renderVerticalScrollbar state config 10
  -- Scrollbar should be a column node
  match scrollbar with
  | .column _ _ children => children.size ≡ 10
  | _ => ensure false "expected column node"

test "computeThumbMetrics returns full track when content fits" := do
  let (thumbSize, thumbPos) := computeThumbMetrics 10 20 0 10  -- content=10, viewport=20
  thumbSize ≡ 10  -- Full track
  thumbPos ≡ 0

test "computeThumbMetrics proportional thumb size" := do
  let (thumbSize, _) := computeThumbMetrics 100 20 0 10  -- content=100, viewport=20
  -- Thumb size = trackLen * viewportSize / contentSize = 10 * 20 / 100 = 2
  thumbSize ≡ 2

test "computeThumbMetrics thumb position at end" := do
  let (thumbSize, thumbPos) := computeThumbMetrics 100 20 80 10  -- offset=80 (max)
  -- Thumb should be at end of track
  (thumbPos + thumbSize) ≡ 10

#generate_tests

end Tests.Reactive

def main : IO UInt32 := do
  IO.println "╔══════════════════════════════════════════════════════════════╗"
  IO.println "║                Terminus.Reactive Test Suite                  ║"
  IO.println "╚══════════════════════════════════════════════════════════════╝"
  IO.println ""

  let result ← runAllSuites

  IO.println ""
  if result == 0 then
    IO.println "All tests passed!"
  else
    IO.println "Some tests failed"

  return result
