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
  | .spacer _ _ | .empty => false

partial def rnodeHasText (node : RNode) (needle : String) : Bool :=
  match node with
  | .text content _ => content == needle
  | .block _ _ _ child => rnodeHasText child needle
  | .row _ _ children => children.any (fun child => rnodeHasText child needle)
  | .column _ _ children => children.any (fun child => rnodeHasText child needle)
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
