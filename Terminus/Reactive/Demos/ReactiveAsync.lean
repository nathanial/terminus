/-
  Reactive Async Demo Widget
  Showcases async operations and split pane layouts.
-/
import Terminus.Reactive

open Terminus Terminus.Reactive
open Reactive Reactive.Host

/-- Simulate a slow data load. -/
def simulateSlowLoad : IO String := do
  IO.sleep 1500
  pure "Data loaded successfully!"

/-- Simulate an error. -/
def simulateError : IO String := do
  IO.sleep 500
  throw (IO.userError "Simulated network error")

/-- Simulate streaming chunks. -/
def createStreamAction : IO (IO.Ref Nat × IO (Option String)) := do
  let counterRef ← IO.mkRef 0
  let action : IO (Option String) := do
    let count ← counterRef.get
    if count >= 5 then
      pure none
    else
      IO.sleep 300
      counterRef.set (count + 1)
      pure (some s!"Chunk {count + 1}: Lorem ipsum dolor sit amet...")
  pure (counterRef, action)

/-- Demo application showing async operations and split layouts. -/
def reactiveAsyncApp : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark

  -- Create stream action factory
  let streamStateRef ← SpiderM.liftIO (IO.mkRef (0 : Nat))

  let (_, render) ← runWidget do
    column' (gap := 1) (style := {}) do
      -- Header
      text' "=== Reactive Async Demo ===" theme.heading1Style
      text' "Space: load data | E: trigger error | S: stream | +/-: resize | Ctrl+C: quit" theme.captionStyle

      -- Split layout: left pane (async demo) | right pane (stream demo)
      let _ ← horizontalSplit' 50 {}
        -- Left pane: Basic async demo
        (do
          titledBlock' "Async Operations" .rounded theme do
            text' "Press Space to trigger async load:" theme.bodyStyle

            -- Setup async trigger
            let keyEvent ← useKeyEventW
            let spaceEvents ← Event.filterM (fun kd => kd.event.code == .space) keyEvent
            let trigger ← Event.voidM spaceEvents

            -- Async operation
            let asyncResult ← useAsyncW simulateSlowLoad trigger

            spacer' 1 1

            -- Loading indicator
            emitDynamic do
              let loading ← asyncResult.loading.sample
              if loading then
                pure (RNode.text "Loading..." { fg := .ansi .yellow, modifier := { bold := true } })
              else
                pure RNode.empty

            -- Result display
            emitDynamic do
              let result ← asyncResult.result.sample
              match result with
              | none =>
                pure (RNode.text "No data loaded yet" theme.captionStyle)
              | some data =>
                pure (RNode.text s!"Result: {data}" { fg := .ansi .green })

            spacer' 1 1

            -- Error demo
            text' "Press E to trigger an error:" theme.bodyStyle

            let errorEvents ← Event.filterM (fun kd => kd.event.code == .char 'e' || kd.event.code == .char 'E') keyEvent
            let errorTrigger ← Event.voidM errorEvents
            let errorResult ← useAsyncW simulateError errorTrigger

            -- Error display
            emitDynamic do
              let loading ← errorResult.loading.sample
              if loading then
                pure (RNode.text "Triggering error..." { fg := .ansi .yellow })
              else
                pure RNode.empty

            let errorMsgDyn ← holdDyn "" errorResult.onError
            emitDynamic do
              let errMsg ← errorMsgDyn.sample
              if errMsg.isEmpty then
                pure RNode.empty
              else
                pure (RNode.text s!"Error: {errMsg}" { fg := .ansi .red })
        )
        -- Right pane: Streaming demo
        (do
          titledBlock' "Streaming Demo" .rounded theme do
            text' "Press S to start streaming:" theme.bodyStyle

            spacer' 1 1

            -- Track stream state manually (simplified)
            let (chunkEvent, fireChunk) ← newTriggerEvent (t := Spider) (a := String)
            let (streamingEvent, fireStreaming) ← newTriggerEvent (t := Spider) (a := Bool)
            let (chunksEvent, fireChunks) ← newTriggerEvent (t := Spider) (a := Array String)

            let chunksRef ← SpiderM.liftIO (IO.mkRef #[])
            let env ← SpiderM.getEnv

            -- Subscribe to S key
            let keyEvent ← useKeyEventW
            let _unsub ← SpiderM.liftIO <| keyEvent.subscribe fun kd => do
              if kd.event.code == .char 's' || kd.event.code == .char 'S' then
                -- Reset and start streaming
                chunksRef.set #[]
                env.withFrame do
                  fireStreaming true
                  fireChunks #[]

                -- Spawn streaming task
                let _ ← IO.asTask (prio := .dedicated) do
                  for i in [1:6] do
                    IO.sleep 400
                    let chunk := s!"Chunk {i}: Data received..."
                    let newChunks ← chunksRef.modifyGet fun cs => (cs.push chunk, cs.push chunk)
                    env.withFrame do
                      fireChunk chunk
                      fireChunks newChunks
                  env.withFrame (fireStreaming false)

            let streamingDyn ← holdDyn false streamingEvent
            let chunksDyn ← holdDyn #[] chunksEvent

            -- Streaming indicator
            emitDynamic do
              let streaming ← streamingDyn.sample
              if streaming then
                pure (RNode.text "Streaming..." { fg := .ansi .cyan, modifier := { bold := true } })
              else
                pure (RNode.text "Idle" theme.captionStyle)

            spacer' 1 1

            -- Display chunks
            text' "Received chunks:" theme.bodyStyle
            emitDynamic do
              let chunks ← chunksDyn.sample
              if chunks.isEmpty then
                pure (RNode.text "(no chunks yet)" theme.captionStyle)
              else
                let chunkNodes := chunks.map fun chunk =>
                  RNode.text s!"  {chunk}" { fg := .ansi .green }
                pure (RNode.column 0 {} chunkNodes)
        )

      -- Footer with focus indicator
      let focusedInput ← useFocusedInputW
      emitDynamic do
        let focused ← focusedInput.sample
        let focusName := focused.getD "(none)"
        pure (RNode.text s!"Focused: {focusName}" theme.captionStyle)

  pure { render }
