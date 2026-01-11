import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Async Tab Content -/

def simulateSlowLoad : IO String := do
  IO.sleep 1500
  pure "Data loaded successfully!"

def asyncLoadSection (theme : Theme) : WidgetM Unit := do
  titledBlock' "Async Operation" .rounded theme do
    text' "Press Space to trigger:" theme.bodyStyle

    let keyEvent ← useKeyEventW
    let spaceEvents ← Event.filterM (fun kd => kd.event.code == .space) keyEvent
    let trigger ← Event.voidM spaceEvents
    let asyncResult ← useAsyncW simulateSlowLoad trigger

    spacer' 1 1

    emitDynamic do
      let loading ← asyncResult.loading.sample
      if loading then
        pure (RNode.text "Loading..." { fg := .ansi .yellow, modifier := { bold := true } })
      else
        pure RNode.empty

    emitDynamic do
      let result ← asyncResult.result.sample
      match result with
      | none => pure (RNode.text "No data yet" theme.captionStyle)
      | some data => pure (RNode.text s!"Result: {data}" { fg := .ansi .green })

def streamingSection (theme : Theme) : WidgetM Unit := do
  titledBlock' "Streaming Demo" .rounded theme do
    text' "Press S to stream:" theme.bodyStyle

    -- FRP: Trigger events for streaming state
    let (chunkEvent, fireChunk) ← Reactive.newTriggerEvent (t := Spider) (a := String)
    let (resetEvent, fireReset) ← Reactive.newTriggerEvent (t := Spider) (a := Unit)
    let (streamingEvent, fireStreaming) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)

    let env ← SpiderM.getEnv

    -- FRP: Accumulate chunks using foldDyn (either add chunk or reset)
    let addChunkEvents ← Event.mapM (fun chunk => (true, chunk)) chunkEvent
    let resetEvents ← Event.mapM (fun _ => (false, "")) resetEvent
    let allChunkEvents ← Event.leftmostM [addChunkEvents, resetEvents]
    let chunksDyn ← Reactive.foldDyn (fun (isAdd, chunk) chunks =>
      if isAdd then chunks.push chunk else #[]
    ) (#[] : Array String) allChunkEvents

    let streamingDyn ← Reactive.holdDyn false streamingEvent

    let keyEvent ← useKeyEventW
    let streamKeys ← Event.filterM (fun kd =>
      kd.event.code == .char 's' || kd.event.code == .char 'S') keyEvent

    -- FRP: Map stream keys to IO actions and use performEvent_
    let streamAction ← Event.mapM (fun _ => do
      env.withFrame do
        fireReset ()
        fireStreaming true
      let _ ← IO.asTask (prio := .dedicated) do
        for i in [1:5] do
          IO.sleep 400
          env.withFrame (fireChunk s!"Chunk {i}")
        env.withFrame (fireStreaming false)
    ) streamKeys
    performEvent_ streamAction

    spacer' 1 1

    emitDynamic do
      let streaming ← streamingDyn.sample
      if streaming then
        pure (RNode.text "Streaming..." { fg := .ansi .cyan, modifier := { bold := true } })
      else
        pure (RNode.text "Idle" theme.captionStyle)

    emitDynamic do
      let chunks ← chunksDyn.sample
      if chunks.isEmpty then
        pure (RNode.text "(no chunks)" theme.captionStyle)
      else
        let chunkNodes := chunks.map fun chunk =>
          RNode.text s!"  {chunk}" { fg := .ansi .green }
        pure (RNode.column 0 {} chunkNodes)

def asyncContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Space: load data | S: stream chunks" theme.captionStyle

    let _ ← horizontalSplit' 50 {}
      (asyncLoadSection theme)
      (streamingSection theme)
