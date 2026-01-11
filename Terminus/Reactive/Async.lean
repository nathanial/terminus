/-
  Terminus Reactive - Async Utilities
  Support for asynchronous operations with reactive integration.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Async Configuration -/

/-- Configuration for async operations. -/
structure AsyncConfig where
  /-- Task priority for async work. -/
  priority : Task.Priority := .dedicated
  deriving Repr, Inhabited

/-! ## Async Result -/

/-- Result of an async operation with loading state. -/
structure AsyncResult (α : Type) where
  /-- Result once complete (None while loading or before first trigger). -/
  result : Reactive.Dynamic Spider (Option α)
  /-- True while async work is in progress. -/
  loading : Reactive.Dynamic Spider Bool
  /-- Event fired when async work completes successfully. -/
  onComplete : Reactive.Event Spider α
  /-- Event fired on error (contains error message). -/
  onError : Reactive.Event Spider String

/-! ## Core Async Functions -/

/-- Run an async IO action when trigger fires.

    The action runs in a dedicated task and fires reactive events
    when complete. Loading state is tracked automatically.

    Example:
    ```
    let keyEvent ← useKey .space
    let trigger ← Event.voidM keyEvent
    let result ← useAsync fetchData trigger
    -- result.loading is true while fetching
    -- result.result contains the data once complete
    ```
-/
def useAsync (action : IO α) (trigger : Reactive.Event Spider Unit)
    (config : AsyncConfig := {}) : ReactiveTermM (AsyncResult α) := do
  -- Get the SpiderEnv for frame-wrapped firing
  let env ← SpiderM.getEnv

  -- Create trigger events for results
  let (resultEvent, fireResult) ← newTriggerEvent (t := Spider) (a := α)
  let (errorEvent, fireError) ← newTriggerEvent (t := Spider) (a := String)
  let (loadingEvent, fireLoading) ← newTriggerEvent (t := Spider) (a := Bool)

  -- Subscribe to trigger
  let _unsub ← SpiderM.liftIO <| trigger.subscribe fun () => do
    -- Set loading state
    env.withFrame (fireLoading true)

    -- Spawn async task
    let _ ← IO.asTask (prio := config.priority) do
      try
        let result ← action
        -- Fire result and clear loading (in same frame for consistency)
        env.withFrame do
          fireLoading false
          fireResult result
      catch e =>
        env.withFrame do
          fireLoading false
          fireError e.toString

  -- Create dynamics from events
  let loadingDyn ← holdDyn false loadingEvent
  let resultEventMapped ← Event.mapM some resultEvent
  let resultDyn ← holdDyn none resultEventMapped

  pure {
    result := resultDyn
    loading := loadingDyn
    onComplete := resultEvent
    onError := errorEvent
  }

/-- Run an async action once on setup.

    Unlike `useAsync`, this runs immediately when the component mounts,
    not in response to a trigger event.

    Example:
    ```
    let config ← useAsyncOnce loadConfig
    -- config.result contains loaded config once ready
    ```
-/
def useAsyncOnce (action : IO α) (config : AsyncConfig := {})
    : ReactiveTermM (AsyncResult α) := do
  -- Get the SpiderEnv
  let env ← SpiderM.getEnv

  -- Create trigger events
  let (resultEvent, fireResult) ← newTriggerEvent (t := Spider) (a := α)
  let (errorEvent, fireError) ← newTriggerEvent (t := Spider) (a := String)
  let (loadingEvent, fireLoading) ← newTriggerEvent (t := Spider) (a := Bool)

  -- Start loading immediately
  SpiderM.liftIO <| env.withFrame (fireLoading true)

  -- Spawn async task immediately
  let _ ← SpiderM.liftIO <| IO.asTask (prio := config.priority) do
    try
      let result ← action
      env.withFrame do
        fireLoading false
        fireResult result
    catch e =>
      env.withFrame do
        fireLoading false
        fireError e.toString

  -- Create dynamics
  let loadingDyn ← holdDyn true loadingEvent  -- Start as loading
  let resultEventMapped ← Event.mapM some resultEvent
  let resultDyn ← holdDyn none resultEventMapped

  pure {
    result := resultDyn
    loading := loadingDyn
    onComplete := resultEvent
    onError := errorEvent
  }

/-! ## Streaming Support -/

/-- Stream result - accumulates chunks as they arrive. -/
structure StreamResult where
  /-- Accumulated chunks so far. -/
  chunks : Reactive.Dynamic Spider (Array String)
  /-- True while stream is active. -/
  streaming : Reactive.Dynamic Spider Bool
  /-- Event fired for each new chunk. -/
  onChunk : Reactive.Event Spider String
  /-- Event fired when stream completes. -/
  onComplete : Reactive.Event Spider Unit
  /-- Event fired on error. -/
  onError : Reactive.Event Spider String

/-- Stream chunks from an IO action that yields incrementally.

    The `streamAction` should return `some chunk` for each chunk,
    and `none` when the stream is complete.

    Example:
    ```
    let streamResult ← useStream readNextChunk trigger
    -- streamResult.chunks accumulates as data arrives
    -- streamResult.streaming indicates if still receiving
    ```
-/
def useStream (streamAction : IO (Option String)) (trigger : Reactive.Event Spider Unit)
    (config : AsyncConfig := {}) : ReactiveTermM StreamResult := do
  let env ← SpiderM.getEnv

  -- Create events
  let (chunkEvent, fireChunk) ← newTriggerEvent (t := Spider) (a := String)
  let (completeEvent, fireComplete) ← newTriggerEvent (t := Spider) (a := Unit)
  let (errorEvent, fireError) ← newTriggerEvent (t := Spider) (a := String)
  let (streamingEvent, fireStreaming) ← newTriggerEvent (t := Spider) (a := Bool)

  -- Track accumulated chunks
  let chunksRef ← SpiderM.liftIO (IO.mkRef #[])
  let (chunksUpdateEvent, fireChunksUpdate) ← newTriggerEvent (t := Spider) (a := Array String)

  -- Subscribe to trigger
  let _unsub ← SpiderM.liftIO <| trigger.subscribe fun () => do
    -- Reset chunks and start streaming
    chunksRef.set #[]
    env.withFrame do
      fireStreaming true
      fireChunksUpdate #[]

    -- Spawn streaming task
    let _ ← IO.asTask (prio := config.priority) do
      try
        -- Use repeat pattern instead of recursive loop
        repeat do
          match ← streamAction with
          | some chunk =>
            let newChunks ← chunksRef.modifyGet fun cs => (cs.push chunk, cs.push chunk)
            env.withFrame do
              fireChunk chunk
              fireChunksUpdate newChunks
          | none =>
            env.withFrame do
              fireStreaming false
              fireComplete ()
            break
      catch e =>
        env.withFrame do
          fireStreaming false
          fireError e.toString

  -- Create dynamics
  let streamingDyn ← holdDyn false streamingEvent
  let chunksDyn ← holdDyn #[] chunksUpdateEvent

  pure {
    chunks := chunksDyn
    streaming := streamingDyn
    onChunk := chunkEvent
    onComplete := completeEvent
    onError := errorEvent
  }

/-! ## Debounced Async -/

/-- Run async action with debouncing.

    If a new input arrives before the previous async completes,
    the pending result is discarded (though the IO action still runs).

    Useful for search-as-you-type patterns where only the latest
    result matters.

    Example:
    ```
    let searchEvent ← ... -- event with search query
    let results ← useDebounceAsync 300 performSearch searchEvent
    ```
-/
def useDebounceAsync (delayMs : Nat) (action : α → IO β)
    (input : Reactive.Event Spider α) (config : AsyncConfig := {})
    : ReactiveTermM (Reactive.Event Spider β) := do
  let env ← SpiderM.getEnv

  -- Generation counter for cancellation
  let genRef ← SpiderM.liftIO (IO.mkRef (0 : Nat))
  let (resultEvent, fireResult) ← newTriggerEvent (t := Spider) (a := β)

  -- Subscribe to input
  let _unsub ← SpiderM.liftIO <| input.subscribe fun a => do
    -- Increment generation (cancels previous pending)
    let gen ← genRef.modifyGet fun g => (g + 1, g + 1)

    -- Spawn debounced async task
    let _ ← IO.asTask (prio := config.priority) do
      -- Wait for debounce delay
      IO.sleep delayMs.toUInt32

      -- Check if still current generation
      let currentGen ← genRef.get
      if gen == currentGen then
        -- Still current, run action
        let result ← action a
        -- Check again after action
        let currentGen' ← genRef.get
        if gen == currentGen' then
          env.withFrame (fireResult result)

  pure resultEvent

/-! ## WidgetM Versions -/

/-- Use async in WidgetM context. -/
def useAsyncW (action : IO α) (trigger : Reactive.Event Spider Unit)
    (config : AsyncConfig := {}) : WidgetM (AsyncResult α) :=
  StateT.lift (useAsync action trigger config)

/-- Use async once in WidgetM context. -/
def useAsyncOnceW (action : IO α) (config : AsyncConfig := {})
    : WidgetM (AsyncResult α) :=
  StateT.lift (useAsyncOnce action config)

/-- Use stream in WidgetM context. -/
def useStreamW (streamAction : IO (Option String)) (trigger : Reactive.Event Spider Unit)
    (config : AsyncConfig := {}) : WidgetM StreamResult :=
  StateT.lift (useStream streamAction trigger config)

/-- Use debounce async in WidgetM context. -/
def useDebounceAsyncW (delayMs : Nat) (action : α → IO β)
    (input : Reactive.Event Spider α) (config : AsyncConfig := {})
    : WidgetM (Reactive.Event Spider β) :=
  StateT.lift (useDebounceAsync delayMs action input config)

end Terminus.Reactive
