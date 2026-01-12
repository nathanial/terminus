/-
  Terminus Reactive - Spinner Widget
  Animated loading indicator with multiple styles.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Spinner Styles -/

/-- Built-in spinner animation styles. -/
inductive SpinnerStyle where
  /-- Braille dots (default) -/
  | dots
  /-- Line rotation -/
  | line
  /-- Arc rotation -/
  | arc
  /-- Arrow rotation -/
  | arrows
  /-- Quarter blocks -/
  | blocks
  /-- Growing bar -/
  | growing
  /-- Simple ASCII -/
  | ascii
  deriving Repr, BEq, Inhabited

namespace SpinnerStyle

/-- Get the frame array for a spinner style. -/
def frames : SpinnerStyle → Array String
  | .dots    => #["⠋", "⠙", "⠹", "⠸", "⠼", "⠴", "⠦", "⠧", "⠇", "⠏"]
  | .line    => #["─", "╲", "│", "╱"]
  | .arc     => #["◜", "◠", "◝", "◞", "◡", "◟"]
  | .arrows  => #["←", "↖", "↑", "↗", "→", "↘", "↓", "↙"]
  | .blocks  => #["▖", "▘", "▝", "▗"]
  | .growing => #["▏", "▎", "▍", "▌", "▋", "▊", "▉", "█", "▉", "▊", "▋", "▌", "▍", "▎", "▏"]
  | .ascii   => #["|", "/", "-", "\\"]

/-- Get the frame at a given index (wrapping). -/
def frameAt (style : SpinnerStyle) (idx : Nat) : String :=
  let fs := style.frames
  if fs.isEmpty then "?" else fs[idx % fs.size]!

end SpinnerStyle

/-! ## Spinner Configuration -/

/-- Configuration for spinner appearance. -/
structure SpinnerConfig where
  /-- Animation style. -/
  style : SpinnerStyle := .dots
  /-- Style for the spinner character. -/
  textStyle : Style := {}
  /-- Style for the optional label. -/
  labelStyle : Style := { fg := .ansi .brightBlack }
  deriving Repr, Inhabited

/-! ## Spinner Result -/

/-- Result returned by spinner widget. -/
structure SpinnerResult where
  /-- Current frame character as a Dynamic. -/
  frame : Reactive.Dynamic Spider String

/-! ## Spinner Widget -/

/-- Create a spinner with manual frame control.

    The frame index must be updated externally (e.g., from a tick counter).

    Example:
    ```
    let tickCount ← useCounter tickEvent
    let frameIdx ← holdDyn 0 (tickCount.updated.map (· / 5))  -- Advance every 5 ticks
    let spinner ← spinner' (some "Loading...") frameIdx {}
    ```
-/
def spinner' (label : Option String := none) (frameIndex : Reactive.Dynamic Spider Nat)
    (config : SpinnerConfig := {}) : WidgetM SpinnerResult := do
  -- Map frame index to frame string using FRP combinator
  let frameDyn ← Dynamic.mapM (fun idx => config.style.frameAt idx) frameIndex

  let node ← frameDyn.map' fun frameStr =>
    match label with
    | some lbl =>
      RNode.row 0 {} #[
        RNode.text frameStr config.textStyle,
        RNode.text " " {},
        RNode.text lbl config.labelStyle
      ]
    | none =>
      RNode.text frameStr config.textStyle
  emit node

  pure { frame := frameDyn }

/-- Create an animated spinner that auto-advances with tick events.

    This is the most common use case - the spinner automatically
    advances frames at the specified interval.

    Example:
    ```
    let loading ← animatedSpinner' (some "Loading...") 80 {}
    -- Spinner auto-advances every 80ms
    ```
-/
def animatedSpinner' (label : Option String := none) (intervalMs : Nat := 80)
    (config : SpinnerConfig := {}) : WidgetM SpinnerResult := do
  -- Get tick events
  let tickEvent ← useTickW
  let _elapsedMs ← useElapsedMsW

  -- Calculate frame index from elapsed time
  let frameIndexRef ← SpiderM.liftIO (IO.mkRef 0)
  let lastUpdateRef ← SpiderM.liftIO (IO.mkRef 0)

  -- Create frame dynamic
  let (frameEvent, fireFrame) ← newTriggerEvent (t := Spider) (a := String)
  let frameDyn ← holdDyn (config.style.frameAt 0) frameEvent

  -- Subscribe to tick events
  let _unsub ← SpiderM.liftIO <| tickEvent.subscribe fun td => do
    let lastUpdate ← lastUpdateRef.get
    if td.elapsedMs - lastUpdate >= intervalMs then
      lastUpdateRef.set td.elapsedMs
      let idx ← frameIndexRef.get
      let newIdx := idx + 1
      frameIndexRef.set newIdx
      fireFrame (config.style.frameAt newIdx)

  let node ← frameDyn.map' fun frameStr =>
    match label with
    | some lbl =>
      RNode.row 0 {} #[
        RNode.text frameStr config.textStyle,
        RNode.text " " {},
        RNode.text lbl config.labelStyle
      ]
    | none =>
      RNode.text frameStr config.textStyle
  emit node

  pure { frame := frameDyn }

/-! ## Convenience Functions -/

/-- Create a simple loading spinner with a message. -/
def loadingSpinner' (message : String := "Loading...") (intervalMs : Nat := 80)
    (style : Style := { fg := .ansi .cyan }) : WidgetM SpinnerResult :=
  animatedSpinner' (some message) intervalMs { textStyle := style }

/-- Create a custom frame spinner (for non-standard animations). -/
def customSpinner' (frames : Array String) (label : Option String := none)
    (intervalMs : Nat := 80) (config : SpinnerConfig := {}) : WidgetM SpinnerResult := do
  if frames.isEmpty then
    -- Empty frames - just show placeholder
    emitStatic (RNode.text "?" config.textStyle)
    let (neverEvent, _) ← newTriggerEvent (t := Spider) (a := String)
    let dyn ← holdDyn "?" neverEvent
    pure { frame := dyn }
  else
    -- Get tick events
    let tickEvent ← useTickW

    let frameIndexRef ← SpiderM.liftIO (IO.mkRef 0)
    let lastUpdateRef ← SpiderM.liftIO (IO.mkRef 0)

    let (frameEvent, fireFrame) ← newTriggerEvent (t := Spider) (a := String)
    let frameDyn ← holdDyn frames[0]! frameEvent

    let _unsub ← SpiderM.liftIO <| tickEvent.subscribe fun td => do
      let lastUpdate ← lastUpdateRef.get
      if td.elapsedMs - lastUpdate >= intervalMs then
        lastUpdateRef.set td.elapsedMs
        let idx ← frameIndexRef.get
        let newIdx := (idx + 1) % frames.size
        frameIndexRef.set newIdx
        if h : newIdx < frames.size then
          fireFrame frames[newIdx]

    let node ← frameDyn.map' fun frameStr =>
      match label with
      | some lbl =>
        RNode.row 0 {} #[
          RNode.text frameStr config.textStyle,
          RNode.text " " {},
          RNode.text lbl config.labelStyle
        ]
      | none =>
        RNode.text frameStr config.textStyle
    emit node

    pure { frame := frameDyn }

end Terminus.Reactive
