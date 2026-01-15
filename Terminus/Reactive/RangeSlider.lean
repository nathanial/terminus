/-
  Terminus Reactive - Range Slider Widget
  Two-handle slider for selecting a min/max range.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Range Slider Widget -/

/-- Configuration for range slider appearance and behavior. -/
structure RangeSliderConfig where
  /-- Width of the slider track in characters. -/
  width : Nat := 20
  /-- Minimum value of the slider range. -/
  minValue : Float := 0.0
  /-- Maximum value of the slider range. -/
  maxValue : Float := 1.0
  /-- Step size for arrow keys. -/
  step : Float := 0.05
  /-- Large step size (Shift+arrow). -/
  largeStep : Float := 0.1
  /-- Character for the track (between handles). -/
  trackChar : Char := '-'
  /-- Character for the filled/selected range. -/
  fillChar : Char := '='
  /-- Character for the minimum handle. -/
  minHandleChar : Char := '|'
  /-- Character for the maximum handle. -/
  maxHandleChar : Char := '|'
  /-- Whether to show the value range. -/
  showValues : Bool := true
  /-- Style for the active (focused) handle. -/
  activeStyle : Style := { fg := .ansi .cyan }
  /-- Style for the inactive handle. -/
  inactiveStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for the filled range portion. -/
  fillStyle : Style := { fg := .ansi .green }
  /-- Style for the track (unfilled portions). -/
  trackStyle : Style := { fg := .ansi .brightBlack }
  /-- Style when the widget is focused. -/
  focusedStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Style for value display. -/
  valueStyle : Style := {}
  /-- Function to format values for display. -/
  formatValue : Float → String := fun v => s!"{(v * 100).toUInt32}%"
  deriving Inhabited

/-- Result from range slider widget. -/
structure RangeSliderResult where
  /-- Current minimum value (between config.minValue and maxValue Dynamic). -/
  minValue : Dynamic Spider Float
  /-- Current maximum value (between minValue Dynamic and config.maxValue). -/
  maxValue : Dynamic Spider Float
  /-- Which handle is currently active (false = min, true = max). -/
  activeHandle : Dynamic Spider Bool
  /-- Event fired when either value changes. -/
  onChange : Reactive.Event Spider (Float × Float)

/-- Internal state for range slider. -/
private structure RangeSliderState where
  /-- Current minimum handle value. -/
  minVal : Float
  /-- Current maximum handle value. -/
  maxVal : Float
  /-- Which handle is active (false = min, true = max). -/
  activeIsMax : Bool
  deriving Repr, Inhabited

/-- Create a range slider widget.

    A two-handle slider for selecting a min/max range.
    Visual: `[==|-------|==] 20-80%`

    Controls:
    - Tab: Switch between min and max handles
    - Left/Right arrows: Move the active handle
    - Shift+Left/Right: Move by large step
    - Home/End: Move to min/max bounds
    - h/l: Vim-style movement

    Example:
    ```
    let range ← rangeSlider' "priceRange" 0.2 0.8 { width := 30, showValues := true }
    let _ ← dynWidget (range.minValue.zipWith' (·, ·) range.maxValue) fun (lo, hi) =>
      text' s!"Price: {(lo * 100).toUInt32}% - {(hi * 100).toUInt32}%" theme.bodyStyle
    ```
-/
def rangeSlider' (name : String) (initialMin : Float := 0.25) (initialMax : Float := 0.75)
    (config : RangeSliderConfig := {}) : WidgetM RangeSliderResult := do
  -- Register as focusable
  let widgetName ← registerComponentW "rangeSlider" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Clamp initial values
  let clampedMin := max config.minValue (min config.maxValue initialMin)
  let clampedMax := max clampedMin (min config.maxValue initialMax)

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  let initialState : RangeSliderState := {
    minVal := clampedMin
    maxVal := clampedMax
    activeIsMax := false  -- Start with min handle active
  }

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun kd =>
    let ke := kd.event
    let step := if ke.modifiers.shift then config.largeStep else config.step
    match ke.code with
    | .tab =>
      some fun (state : RangeSliderState) => { state with activeIsMax := !state.activeIsMax }
    | .left | .char 'h' =>
      some fun (state : RangeSliderState) =>
        if state.activeIsMax then
          let newMax := max state.minVal (state.maxVal - step)
          { state with maxVal := newMax }
        else
          let newMin := max config.minValue (state.minVal - step)
          { state with minVal := newMin }
    | .right | .char 'l' =>
      some fun (state : RangeSliderState) =>
        if state.activeIsMax then
          let newMax := min config.maxValue (state.maxVal + step)
          { state with maxVal := newMax }
        else
          let newMin := min state.maxVal (state.minVal + step)
          { state with minVal := newMin }
    | .home =>
      some fun (state : RangeSliderState) =>
        if state.activeIsMax then { state with maxVal := state.minVal }
        else { state with minVal := config.minValue }
    | .end =>
      some fun (state : RangeSliderState) =>
        if state.activeIsMax then { state with maxVal := config.maxValue }
        else { state with minVal := state.maxVal }
    | _ => none) keyEvents

  -- Fold state operations
  let stateDyn ← foldDyn (fun op state => op state) initialState stateOps

  -- Derived dynamics
  let minValDyn ← stateDyn.map' (·.minVal)
  let maxValDyn ← stateDyn.map' (·.maxVal)
  let activeHandleDyn ← stateDyn.map' (·.activeIsMax)

  -- Derive onChange event from state updates (only when values change)
  let changeEvent ← Event.mapM (fun state => (state.minVal, state.maxVal)) stateDyn.updated

  -- Focus for rendering
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render
  let node ← focusDyn.zipWith' (fun isFocused state => Id.run do
    -- Normalize values to 0.0-1.0 for rendering
    let range := config.maxValue - config.minValue
    let normalizedMin := if range > 0 then (state.minVal - config.minValue) / range else 0.0
    let normalizedMax := if range > 0 then (state.maxVal - config.minValue) / range else 0.0

    -- Calculate handle positions (0 to width-1)
    let minPos := (normalizedMin * (config.width - 1).toFloat).toUInt32.toNat
    let maxPos := (normalizedMax * (config.width - 1).toFloat).toUInt32.toNat
    let minPos := min minPos (config.width - 1)
    let maxPos := min maxPos (config.width - 1)

    -- Build the track
    let mut nodes : Array RNode := #[]
    nodes := nodes.push (RNode.text "[" (if isFocused then config.focusedStyle else {}))

    for i in [:config.width] do
      if i == minPos then
        -- Min handle
        let handleStyle := if isFocused then
          if !state.activeIsMax then config.activeStyle else config.inactiveStyle
        else config.inactiveStyle
        nodes := nodes.push (RNode.text (String.singleton config.minHandleChar) handleStyle)
      else if i == maxPos then
        -- Max handle
        let handleStyle := if isFocused then
          if state.activeIsMax then config.activeStyle else config.inactiveStyle
        else config.inactiveStyle
        nodes := nodes.push (RNode.text (String.singleton config.maxHandleChar) handleStyle)
      else if i > minPos && i < maxPos then
        -- Filled range between handles
        nodes := nodes.push (RNode.text (String.singleton config.fillChar) config.fillStyle)
      else
        -- Track outside the range
        nodes := nodes.push (RNode.text (String.singleton config.trackChar) config.trackStyle)

    nodes := nodes.push (RNode.text "]" (if isFocused then config.focusedStyle else {}))

    -- Add value display
    if config.showValues then
      let minStr := config.formatValue state.minVal
      let maxStr := config.formatValue state.maxVal
      nodes := nodes.push (RNode.text s!" {minStr}-{maxStr}" config.valueStyle)

    RNode.row 0 {} nodes
  ) stateDyn
  emit node

  pure {
    minValue := minValDyn
    maxValue := maxValDyn
    activeHandle := activeHandleDyn
    onChange := changeEvent
  }

end Terminus.Reactive
