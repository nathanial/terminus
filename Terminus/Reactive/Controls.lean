/-
  Terminus Reactive - Input Control Widgets
  Button, Switch, Stepper, Slider, and PasswordInput widgets.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Button Widget -/

/-- Configuration for button appearance. -/
structure ButtonConfig where
  /-- Style for unfocused button. -/
  style : Style := {}
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Padding around the label. -/
  padding : Nat := 1
  deriving Repr, Inhabited

/-- Result from button widget. -/
structure ButtonResult where
  /-- Event fired when button is clicked (Space/Enter). -/
  onClick : Reactive.Event Spider Unit

/-- Create a button widget.

    Example:
    ```
    let btn ← button' "submit" "Submit" {}
    performEvent_ (← Event.mapM (fun _ => submitForm) btn.onClick)
    ```
-/
def button' (name : String) (label : String) (config : ButtonConfig := {})
    : WidgetM ButtonResult := do
  -- Register as focusable
  let widgetName ← registerComponentW "button" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Filter to activation keys (Space/Enter)
  let activateEvents ← Event.filterM (fun kd =>
    match kd.event.code with
    | .space | .enter => true
    | _ => false) keyEvents
  let clickEvent ← Event.voidM activateEvents

  -- Focus for rendering
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render
  let node ← focusDyn.map' fun isFocused =>
    let displayStyle := if isFocused then config.focusedStyle else config.style
    let pad := String.ofList (List.replicate config.padding ' ')
    let text := if isFocused then s!"[{pad}{label}{pad}]" else s!" {pad}{label}{pad} "
    RNode.text text displayStyle
  emit node

  pure { onClick := clickEvent }

/-! ## Switch Widget -/

/-- Configuration for switch appearance. -/
structure SwitchConfig where
  /-- Label shown when on. -/
  onLabel : String := " ON"
  /-- Label shown when off. -/
  offLabel : String := "OFF"
  /-- Style for the on state. -/
  onStyle : Style := { fg := .ansi .green }
  /-- Style for the off state. -/
  offStyle : Style := { fg := .ansi .brightBlack }
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  deriving Repr, Inhabited

/-- Result from switch widget. -/
structure SwitchResult where
  /-- Current on/off state. -/
  isOn : Reactive.Dynamic Spider Bool
  /-- Event fired when state changes. -/
  onToggle : Reactive.Event Spider Bool

/-- Create a switch (toggle) widget.

    Example:
    ```
    let sw ← switch' "darkMode" false {}
    let _ ← dynWidget sw.isOn fun on =>
      text' (if on then "Dark mode enabled" else "Light mode") theme.bodyStyle
    ```
-/
def switch' (name : String) (initial : Bool := false) (config : SwitchConfig := {})
    : WidgetM SwitchResult := do
  -- Register as focusable
  let widgetName ← registerComponentW "switch" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Filter to toggle events (Space/Enter)
  let toggleEvents ← Event.filterM (fun kd =>
    match kd.event.code with
    | .space | .enter => true
    | _ => false) keyEvents

  -- Fold toggle events to track state
  let isOnDyn ← foldDyn (fun _ on => !on) initial toggleEvents

  -- Focus for rendering
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render
  let node ← focusDyn.zipWith' (fun isFocused isOn =>
    let label := if isOn then config.onLabel else config.offLabel
    let stateStyle := if isOn then config.onStyle else config.offStyle
    let displayStyle := if isFocused then config.focusedStyle else stateStyle
    RNode.text s!"[{label}]" displayStyle
  ) isOnDyn
  emit node

  pure {
    isOn := isOnDyn
    onToggle := isOnDyn.updated
  }

/-! ## Stepper Widget -/

/-- Configuration for stepper appearance and behavior. -/
structure StepperConfig where
  /-- Minimum value. -/
  min : Int := 0
  /-- Maximum value. -/
  max : Int := 100
  /-- Step size for increment/decrement. -/
  step : Int := 1
  /-- Style for buttons. -/
  buttonStyle : Style := {}
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan }
  /-- Style for disabled buttons (at bounds). -/
  disabledStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for the value display. -/
  valueStyle : Style := {}
  /-- Width for value display (for alignment). -/
  valueWidth : Nat := 4
  deriving Repr, Inhabited

/-- Result from stepper widget. -/
structure StepperResult where
  /-- Current integer value. -/
  value : Reactive.Dynamic Spider Int
  /-- Event fired when value changes. -/
  onChange : Reactive.Event Spider Int

/-- Create a stepper (increment/decrement) widget.

    Example:
    ```
    let qty ← stepper' "quantity" 1 { min := 0, max := 99, step := 1 }
    let _ ← dynWidget qty.value fun n =>
      text' s!"Quantity: {n}" theme.bodyStyle
    ```
-/
def stepper' (name : String) (initial : Int := 0) (config : StepperConfig := {})
    : WidgetM StepperResult := do
  -- Register as focusable
  let widgetName ← registerComponentW "stepper" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Clamp initial value
  let clampedInitial := max config.min (min config.max initial)

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Map key events to value transformation functions
  let valueOps ← Event.mapMaybeM (fun kd =>
    let ke := kd.event
    match ke.code with
    | .up | .char 'k' | .char '+' =>
      some fun (v : Int) => min config.max (v + config.step)
    | .down | .char 'j' | .char '-' =>
      some fun (v : Int) => max config.min (v - config.step)
    | .home => some fun (_ : Int) => config.min
    | .end => some fun (_ : Int) => config.max
    | _ => none) keyEvents

  -- Fold value operations
  let valueDyn ← foldDyn (fun op v => op v) clampedInitial valueOps

  -- Focus for rendering
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render
  let node ← focusDyn.zipWith' (fun isFocused value =>
    let atMin := value <= config.min
    let atMax := value >= config.max

    let minusStyle := if atMin then config.disabledStyle
                      else if isFocused then config.focusedStyle
                      else config.buttonStyle
    let plusStyle := if atMax then config.disabledStyle
                     else if isFocused then config.focusedStyle
                     else config.buttonStyle
    let valStyle := if isFocused then config.focusedStyle else config.valueStyle

    -- Format value with padding
    let valStr := toString value
    let padLen := if valStr.length < config.valueWidth
                  then config.valueWidth - valStr.length else 0
    let leftPad := padLen / 2
    let rightPad := padLen - leftPad
    let paddedVal := String.ofList (List.replicate leftPad ' ') ++
                     valStr ++
                     String.ofList (List.replicate rightPad ' ')

    RNode.row 0 {} #[
      RNode.text "[-]" minusStyle,
      RNode.text paddedVal valStyle,
      RNode.text "[+]" plusStyle
    ]
  ) valueDyn
  emit node

  pure {
    value := valueDyn
    onChange := valueDyn.updated
  }

/-! ## Slider Widget -/

/-- Configuration for slider appearance and behavior. -/
structure SliderConfig where
  /-- Width of the slider track in characters. -/
  width : Nat := 20
  /-- Minimum value. -/
  minValue : Float := 0.0
  /-- Maximum value. -/
  maxValue : Float := 1.0
  /-- Step size for arrow keys. -/
  step : Float := 0.05
  /-- Large step size (Shift+arrow). -/
  largeStep : Float := 0.1
  /-- Character for filled portion. -/
  filledChar : Char := '='
  /-- Character for unfilled portion. -/
  unfilledChar : Char := '-'
  /-- Character for the thumb/knob. -/
  thumbChar : Char := 'O'
  /-- Whether to show the value. -/
  showValue : Bool := true
  /-- Function to format the value for display. -/
  formatValue : Float → String := fun v => s!"{(v * 100).toUInt32}%"
  /-- Style for filled portion. -/
  filledStyle : Style := { fg := .ansi .green }
  /-- Style for unfilled portion. -/
  unfilledStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for thumb. -/
  thumbStyle : Style := { fg := .ansi .cyan }
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Style for value display. -/
  valueStyle : Style := {}
  deriving Inhabited

/-- Result from slider widget. -/
structure SliderResult where
  /-- Current value (between minValue and maxValue). -/
  value : Reactive.Dynamic Spider Float
  /-- Event fired when value changes. -/
  onChange : Reactive.Event Spider Float

/-- Create a slider widget.

    Example:
    ```
    let volume ← slider' "volume" 0.5 { width := 20, showValue := true }
    let _ ← dynWidget volume.value fun v =>
      text' s!"Volume: {(v * 100).toUInt32}%" theme.bodyStyle
    ```
-/
def slider' (name : String) (initial : Float := 0.5) (config : SliderConfig := {})
    : WidgetM SliderResult := do
  -- Register as focusable
  let widgetName ← registerComponentW "slider" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Clamp initial value
  let clampedInitial := max config.minValue (min config.maxValue initial)

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Map key events to value transformation functions
  let valueOps ← Event.mapMaybeM (fun kd =>
    let ke := kd.event
    let step := if ke.modifiers.shift then config.largeStep else config.step
    match ke.code with
    | .left | .char 'h' =>
      some fun (v : Float) => max config.minValue (v - step)
    | .right | .char 'l' =>
      some fun (v : Float) => min config.maxValue (v + step)
    | .home => some fun (_ : Float) => config.minValue
    | .end => some fun (_ : Float) => config.maxValue
    | _ => none) keyEvents

  -- Fold value operations
  let valueDyn ← foldDyn (fun op v => op v) clampedInitial valueOps

  -- Focus for rendering
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render
  let node ← focusDyn.zipWith' (fun isFocused value => Id.run do
    -- Normalize value to 0.0-1.0 for rendering
    let range := config.maxValue - config.minValue
    let normalized := if range > 0 then (value - config.minValue) / range else 0.0

    -- Calculate thumb position (0 to width-1)
    let thumbPos := (normalized * (config.width - 1).toFloat).toUInt32.toNat
    let thumbPos := min thumbPos (config.width - 1)

    -- Build the track
    let mut nodes : Array RNode := #[]
    nodes := nodes.push (RNode.text "[" (if isFocused then config.focusedStyle else {}))

    for i in [:config.width] do
      let (char, style) :=
        if i == thumbPos then
          (config.thumbChar, if isFocused then config.focusedStyle else config.thumbStyle)
        else if i < thumbPos then
          (config.filledChar, config.filledStyle)
        else
          (config.unfilledChar, config.unfilledStyle)
      nodes := nodes.push (RNode.text (String.singleton char) style)

    nodes := nodes.push (RNode.text "]" (if isFocused then config.focusedStyle else {}))

    -- Add value display
    if config.showValue then
      let valStr := config.formatValue value
      nodes := nodes.push (RNode.text s!" {valStr}" config.valueStyle)

    RNode.row 0 {} nodes
  ) valueDyn
  emit node

  pure {
    value := valueDyn
    onChange := valueDyn.updated
  }

/-! ## PasswordInput Widget -/

/-- Configuration for password input appearance and behavior. -/
structure PasswordInputConfig where
  /-- Character used to mask the password. -/
  maskChar : Char := '*'
  /-- Placeholder text when empty. -/
  placeholder : String := ""
  /-- Maximum input length (none = unlimited). -/
  maxLength : Option Nat := none
  /-- Minimum display width in characters. -/
  width : Nat := 20
  /-- Style when unfocused. -/
  style : Style := {}
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan }
  /-- Style for placeholder text. -/
  placeholderStyle : Style := { fg := .ansi .brightBlack }
  /-- Cursor character. -/
  cursorChar : Char := '|'
  /-- Style for cursor. -/
  cursorStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Key to toggle reveal (Tab by default). -/
  revealKey : KeyCode := .tab
  /-- Style for reveal indicator. -/
  revealIndicatorStyle : Style := { fg := .ansi .brightBlack }
  deriving Inhabited

/-- Result from password input widget. -/
structure PasswordInputResult where
  /-- Current password value (unmasked). -/
  value : Reactive.Dynamic Spider String
  /-- Whether password is currently revealed. -/
  isRevealed : Reactive.Dynamic Spider Bool
  /-- Event fired when Enter is pressed. -/
  onSubmit : Reactive.Event Spider String
  /-- Event fired when Escape is pressed. -/
  onCancel : Reactive.Event Spider Unit
  /-- Event fired when text changes. -/
  onChange : Reactive.Event Spider String

/-- Internal state for password input. -/
private structure PasswordState where
  /-- Current text content. -/
  text : String := ""
  /-- Cursor position. -/
  cursor : Nat := 0
  /-- Whether password is revealed. -/
  revealed : Bool := false
  deriving Repr, Inhabited

/-- Create a password input widget.

    Similar to textInput' but masks the input by default.
    Press Tab (configurable) to toggle reveal.

    Example:
    ```
    let pwd ← passwordInput' "password" { placeholder := "Enter password" }
    performEvent_ (← Event.mapM (fun pass => authenticate pass) pwd.onSubmit)
    ```
-/
def passwordInput' (name : String) (config : PasswordInputConfig := {})
    : WidgetM PasswordInputResult := do
  -- Register as focusable
  let widgetName ← registerComponentW "passwordInput" (isInput := true) (nameOverride := name)
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- State
  let initialState : PasswordState := {}

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun kd =>
    let ke := kd.event
    -- Reveal toggle
    if ke.code == config.revealKey then
      some fun (state : PasswordState) => { state with revealed := !state.revealed }
    else
      match ke.code with
      | .char c =>
        if c.val >= 32 then
          some fun (state : PasswordState) =>
            match config.maxLength with
            | some maxLen =>
              if state.text.length >= maxLen then state
              else
                let before := state.text.take state.cursor
                let after := state.text.drop state.cursor
                { state with text := before ++ c.toString ++ after, cursor := state.cursor + 1 }
            | none =>
              let before := state.text.take state.cursor
              let after := state.text.drop state.cursor
              { state with text := before ++ c.toString ++ after, cursor := state.cursor + 1 }
        else none
      | .backspace =>
        some fun (state : PasswordState) =>
          if state.cursor == 0 then state
          else
            let before := state.text.take (state.cursor - 1)
            let after := state.text.drop state.cursor
            { state with text := before ++ after, cursor := state.cursor - 1 }
      | .delete =>
        some fun (state : PasswordState) =>
          if state.cursor >= state.text.length then state
          else
            let before := state.text.take state.cursor
            let after := state.text.drop (state.cursor + 1)
            { state with text := before ++ after }
      | .left =>
        some fun (state : PasswordState) =>
          if state.cursor == 0 then state
          else { state with cursor := state.cursor - 1 }
      | .right =>
        some fun (state : PasswordState) =>
          if state.cursor >= state.text.length then state
          else { state with cursor := state.cursor + 1 }
      | .home => some fun (state : PasswordState) => { state with cursor := 0 }
      | .end => some fun (state : PasswordState) => { state with cursor := state.text.length }
      | _ => none) keyEvents

  -- Fold state operations
  let stateDyn ← foldDyn (fun op state => op state) initialState stateOps

  -- Derive value and reveal dynamics from state
  let valueDyn ← stateDyn.map' (·.text)
  let revealDyn ← stateDyn.map' (·.revealed)

  -- Derive submit event: fires current text when Enter pressed
  let enterEvents ← Event.filterM (fun kd => kd.event.code == .enter) keyEvents
  let submitEvent ← Event.attachWithM (fun state _ => state.text) stateDyn.current enterEvents

  -- Derive cancel event: fires when Escape pressed
  let cancelEvent ← Event.filterM (fun kd => kd.event.code == .escape) keyEvents
  let cancelEvent ← Event.mapM (fun _ => ()) cancelEvent

  -- Derive change event from text changes
  let changeEvent ← Event.mapM (·.text) stateDyn.updated

  -- Focus for rendering
  let focusedInput ← useFocusedInputW
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName)

  -- Render
  let node ← focusDyn.zipWith' (fun isFocused state => Id.run do
    let displayText := if state.revealed then state.text
                       else String.ofList (List.replicate state.text.length config.maskChar)

    let isEmpty := state.text.isEmpty
    let showPlaceholder := isEmpty && !isFocused

    let textStyle := if showPlaceholder then config.placeholderStyle
                     else if isFocused then config.focusedStyle
                     else config.style

    let mut nodes : Array RNode := #[]

    if isFocused && !isEmpty then
      let displayBefore := if state.revealed then state.text.take state.cursor
                           else String.ofList (List.replicate state.cursor config.maskChar)
      let displayAfter := if state.revealed then state.text.drop state.cursor
                          else String.ofList (List.replicate (state.text.length - state.cursor) config.maskChar)

      if !displayBefore.isEmpty then
        nodes := nodes.push (RNode.text displayBefore textStyle)
      nodes := nodes.push (RNode.text (String.singleton config.cursorChar) config.cursorStyle)
      if !displayAfter.isEmpty then
        nodes := nodes.push (RNode.text displayAfter textStyle)

      let currentLen := state.text.length + 1
      let padding := if currentLen < config.width then config.width - currentLen else 0
      if padding > 0 then
        nodes := nodes.push (RNode.text (String.ofList (List.replicate padding ' ')) textStyle)
    else if isFocused then
      nodes := nodes.push (RNode.text (String.singleton config.cursorChar) config.cursorStyle)
      let padding := if config.width > 1 then config.width - 1 else 0
      if padding > 0 then
        nodes := nodes.push (RNode.text (String.ofList (List.replicate padding ' ')) textStyle)
    else
      let text := if showPlaceholder then config.placeholder else displayText
      let padding := if text.length < config.width then config.width - text.length else 0
      if text.isEmpty then
        nodes := nodes.push (RNode.text (String.ofList (List.replicate config.width ' ')) textStyle)
      else
        nodes := nodes.push (RNode.text text textStyle)
        if padding > 0 then
          nodes := nodes.push (RNode.text (String.ofList (List.replicate padding ' ')) textStyle)

    -- Add reveal indicator
    let revealIndicator := if state.revealed then "[o]" else "[*]"
    nodes := nodes.push (RNode.text s!" {revealIndicator}" config.revealIndicatorStyle)

    RNode.row 0 {} nodes
  ) stateDyn
  emit node

  pure {
    value := valueDyn
    isRevealed := revealDyn
    onSubmit := submitEvent
    onCancel := cancelEvent
    onChange := changeEvent
  }

end Terminus.Reactive
