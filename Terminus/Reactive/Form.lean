/-
  Terminus Reactive - Form Components
  Structured form widgets with validation, labeled inputs, and option selectors.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Terminus.Reactive.Containers
import Terminus.Reactive.Input
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Form Field Configuration -/

/-- Configuration for a single form field. -/
structure FormFieldConfig where
  /-- Field label displayed to the user. -/
  label : String
  /-- Unique field name for form data. -/
  name : String
  /-- Whether the field is required. -/
  required : Bool := false
  /-- Placeholder text for text fields. -/
  placeholder : String := ""
  /-- Initial value. -/
  initial : String := ""
  /-- Validation function (returns error message if invalid). -/
  validate : String → Option String := fun _ => none
  deriving Inhabited

/-- Configuration for the overall form. -/
structure FormConfig where
  /-- Style for labels. -/
  labelStyle : Style := {}
  /-- Style for input fields. -/
  inputStyle : Style := {}
  /-- Style for focused input fields. -/
  focusedInputStyle : Style := { fg := .ansi .cyan }
  /-- Style for error messages. -/
  errorStyle : Style := { fg := .ansi .red }
  /-- Style for required field indicator. -/
  requiredStyle : Style := { fg := .ansi .red }
  /-- Gap between form fields. -/
  fieldGap : Nat := 1
  /-- Width of input fields. -/
  inputWidth : Nat := 30
  deriving Repr, Inhabited

/-! ## Form Result -/

/-- Result returned by form widgets. -/
structure FormResult where
  /-- Current values as (name, value) pairs. -/
  values : Reactive.Dynamic Spider (Array (String × String))
  /-- Whether all fields are valid. -/
  isValid : Reactive.Dynamic Spider Bool
  /-- Current validation errors as (name, error) pairs. -/
  errors : Reactive.Dynamic Spider (Array (String × String))
  /-- Event fired when form is submitted (contains values). -/
  onSubmit : Reactive.Event Spider (Array (String × String))
  /-- Event fired when form is cancelled. -/
  onCancel : Reactive.Event Spider Unit

/-! ## Option Selector -/

/-- Configuration for option selector. -/
structure OptionSelectorConfig where
  /-- Style for unselected options. -/
  style : Style := {}
  /-- Style for selected option. -/
  selectedStyle : Style := { fg := .ansi .cyan, modifier := { bold := true } }
  /-- Style when focused. -/
  focusedStyle : Style := { bg := .ansi .blue }
  /-- Separator between options (for horizontal layout). -/
  separator : String := "  "
  /-- Show options horizontally. -/
  horizontal : Bool := true
  /-- Prefix for selected option. -/
  selectedPrefix : String := "● "
  /-- Prefix for unselected option. -/
  unselectedPrefix : String := "○ "
  deriving Repr, Inhabited

/-- Result from option selector. -/
structure OptionSelectorResult where
  /-- Currently selected index. -/
  selectedIndex : Reactive.Dynamic Spider Nat
  /-- Currently selected value. -/
  selectedValue : Reactive.Dynamic Spider String
  /-- Event fired when selection changes. -/
  onChange : Reactive.Event Spider Nat

/-- Create an option selector widget.

    Example:
    ```
    let priority ← optionSelector' "priority" #["Low", "Medium", "High"] 1 {}
    -- priority.selectedValue will be "Medium" initially
    ```
-/
def optionSelector' (name : String) (options : Array String) (initial : Nat := 0)
    (config : OptionSelectorConfig := {}) : WidgetM OptionSelectorResult := do
  -- Register as focusable
  let widgetName ← registerComponentW "optionSelector" (isInput := true) (nameOverride := name)

  -- Compute inputName before calling useFocusedKeyEventsW
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Create events
  let (changeEvent, fireChange) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Nat)
  let (valueEvent, fireValue) ← newTriggerEvent (t := Spider) (a := String)

  -- State
  let selectedRef ← SpiderM.liftIO (IO.mkRef (min initial (options.size - 1)))

  -- Dynamics
  let initialIdx := min initial (options.size - 1)
  let initialVal := if h : initialIdx < options.size then options[initialIdx] else ""
  let indexDyn ← holdDyn initialIdx indexEvent
  let valueDyn ← holdDyn initialVal valueEvent

  -- Focus for rendering
  let focusedInput ← useFocusedInputW

  -- Key handling
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    if options.size > 0 then
      let current ← selectedRef.get
      let ke := kd.event

      let newIdx ← match ke.code with
        | .left | .char 'h' =>
          pure (if current > 0 then current - 1 else current)
        | .right | .char 'l' =>
          pure (if current + 1 < options.size then current + 1 else current)
        | .up | .char 'k' =>
          if !config.horizontal then
            pure (if current > 0 then current - 1 else current)
          else
            pure current
        | .down | .char 'j' =>
          if !config.horizontal then
            pure (if current + 1 < options.size then current + 1 else current)
          else
            pure current
        | .home => pure 0
        | .end => pure (options.size - 1)
        | _ => pure current

      if newIdx != current then
        selectedRef.set newIdx
        fireIndex newIdx
        if h : newIdx < options.size then
          fireValue options[newIdx]
        fireChange newIdx

  -- Render
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName
  )
  let node ← focusDyn.zipWith' (fun isFocused selected =>
    -- Build option nodes functionally
    let optionNodes := options.foldl (init := (#[], 0)) fun (acc, i) opt =>
      let isSelected := i == selected
      let pfx := if isSelected then config.selectedPrefix else config.unselectedPrefix
      let style := if isSelected then
        if isFocused then { config.selectedStyle with bg := config.focusedStyle.bg }
        else config.selectedStyle
      else
        if isFocused && i == selected then config.focusedStyle
        else config.style

      let acc := acc.push (RNode.text (pfx ++ opt) style)

      let acc := if config.horizontal && i + 1 < options.size then
        acc.push (RNode.text config.separator config.style)
      else
        acc
      (acc, i + 1)
    let optionNodes := optionNodes.1

    if config.horizontal then
      RNode.row 0 {} optionNodes
    else
      RNode.column 0 {} optionNodes
  ) indexDyn
  emit node

  pure {
    selectedIndex := indexDyn
    selectedValue := valueDyn
    onChange := changeEvent
  }

/-- Create a labeled option selector. -/
def labeledOptionSelector' (label : String) (name : String) (options : Array String)
    (initial : Nat := 0) (config : OptionSelectorConfig := {})
    (theme : Theme := .dark) : WidgetM OptionSelectorResult := do
  emitStatic (RNode.text label theme.bodyStyle)
  optionSelector' name options initial config

/-! ## Checkbox -/

/-- Configuration for checkbox. -/
structure CheckboxConfig where
  /-- Style for label. -/
  labelStyle : Style := {}
  /-- Style when focused. -/
  focusedStyle : Style := { fg := .ansi .cyan }
  /-- Checked indicator. -/
  checkedIcon : String := "[x]"
  /-- Unchecked indicator. -/
  uncheckedIcon : String := "[ ]"
  deriving Repr, Inhabited

/-- Result from checkbox. -/
structure CheckboxResult where
  /-- Current checked state. -/
  checked : Reactive.Dynamic Spider Bool
  /-- Event fired when state changes. -/
  onChange : Reactive.Event Spider Bool

/-- Create a checkbox widget.

    Example:
    ```
    let agree ← checkbox' "agree" "I agree to the terms" false {}
    -- agree.checked will be false initially
    ```
-/
def checkbox' (name : String) (label : String) (initial : Bool := false)
    (config : CheckboxConfig := {}) : WidgetM CheckboxResult := do
  -- Register as focusable
  let widgetName ← registerComponentW "checkbox" (isInput := true) (nameOverride := name)

  -- Compute inputName before calling useFocusedKeyEventsW
  let inputName := if name.isEmpty then widgetName else name

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Filter to toggle events (space/enter)
  let toggleEvents ← Event.filterM (fun kd =>
    match kd.event.code with
    | .space | .enter => true
    | _ => false) keyEvents

  -- Fold toggle events to track checked state
  let checkedDyn ← foldDyn (fun _ checked => !checked) initial toggleEvents

  -- Focus for rendering
  let focusedInput ← useFocusedInputW

  -- Render
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName
  )
  let node ← focusDyn.zipWith' (fun isFocused checked =>
    let icon := if checked then config.checkedIcon else config.uncheckedIcon
    let style := if isFocused then config.focusedStyle else config.labelStyle
    RNode.text (icon ++ " " ++ label) style
  ) checkedDyn
  emit node

  pure {
    checked := checkedDyn
    onChange := checkedDyn.updated
  }

/-! ## Form Widget -/

/-- Create a form with multiple fields.

    The form handles:
    - Tab/Shift+Tab navigation between fields
    - Field validation
    - Submit (when all fields valid) and cancel events

    Example:
    ```
    let fields := #[
      { label := "Name", name := "name", required := true },
      { label := "Email", name := "email", validate := validateEmail }
    ]
    let form ← form' fields { fieldGap := 1 } theme
    -- form.onSubmit fires with all field values
    ```
-/
def form' (fields : Array FormFieldConfig) (config : FormConfig := {})
    (_theme : Theme := .dark) : WidgetM FormResult := do
  let events ← getEventsW

  -- Create events for the form
  let (submitEvent, fireSubmit) ← newTriggerEvent (t := Spider) (a := Array (String × String))
  let (cancelEvent, fireCancel) ← newTriggerEvent (t := Spider) (a := Unit)

  -- Create tracking for values and errors
  let valuesRef ← SpiderM.liftIO (IO.mkRef (fields.map fun f => (f.name, f.initial)))
  let errorsRef ← SpiderM.liftIO (IO.mkRef (#[] : Array (String × String)))

  -- Create dynamics
  let (valuesEvent, fireValues) ← newTriggerEvent (t := Spider) (a := Array (String × String))
  let (errorsEvent, fireErrors) ← newTriggerEvent (t := Spider) (a := Array (String × String))
  let (validEvent, fireValid) ← newTriggerEvent (t := Spider) (a := Bool)

  let initialValues := fields.map fun f => (f.name, f.initial)
  let valuesDyn ← holdDyn initialValues valuesEvent
  let errorsDyn ← holdDyn #[] errorsEvent
  let validDyn ← holdDyn true validEvent

  -- Validation helper
  let validateAll : IO (Bool × Array (String × String)) := do
    let values ← valuesRef.get
    let mut allErrors : Array (String × String) := #[]
    let mut isValid := true

    for field in fields do
      let value := (values.find? fun (n, _) => n == field.name).map (·.2) |>.getD ""

      -- Check required
      if field.required && value.isEmpty then
        allErrors := allErrors.push (field.name, "This field is required")
        isValid := false
      else
        -- Run custom validation
        match field.validate value with
        | some err =>
          allErrors := allErrors.push (field.name, err)
          isValid := false
        | none => pure ()

    pure (isValid, allErrors)

  -- Render fields
  column' (gap := config.fieldGap) {} do
    for field in fields do
      -- Field label with required indicator
      let labelText := if field.required then
        field.label ++ " *"
      else
        field.label

      row' (gap := 1) {} do
        text' labelText config.labelStyle

      -- Text input for the field
      let input ← textInput' field.name field.initial {
        placeholder := field.placeholder
        width := config.inputWidth
        style := config.inputStyle
        focusedStyle := config.focusedInputStyle
      }

      -- Track value changes
      let _unsub ← SpiderM.liftIO <| input.onChange.subscribe fun newValue => do
        let values ← valuesRef.get
        let newValues := values.map fun (n, v) =>
          if n == field.name then (n, newValue) else (n, v)
        valuesRef.set newValues
        fireValues newValues

        -- Re-validate
        let (isValid, errors) ← validateAll
        errorsRef.set errors
        fireErrors errors
        fireValid isValid

      -- Show error for this field
      let node ← errorsDyn.map' (fun errors =>
        match errors.find? fun (n, _) => n == field.name with
        | some (_, err) => RNode.text err config.errorStyle
        | none => RNode.empty
      )
      emit node

  -- Handle form-level keys (Escape to cancel)
  let _unsub ← SpiderM.liftIO <| events.keyEvent.subscribe fun kd => do
    let ke := kd.event
    match ke.code with
    | .escape => fireCancel ()
    | .enter =>
      if ke.modifiers.ctrl then
        let (isValid, _) ← validateAll
        if isValid then
          let values ← valuesRef.get
          fireSubmit values
    | _ => pure ()

  pure {
    values := valuesDyn
    isValid := validDyn
    errors := errorsDyn
    onSubmit := submitEvent
    onCancel := cancelEvent
  }

/-! ## Convenience Functions -/

/-- Create a labeled text input with inline label. -/
def inlineLabeledInput' (label : String) (name : String) (initial : String := "")
    (config : TextInputConfig := {}) (labelStyle : Style := {}) : WidgetM TextInputResult := do
  row' (gap := 1) {} do
    text' (label ++ ":") labelStyle
    textInput' name initial config

/-- Create a field group with a border. -/
def fieldGroup' (title : String) (theme : Theme := .dark)
    (content : WidgetM α) : WidgetM α :=
  titledBlock' title .rounded theme none content

/-- Create a submit button. -/
def submitButton' (label : String := "Submit") (enabled : Reactive.Dynamic Spider Bool)
    (style : Style := {}) (disabledStyle : Style := { fg := .ansi .brightBlack })
    : WidgetM (Reactive.Event Spider Unit) := do
  let widgetName ← registerComponentW "submitButton" (isInput := true)

  -- Get focused key events, gated by enabled state
  let keyEvents ← useFocusedKeyEventsW widgetName
  let enabledKeyEvents ← Event.gateM enabled.current keyEvents

  -- Filter to submit keys (Enter/Space) and convert to Unit event
  let submitKeyEvents ← Event.filterM (fun kd =>
    match kd.event.code with
    | .enter | .space => true
    | _ => false
  ) enabledKeyEvents
  let clickEvent ← Event.voidM submitKeyEvents

  -- Focus for rendering
  let focusedInput ← useFocusedInputW

  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some widgetName
  )
  let node ← enabled.zipWith' (fun isEnabled isFocused =>
    let displayStyle := if !isEnabled then
      disabledStyle
    else if isFocused then
      { style with modifier := { bold := true } }
    else
      style

    let text := if isFocused then s!"[ {label} ]" else s!"  {label}  "
    RNode.text text displayStyle
  ) focusDyn
  emit node

  pure clickEvent

/-- Create a cancel button. -/
def cancelButton' (label : String := "Cancel") (style : Style := { fg := .ansi .brightBlack })
    : WidgetM (Reactive.Event Spider Unit) := do
  let widgetName ← registerComponentW "cancelButton" (isInput := true)

  -- Get focused key events
  let keyEvents ← useFocusedKeyEventsW widgetName

  -- Filter to cancel keys (Enter/Space/Escape) and convert to Unit event
  let cancelKeyEvents ← Event.filterM (fun kd =>
    match kd.event.code with
    | .enter | .space | .escape => true
    | _ => false) keyEvents
  let clickEvent ← Event.voidM cancelKeyEvents

  -- Focus for rendering
  let focusedInput ← useFocusedInputW

  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some widgetName
  )
  let node ← focusDyn.map' (fun isFocused =>
    let displayStyle := if isFocused then
      { style with modifier := { bold := true } }
    else
      style

    let text := if isFocused then s!"[ {label} ]" else s!"  {label}  "
    RNode.text text displayStyle
  )
  emit node

  pure clickEvent

end Terminus.Reactive
