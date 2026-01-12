/-
  Terminus Reactive - Checkbox and RadioGroup Widgets
  Toggle checkboxes and mutually exclusive option selection.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Containers
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Checkbox Configuration -/

/-- Configuration for checkbox appearance and behavior. -/
structure CheckboxConfig' where
  /-- Symbol shown when checked. -/
  checkedSymbol : String := "[x]"
  /-- Symbol shown when unchecked. -/
  uncheckedSymbol : String := "[ ]"
  /-- Separator between symbol and label. -/
  labelSeparator : String := " "
  /-- Default style for checkbox. -/
  style : Style := {}
  /-- Style when checkbox is checked. -/
  checkedStyle : Style := { fg := .ansi .green }
  /-- Style when checkbox is focused. -/
  focusedStyle : Style := { fg := .ansi .cyan }
  deriving Repr, Inhabited

/-! ## Checkbox Result -/

/-- Result returned by checkbox widget containing reactive values and events. -/
structure CheckboxResult' where
  /-- Current checked state as a Dynamic. -/
  checked : Reactive.Dynamic Spider Bool
  /-- Event fired when checkbox is toggled (contains new state). -/
  onToggle : Reactive.Event Spider Bool

/-! ## Checkbox Widget -/

/-- Create a toggle checkbox with label support.

    The widget handles:
    - Space/Enter to toggle checked state
    - Focus-aware styling
    - Accessible labeling

    Example:
    ```
    let agree ← checkbox'' "terms" "I agree to the terms" false {}
    -- Use agree.checked to get current state
    -- Use agree.onToggle to handle toggle events
    ```
-/
def checkbox'' (name : String) (label : String) (initial : Bool := false)
    (config : CheckboxConfig' := {}) : WidgetM CheckboxResult' := do
  -- Register as focusable input
  let widgetName ← registerComponentW "checkbox" (isInput := true) (nameOverride := name)

  -- Compute input name for focus handling
  let inputName := if name.isEmpty then widgetName else name

  -- Create trigger events
  let (toggleEvent, fireToggle) ← newTriggerEvent (t := Spider) (a := Bool)

  -- Create dynamic for checked state
  let checkedDyn ← holdDyn initial toggleEvent

  -- Internal state ref for render sampling
  let checkedRef ← SpiderM.liftIO (IO.mkRef initial)

  -- Get focus state (for rendering)
  let focusedInput ← useFocusedInputW

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Subscribe to key events (already filtered by focus)
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    let ke := kd.event
    match ke.code with
    | .space | .enter =>
      let current ← checkedRef.get
      let newVal := !current
      checkedRef.set newVal
      fireToggle newVal
    | _ => pure ()
  let node ← focusedInput.zipWith' (fun currentFocus isChecked =>
    let isFocused := currentFocus == some inputName
    let symbol := if isChecked then config.checkedSymbol else config.uncheckedSymbol
    let textStyle := if isFocused then
      config.focusedStyle
    else if isChecked then
      config.checkedStyle
    else
      config.style
    let displayText := symbol ++ config.labelSeparator ++ label
    RNode.text displayText textStyle
  ) checkedDyn
  emit node

  pure {
    checked := checkedDyn
    onToggle := toggleEvent
  }

/-! ## RadioGroup Configuration -/

/-- Configuration for radio group appearance and behavior. -/
structure RadioConfig where
  /-- Symbol for selected option. -/
  selectedSymbol : String := "(o)"
  /-- Symbol for unselected option. -/
  unselectedSymbol : String := "( )"
  /-- Separator between symbol and label. -/
  labelSeparator : String := " "
  /-- Default style. -/
  style : Style := {}
  /-- Style for selected option. -/
  selectedStyle : Style := { fg := .ansi .cyan }
  /-- Style when focused. -/
  focusedStyle : Style := {}
  /-- Whether navigation wraps around. -/
  wrapAround : Bool := true
  /-- Maximum visible options (none = show all). -/
  maxVisible : Option Nat := none
  /-- Custom focus name (empty = auto-generated). -/
  focusName : String := ""
  /-- Whether to respond to keys globally (not just when focused). -/
  globalKeys : Bool := false
  deriving Repr, Inhabited

/-! ## RadioGroup Result -/

/-- Result returned by radio group containing reactive values and events. -/
structure RadioResult where
  /-- Currently selected index (none if empty or no selection). -/
  selectedIndex : Reactive.Dynamic Spider (Option Nat)
  /-- Currently selected label (none if empty or no selection). -/
  selectedLabel : Reactive.Dynamic Spider (Option String)
  /-- Event fired when selection changes (contains new index). -/
  onSelect : Reactive.Event Spider Nat

/-! ## RadioGroup Internal State -/

/-- Internal state for radio group. -/
structure RadioState where
  /-- Currently selected index. -/
  selected : Option Nat := none
  /-- Scroll offset for visible window. -/
  scrollOffset : Nat := 0
  deriving Repr, Inhabited, BEq

namespace RadioState

/-- Move selection up. -/
def moveUp (s : RadioState) (count : Nat) (wrap : Bool) : RadioState :=
  if count == 0 then s
  else match s.selected with
    | none => { s with selected := some (count - 1) }
    | some idx =>
      if idx > 0 then
        { s with selected := some (idx - 1) }
      else if wrap then
        { s with selected := some (count - 1) }
      else s

/-- Move selection down. -/
def moveDown (s : RadioState) (count : Nat) (wrap : Bool) : RadioState :=
  if count == 0 then s
  else match s.selected with
    | none => { s with selected := some 0 }
    | some idx =>
      if idx + 1 < count then
        { s with selected := some (idx + 1) }
      else if wrap then
        { s with selected := some 0 }
      else s

/-- Move to first option. -/
def moveToFirst (s : RadioState) : RadioState :=
  { s with selected := some 0, scrollOffset := 0 }

/-- Move to last option. -/
def moveToLast (s : RadioState) (count : Nat) : RadioState :=
  if count == 0 then s
  else { s with selected := some (count - 1) }

/-- Adjust scroll offset to keep selection visible. -/
def adjustScroll (s : RadioState) (maxVisible : Nat) : RadioState :=
  match s.selected with
  | none => s
  | some idx =>
    if maxVisible == 0 then s
    else
      let newOffset :=
        if idx < s.scrollOffset then idx
        else if idx >= s.scrollOffset + maxVisible then idx - maxVisible + 1
        else s.scrollOffset
      { s with scrollOffset := newOffset }

/-- Clamp selection to valid range when options change. -/
def clampSelection (s : RadioState) (count : Nat) : RadioState :=
  match s.selected with
  | none => s
  | some idx =>
    if count == 0 then { s with selected := none, scrollOffset := 0 }
    else if idx >= count then { s with selected := some (count - 1) }
    else s

end RadioState

/-! ## RadioGroup Widget -/

/-- Create a radio group for mutually exclusive option selection.

    The widget handles:
    - Arrow key navigation
    - Enter to confirm selection
    - Auto-scroll when exceeding visible height
    - Wrap-around navigation (configurable)

    Example:
    ```
    let priority ← radioGroup' "priority" #["Low", "Medium", "High"] (some 1) {}
    -- priority.selectedLabel will be some "Medium" initially
    ```
-/
def radioGroup' (name : String) (options : Array String) (initial : Option Nat := none)
    (config : RadioConfig := {}) : WidgetM RadioResult := do
  -- Register as focusable input
  let focusOverride := if config.focusName.isEmpty then name else config.focusName
  let widgetName ← registerComponentW "radioGroup" (isInput := true) (nameOverride := focusOverride)

  -- Compute input name for focus handling
  let inputName := if config.focusName.isEmpty then
    (if name.isEmpty then widgetName else name)
  else
    config.focusName

  -- Create trigger events
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Nat)
  let (indexEvent, fireIndex) ← newTriggerEvent (t := Spider) (a := Option Nat)
  let (labelEvent, fireLabel) ← newTriggerEvent (t := Spider) (a := Option String)

  -- Initialize state with clamped initial selection
  let clampedInitial := match initial with
    | none => none
    | some idx => if idx < options.size then some idx else none

  let initialState : RadioState := { selected := clampedInitial, scrollOffset := 0 }
  let stateRef ← SpiderM.liftIO (IO.mkRef initialState)
  let (stateEvent, fireState) ← newTriggerEvent (t := Spider) (a := RadioState)
  let stateDyn ← holdDyn initialState stateEvent

  -- Create dynamics
  let selectedIndexDyn ← holdDyn clampedInitial indexEvent
  let initialLabel := match clampedInitial with
    | none => none
    | some idx => if h : idx < options.size then some options[idx] else none
  let selectedLabelDyn ← holdDyn initialLabel labelEvent

  -- Get focus state (for rendering)
  let focusedInput ← useFocusedInputW

  -- Determine max visible
  let maxVis := config.maxVisible.getD options.size

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName config.globalKeys

  -- Subscribe to key events (already filtered by focus)
  let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
    if options.size > 0 then
      let state ← stateRef.get
      let ke := kd.event

      let newState ← match ke.code with
        | .up | .char 'k' =>
          pure (state.moveUp options.size config.wrapAround |>.adjustScroll maxVis)
        | .down | .char 'j' =>
          pure (state.moveDown options.size config.wrapAround |>.adjustScroll maxVis)
        | .home =>
          pure (state.moveToFirst.adjustScroll maxVis)
        | .end =>
          pure (state.moveToLast options.size |>.adjustScroll maxVis)
        | .enter | .space =>
          -- Fire select event on confirmation
          match state.selected with
          | some idx => fireSelect idx
          | none => pure ()
          pure state
        | _ => pure state

      -- Update state and fire events if selection changed
      if newState.selected != state.selected then
        stateRef.set newState
        fireState newState
        fireIndex newState.selected
        match newState.selected with
        | some idx =>
          if h : idx < options.size then
            fireLabel (some options[idx])
          else
            fireLabel none
        | none => fireLabel none

  let inputName := if config.focusName.isEmpty then
    (if name.isEmpty then widgetName else name)
  else
    config.focusName
  let node ← focusedInput.zipWith' (fun currentFocus state =>
    Id.run do
      let isFocused := currentFocus == some inputName

      if options.isEmpty then
        return RNode.empty
      else
        let startIdx := state.scrollOffset
        let endIdx := min (startIdx + maxVis) options.size

        let mut nodes : Array RNode := #[]
        for i in [startIdx:endIdx] do
          if h : i < options.size then
            let opt := options[i]
            let isSelected := state.selected == some i

            let symbol := if isSelected then config.selectedSymbol else config.unselectedSymbol

            let optStyle := if isSelected then
              if isFocused then
                Style.merge config.selectedStyle config.focusedStyle
              else
                config.selectedStyle
            else
              config.style

            let displayText := symbol ++ config.labelSeparator ++ opt
            nodes := nodes.push (RNode.text displayText optStyle)

        return RNode.column 0 {} nodes
  ) stateDyn
  emit node

  pure {
    selectedIndex := selectedIndexDyn
    selectedLabel := selectedLabelDyn
    onSelect := selectEvent
  }

/-! ## Dynamic RadioGroup Widget -/

/-- State for dynRadioGroup that tracks both selection and last known label. -/
private structure DynRadioState where
  radioState : RadioState
  lastLabel : Option String
  deriving Inhabited, BEq

/-- Create a radio group with dynamic options.

    When options change:
    - Selection is clamped if it exceeds new bounds
    - Selection is preserved by label when possible
    - Empty options result in no selection

    This implementation uses FRP-idiomatic state management via holdDyn and
    attachWithM, avoiding imperative subscribe patterns with IO.mkRef.

    Example:
    ```
    let opts ← holdDyn #["A", "B", "C"] optionsEvent
    let selector ← dynRadioGroup' "choice" opts (some 0) {}
    ```
-/
def dynRadioGroup' (name : String) (options : Reactive.Dynamic Spider (Array String))
    (initial : Option Nat := none) (config : RadioConfig := {}) : WidgetM RadioResult := do
  -- Register as focusable input
  let focusOverride := if config.focusName.isEmpty then name else config.focusName
  let widgetName ← registerComponentW "dynRadioGroup" (isInput := true) (nameOverride := focusOverride)

  -- Create trigger event for external selection notification
  let (selectEvent, fireSelect) ← newTriggerEvent (t := Spider) (a := Nat)

  -- Get initial options
  let initialOpts ← options.sample

  -- Initialize state with clamped initial selection
  let clampedInitial := match initial with
    | none => none
    | some idx => if idx < initialOpts.size then some idx else none

  let initialRadioState : RadioState := { selected := clampedInitial, scrollOffset := 0 }
  let initialLabel := match clampedInitial with
    | none => none
    | some idx => if h : idx < initialOpts.size then some initialOpts[idx] else none
  let initialState : DynRadioState := { radioState := initialRadioState, lastLabel := initialLabel }

  -- Get focus state
  let focusedInput ← useFocusedInputW

  -- Compute input name for focus handling
  let inputName := if config.focusName.isEmpty then
    (if name.isEmpty then widgetName else name)
  else
    config.focusName

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName config.globalKeys

  -- Create a trigger event for state updates
  let (stateUpdateEvent, fireStateUpdate) ← newTriggerEvent (t := Spider) (a := DynRadioState)

  -- Build state dynamic from the state update events
  let dynRadioStateDyn ← holdDyn initialState stateUpdateEvent

  -- Attach options to key events
  let keyEventsWithOpts ← Event.attachWithM (fun (currentOpts : Array String) (kd : KeyData) =>
    (currentOpts, kd)) options.current keyEvents

  -- Attach current state to key events
  let keyEventsWithState ← Event.attachWithM
    (fun (state : DynRadioState) (optsAndKey : Array String × KeyData) => (state, optsAndKey))
    dynRadioStateDyn.current keyEventsWithOpts

  -- Subscribe to key events to compute and fire new state
  let _keyUnsub ← SpiderM.liftIO <| keyEventsWithState.subscribe fun (state, (currentOpts, kd)) => do
    if currentOpts.isEmpty then pure ()
    else
      let maxVis := config.maxVisible.getD currentOpts.size
      let ke := kd.event
      let radioState := state.radioState
      let newRadioState := match ke.code with
        | .up | .char 'k' =>
          radioState.moveUp currentOpts.size config.wrapAround |>.adjustScroll maxVis
        | .down | .char 'j' =>
          radioState.moveDown currentOpts.size config.wrapAround |>.adjustScroll maxVis
        | .home =>
          radioState.moveToFirst.adjustScroll maxVis
        | .end =>
          radioState.moveToLast currentOpts.size |>.adjustScroll maxVis
        | .enter | .space => radioState
        | _ => radioState

      -- Handle enter/space to fire select
      match ke.code with
      | .enter | .space =>
        match radioState.selected with
        | some idx => fireSelect idx
        | none => pure ()
      | _ => pure ()

      if newRadioState != radioState then
        let newLabel := match newRadioState.selected with
          | some idx =>
            if h : idx < currentOpts.size then some currentOpts[idx] else none
          | none => none
        fireStateUpdate { radioState := newRadioState, lastLabel := newLabel }

  -- Attach current state to options changes
  let optionChangesWithState ← Event.attachWithM
    (fun (state : DynRadioState) (newOpts : Array String) => (state, newOpts))
    dynRadioStateDyn.current options.updated

  -- Subscribe to options changes to adjust selection
  let _optUnsub ← SpiderM.liftIO <| optionChangesWithState.subscribe fun (state, newOpts) => do
    let maxVis := config.maxVisible.getD newOpts.size

    -- Try to preserve selection by label
    let newSelection : Option Nat := match state.lastLabel with
      | some label =>
        match newOpts.findIdx? (· == label) with
        | some idx => some idx
        | none => state.radioState.clampSelection newOpts.size |>.selected
      | none => state.radioState.clampSelection newOpts.size |>.selected

    let newRadioState : RadioState := { selected := newSelection, scrollOffset := state.radioState.scrollOffset }
    let newRadioState := newRadioState.clampSelection newOpts.size |>.adjustScroll maxVis

    let newLabel := match newRadioState.selected with
      | some idx =>
        if h : idx < newOpts.size then some newOpts[idx] else none
      | none => none

    let newState := { radioState := newRadioState, lastLabel := newLabel }
    if newState != state then
      fireStateUpdate newState

  -- Extract radioState from DynRadioState
  let stateDyn ← dynRadioStateDyn.map' (·.radioState)

  -- Derive selectedIndex from state
  let selectedIndexDyn ← stateDyn.map' (·.selected)

  -- Derive selectedLabel by combining state with options
  let selectedLabelDyn ← stateDyn.zipWith' (fun state currentOpts =>
    match state.selected with
    | some idx =>
      if h : idx < currentOpts.size then some currentOpts[idx] else none
    | none => none
  ) options

  -- Emit render function
  let focusDyn ← focusedInput.map' (fun currentFocus =>
    currentFocus == some inputName
  )
  let stateWithOpts ← stateDyn.zipWith' (fun state currentOpts => (state, currentOpts)) options
  let node ← focusDyn.zipWith' (fun isFocused stateAndOpts =>
    Id.run do
      let (state, currentOpts) := stateAndOpts
      if currentOpts.isEmpty then
        return RNode.empty
      else
        let maxVis := config.maxVisible.getD currentOpts.size
        let startIdx := state.scrollOffset
        let endIdx := min (startIdx + maxVis) currentOpts.size

        let mut nodes : Array RNode := #[]
        for i in [startIdx:endIdx] do
          if h : i < currentOpts.size then
            let opt := currentOpts[i]
            let isSelected := state.selected == some i

            let symbol := if isSelected then config.selectedSymbol else config.unselectedSymbol

            let optStyle := if isSelected then
              if isFocused then
                Style.merge config.selectedStyle config.focusedStyle
              else
                config.selectedStyle
            else
              config.style

            let displayText := symbol ++ config.labelSeparator ++ opt
            nodes := nodes.push (RNode.text displayText optStyle)

        return RNode.column 0 {} nodes
  ) stateWithOpts
  emit node

  pure {
    selectedIndex := selectedIndexDyn
    selectedLabel := selectedLabelDyn
    onSelect := selectEvent
  }

/-! ## Helper Functions -/

/-- Create a labeled checkbox with the label positioned beside the checkbox. -/
def labeledCheckbox'' (name : String) (label : String) (initial : Bool := false)
    (config : CheckboxConfig' := {}) (_theme : Theme := .dark) : WidgetM CheckboxResult' := do
  checkbox'' name label initial config

/-- Create a labeled radio group with a title above the options. -/
def labeledRadioGroup' (title : String) (name : String) (options : Array String)
    (initial : Option Nat := none) (config : RadioConfig := {})
    (theme : Theme := .dark) : WidgetM RadioResult := do
  column' (gap := 0) {} do
    emitStatic (RNode.text title theme.bodyStyle)
    radioGroup' name options initial config

end Terminus.Reactive
