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

  -- Get focus state (for rendering)
  let focusedInput ← useFocusedInputW

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName

  -- Map key events to state transformation functions (toggle on space/enter)
  let stateOps ← Event.mapMaybeM (fun kd =>
    match kd.event.code with
    | .enter | .space => some fun (checked : Bool) => !checked
    | _ => none) keyEvents

  -- Fold state operations - no subscribe needed!
  let checkedDyn ← foldDyn (fun op state => op state) initial stateOps

  -- Derive toggle event from state updates
  let toggleEvent ← Event.mapM id checkedDyn.updated

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

  -- Initialize state with clamped initial selection
  let clampedInitial := match initial with
    | none => none
    | some idx => if idx < options.size then some idx else none

  let initialState : RadioState := { selected := clampedInitial, scrollOffset := 0 }

  -- Get focus state (for rendering)
  let focusedInput ← useFocusedInputW

  -- Determine max visible
  let maxVis := config.maxVisible.getD options.size

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName config.globalKeys

  -- Map key events to state transformation functions
  let stateOps ← Event.mapMaybeM (fun kd =>
    match kd.event.code with
    | .up | .char 'k' =>
      some fun (state : RadioState) => state.moveUp options.size config.wrapAround |>.adjustScroll maxVis
    | .down | .char 'j' =>
      some fun (state : RadioState) => state.moveDown options.size config.wrapAround |>.adjustScroll maxVis
    | .home =>
      some fun (state : RadioState) => state.moveToFirst.adjustScroll maxVis
    | .end =>
      some fun (state : RadioState) => state.moveToLast options.size |>.adjustScroll maxVis
    | _ => none) keyEvents

  -- Fold state operations - no subscribe needed!
  let stateDyn ← foldDyn (fun op state => op state) initialState stateOps

  -- Derive selectedIndex from state
  let selectedIndexDyn ← stateDyn.map' (·.selected)

  -- Derive selectedLabel from state
  let initialLabel := match clampedInitial with
    | none => none
    | some idx => if h : idx < options.size then some options[idx] else none
  let selectedLabelDyn ← stateDyn.map' fun state =>
    match state.selected with
    | some idx => if h : idx < options.size then some options[idx] else none
    | none => none

  -- Enter/space key event for selection confirmation
  let confirmE ← Event.filterM (fun kd => kd.event.code == .enter || kd.event.code == .space) keyEvents

  -- Derive select event (attach current state's selected index when Enter/Space is pressed)
  let selectEvent ← Event.mapMaybeM (fun (state, _) => state.selected)
    (← Event.attachWithM (fun (s : RadioState) (kd : KeyData) => (s, kd)) stateDyn.current confirmE)

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

    This implementation uses pure FRP-idiomatic state management via foldDyn,
    eliminating all subscribe and sample patterns.

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

  -- Get focus state
  let focusedInput ← useFocusedInputW

  -- Compute input name for focus handling
  let inputName := if config.focusName.isEmpty then
    (if name.isEmpty then widgetName else name)
  else
    config.focusName

  -- Get key events filtered by focus
  let keyEvents ← useFocusedKeyEventsW inputName config.globalKeys

  -- Create state operations from key events (attached with current options)
  let keyOpsWithOpts ← Event.attachWithM (fun (currentOpts : Array String) (kd : KeyData) =>
    (currentOpts, kd)) options.current keyEvents

  let keyStateOps ← Event.mapMaybeM (fun (currentOpts, kd) =>
    if currentOpts.isEmpty then none
    else
      let maxVis := config.maxVisible.getD currentOpts.size
      match kd.event.code with
      | .up | .char 'k' =>
        some fun (state : DynRadioState) =>
          let newRadio := state.radioState.moveUp currentOpts.size config.wrapAround |>.adjustScroll maxVis
          let newLabel := match newRadio.selected with
            | some idx => if h : idx < currentOpts.size then some currentOpts[idx] else none
            | none => none
          { radioState := newRadio, lastLabel := newLabel }
      | .down | .char 'j' =>
        some fun (state : DynRadioState) =>
          let newRadio := state.radioState.moveDown currentOpts.size config.wrapAround |>.adjustScroll maxVis
          let newLabel := match newRadio.selected with
            | some idx => if h : idx < currentOpts.size then some currentOpts[idx] else none
            | none => none
          { radioState := newRadio, lastLabel := newLabel }
      | .home =>
        some fun (state : DynRadioState) =>
          let newRadio := state.radioState.moveToFirst.adjustScroll maxVis
          let newLabel := match newRadio.selected with
            | some idx => if h : idx < currentOpts.size then some currentOpts[idx] else none
            | none => none
          { radioState := newRadio, lastLabel := newLabel }
      | .end =>
        some fun (state : DynRadioState) =>
          let newRadio := state.radioState.moveToLast currentOpts.size |>.adjustScroll maxVis
          let newLabel := match newRadio.selected with
            | some idx => if h : idx < currentOpts.size then some currentOpts[idx] else none
            | none => none
          { radioState := newRadio, lastLabel := newLabel }
      | _ => none) keyOpsWithOpts

  -- Create state operations from options changes
  let optStateOps ← Event.mapM (fun newOpts =>
    fun (state : DynRadioState) =>
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
        | some idx => if h : idx < newOpts.size then some newOpts[idx] else none
        | none => none
      { radioState := newRadioState, lastLabel := newLabel }
  ) options.updated

  -- Merge key operations and options change operations
  let allStateOps ← Event.mergeM keyStateOps optStateOps

  -- Initialize state with the provided initial index
  -- The label will be computed lazily when we first get options
  let initialRadioState : RadioState := { selected := initial, scrollOffset := 0 }
  let initialState : DynRadioState := { radioState := initialRadioState, lastLabel := none }

  -- Fold all state operations - no subscribe needed!
  let dynRadioStateDyn ← foldDyn (fun op state => op state) initialState allStateOps

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

  -- Enter/space key event for selection confirmation
  let confirmE ← Event.filterM (fun kd => kd.event.code == .enter || kd.event.code == .space) keyEvents

  -- Derive select event (attach current state's selected index when Enter/Space is pressed)
  let selectEvent ← Event.mapMaybeM (fun (state, _) => state.radioState.selected)
    (← Event.attachWithM (fun (s : DynRadioState) (kd : KeyData) => (s, kd)) dynRadioStateDyn.current confirmE)

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
