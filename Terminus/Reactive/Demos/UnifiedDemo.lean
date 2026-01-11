/-
  Terminus Unified Demo
  A single application demonstrating all Terminus reactive widgets.
-/
import Terminus.Reactive

namespace Terminus.Reactive.Demos.UnifiedDemo

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

/-! ## Tab Identifiers -/

inductive DemoTab where
  | basics
  | input
  | navigation
  | data
  | charts
  | feedback
  | media
  | async
  deriving Repr, BEq, Inhabited

def DemoTab.label : DemoTab → String
  | .basics => "Basics"
  | .input => "Input"
  | .navigation => "Navigation"
  | .data => "Data"
  | .charts => "Charts"
  | .feedback => "Feedback"
  | .media => "Media"
  | .async => "Async"

def DemoTab.allTabs : Array String := #[
  "Basics", "Input", "Navigation", "Data",
  "Charts", "Feedback", "Media", "Async"
]

def DemoTab.fromIndex : Nat → DemoTab
  | 0 => .basics
  | 1 => .input
  | 2 => .navigation
  | 3 => .data
  | 4 => .charts
  | 5 => .feedback
  | 6 => .media
  | 7 => .async
  | _ => .basics

/-! ## Basics Tab Content -/

def basicsContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Demonstrates progress bars, animations, and time tracking." theme.captionStyle

    row' (gap := 3) {} do
      -- Progress bars
      column' (gap := 1) {} do
        titledBlock' "Progress Bars" .rounded theme do
          -- Animated progress
          let tickEvents ← useTickW
          let progress ← Reactive.foldDyn (fun td _ =>
            let cycleMs := td.elapsedMs % 3000
            cycleMs.toFloat / 3000.0
          ) 0.0 tickEvents

          text' "Cycling (3s):" theme.captionStyle
          dynProgressBar' progress {
            width := 25
            filledStyle := { fg := .ansi .green }
            emptyStyle := { fg := .ansi .brightBlack }
            showPercentage := true
          }

          spacer' 0 1

          text' "Static gauges:" theme.captionStyle
          gauge' 0.65 { width := 25, filledStyle := { fg := .ansi .cyan } }
          gauge' 0.40 { width := 25, filledStyle := { fg := .ansi .yellow } }
          lineGauge' 0.75 { width := 25, label := some "Memory" }

      -- Animations
      column' (gap := 1) {} do
        titledBlock' "Animations" .rounded theme do
          let pulse ← usePulse 500
          let colorCycle ← useCycle 2000

          row' (gap := 1) {} do
            text' "Pulse:" theme.captionStyle
            emitDynamic do
              let on ← pulse.sample
              if on then
                pure (RNode.text "●" { fg := .ansi .green })
              else
                pure (RNode.text "○" { fg := .ansi .brightBlack })

          row' (gap := 1) {} do
            text' "Cycle:" theme.captionStyle
            emitDynamic do
              let p ← colorCycle.sample
              let colors := #[Color.ansi .red, .ansi .yellow, .ansi .green, .ansi .cyan, .ansi .blue, .ansi .magenta]
              let idx := (p * Float.ofNat colors.size).toUInt64.toNat % colors.size
              let color := if h : idx < colors.size then colors[idx] else .ansi .white
              pure (RNode.text "████" { fg := color })

      -- Time tracking
      column' (gap := 1) {} do
        titledBlock' "Time & Keys" .rounded theme do
          let elapsedMs ← useElapsedMsW
          let keyEvents ← useKeyEventW
          let keyCount ← Reactive.foldDyn (fun _ n => n + 1) 0 keyEvents
          let keyStrings ← Event.mapM (fun kd =>
            match kd.event.code with
            | .char c => s!"'{c}'"
            | .enter => "Enter"
            | .escape => "Escape"
            | .space => "Space"
            | .up => "Up"
            | .down => "Down"
            | _ => "other"
          ) keyEvents
          let lastKey ← Reactive.holdDyn "none" keyStrings

          row' (gap := 1) {} do
            text' "Elapsed:" theme.captionStyle
            emitDynamic do
              let ms ← elapsedMs.sample
              let seconds := ms / 1000
              let minutes := seconds / 60
              let secs := seconds % 60
              pure (RNode.text s!"{minutes}:{String.ofList (if secs < 10 then ['0'] else [])}{secs}" theme.primaryStyle)

          row' (gap := 1) {} do
            text' "Keys:" theme.captionStyle
            emitDynamic do
              let count ← keyCount.sample
              pure (RNode.text (toString count) theme.primaryStyle)

          row' (gap := 1) {} do
            text' "Last:" theme.captionStyle
            emitDynamic do
              let key ← lastKey.sample
              pure (RNode.text key theme.primaryStyle)

    spacer' 0 1

    row' (gap := 2) {} do
      -- Paragraphs
      column' (gap := 1) {} do
        titledBlock' "Paragraph" .rounded theme do
          paragraph' "Wrapped paragraph text demonstrates alignment and wrapping behavior in a constrained width." {
            wrapMode := .wrap
            maxWidth := some 28
          }
          spacer' 0 1
          centeredParagraph' "Centered paragraph" { maxWidth := some 28 }

      -- Clear widget
      column' (gap := 1) {} do
        titledBlock' "Clear" .rounded theme do
          text' "Clears a styled region:" theme.captionStyle
          clear' { width := 14, height := 3, fillChar := '.', style := { fg := .ansi .cyan } }

/-! ## Input Tab Content -/

def inputContent (theme : Theme) (_events : TerminusEvents) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Text input, checkboxes, radio buttons, and lists." theme.captionStyle

    row' (gap := 2) {} do
      -- Text input
      column' (gap := 1) {} do
        titledBlock' "Text Input" .rounded theme do
          let input ← textInput' "demo-input" "" {
            placeholder := "Type here..."
            width := 20
            focusedStyle := { fg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Value:" theme.captionStyle
            emitDynamic do
              let val ← input.value.sample
              let display := if val.isEmpty then "(empty)" else s!"\"{val}\""
              pure (RNode.text display theme.primaryStyle)

      -- Checkbox
      column' (gap := 1) {} do
        titledBlock' "Checkboxes" .rounded theme do
          text' "Space/Enter to toggle:" theme.captionStyle

          let cb1 ← checkbox'' "cb-agree" "I agree to terms" false {
            checkedStyle := { fg := .ansi .green }
          }
          let _ ← checkbox'' "cb-notify" "Send notifications" true {}

          row' (gap := 1) {} do
            text' "Agree:" theme.captionStyle
            emitDynamic do
              let checked ← cb1.checked.sample
              pure (RNode.text (if checked then "Yes" else "No") theme.primaryStyle)

      -- Radio group
      column' (gap := 1) {} do
        titledBlock' "Radio Group" .rounded theme do
          let radio ← radioGroup' "priority" #["Low", "Medium", "High"] (some 1) {
            selectedStyle := { fg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let label ← radio.selectedLabel.sample
              pure (RNode.text (label.getD "(none)") theme.primaryStyle)

    row' (gap := 2) {} do
      -- Selectable list
      column' (gap := 1) {} do
        titledBlock' "Selectable List" .rounded theme do
          let fruits := #["Apple", "Banana", "Cherry", "Date", "Elderberry"]
          let list ← selectableList' fruits 0 {
            maxVisible := some 4
            selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
            focusName := "fruit-list"
          }

          row' (gap := 1) {} do
            text' "Item:" theme.captionStyle
            emitDynamic do
              let item ← list.selectedItem.sample
              pure (RNode.text (item.getD "(none)") theme.primaryStyle)

      -- TextArea
      column' (gap := 1) {} do
        titledBlock' "Text Area" .rounded theme do
          let editor ← textArea' "editor" "Hello, World!\nMulti-line text." {
            showLineNumbers := true
            maxVisibleLines := some 4
            minWidth := 25
          }

          row' (gap := 1) {} do
            text' "Cursor:" theme.captionStyle
            emitDynamic do
              let (line, col) ← editor.cursorPos.sample
              pure (RNode.text s!"Ln {line + 1}, Col {col + 1}" theme.primaryStyle)

    row' (gap := 2) {} do
      -- List variants
      column' (gap := 1) {} do
        titledBlock' "List Variants" .rounded theme do
          text' "String list:" theme.captionStyle
          let colors := #["Red", "Green", "Blue", "Orange"]
          let colorList ← stringList' colors 0 {
            maxVisible := some 3
            focusName := "color-list"
            selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
          }

          row' (gap := 1) {} do
            text' "Picked:" theme.captionStyle
            emitDynamic do
              let item ← colorList.selectedItem.sample
              pure (RNode.text (item.getD "(none)") theme.primaryStyle)

          spacer' 0 1

          text' "Numbered list (1-9):" theme.captionStyle
          let steps := #["One", "Two", "Three", "Four"]
          let numbered ← numberedList' steps 0 {
            maxVisible := some 4
            focusName := "numbered-list"
            selectedStyle := { fg := .ansi .cyan }
          }

          row' (gap := 1) {} do
            text' "Chosen:" theme.captionStyle
            emitDynamic do
              let item ← numbered.selectedItem.sample
              pure (RNode.text (item.getD "(none)") theme.primaryStyle)

      -- Form
      column' (gap := 1) {} do
        titledBlock' "Form" .rounded theme do
          text' "Ctrl+Enter: submit | Esc: cancel" theme.captionStyle

          let statusRef ← SpiderM.liftIO (IO.mkRef "Idle")
          let fields := #[
            { label := "Name", name := "name", required := true, placeholder := "Ada" },
            { label := "Email", name := "email", required := true, placeholder := "ada@example.com",
              validate := fun v => if v.length < 3 then some "Too short" else none }
          ]

          let form ← form' fields { fieldGap := 1, inputWidth := 18 } theme

          let _ ← SpiderM.liftIO <| form.onSubmit.subscribe fun _ =>
            statusRef.set "Submitted"
          let _ ← SpiderM.liftIO <| form.onCancel.subscribe fun _ =>
            statusRef.set "Cancelled"

          spacer' 0 1

          emitDynamic do
            let ok ← form.isValid.sample
            let status ← statusRef.get
            let validity := if ok then "valid" else "invalid"
            pure (RNode.text s!"Status: {status} ({validity})" theme.captionStyle)

/-! ## Navigation Tab Content -/

def navigationContent (theme : Theme) (_events : TerminusEvents) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Tabs and tree widgets for navigation." theme.captionStyle

    row' (gap := 2) {} do
      -- Tabs demo
      column' (gap := 1) {} do
        titledBlock' "Tabs Widget" .rounded theme do
          let tabResult ← tabs' #["Home", "Settings", "Help"] 0 {
            focusName := "inner-tabs"
            activeStyle := { fg := .ansi .cyan, modifier := { bold := true } }
          }

          emitDynamic do
            let idx ← tabResult.activeTab.sample
            let content := match idx with
              | 0 => "Welcome to the Home tab!"
              | 1 => "Configure your settings here."
              | _ => "Help: Use arrow keys to navigate."
            pure (RNode.text content theme.bodyStyle)

      -- Tree demo
      column' (gap := 1) {} do
        titledBlock' "Tree Widget" .rounded theme do
          let fileTree := TreeNode.branch "project" #[
            TreeNode.branch "src" #[
              TreeNode.leaf "Main.lean",
              TreeNode.leaf "Types.lean"
            ],
            TreeNode.branch "tests" #[
              TreeNode.leaf "Tests.lean"
            ],
            TreeNode.leaf "lakefile.lean"
          ]

          let treeResult ← tree' fileTree {
            focusName := "file-tree"
            maxVisible := some 6
            selectedStyle := { bg := .ansi .blue, fg := .ansi .white }
            branchStyle := { fg := .ansi .yellow, modifier := { bold := true } }
            expandedIcon := "▼ "
            collapsedIcon := "▶ "
          }

          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let node ← treeResult.selectedNode.sample
              pure (RNode.text (node.getD "(none)") theme.primaryStyle)

/-! ## Data Tab Content -/

def dataContent (theme : Theme) (_events : TerminusEvents) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Menu, table, and calendar widgets." theme.captionStyle

    row' (gap := 2) {} do
      -- Menu
      column' (gap := 1) {} do
        titledBlock' "Menu Widget" .rounded theme do
          let menuSelRef ← SpiderM.liftIO (IO.mkRef "None")
          let menuItems := #[
            MenuItem'.new "File" |>.withSubmenu #[
              MenuItem'.new "New",
              MenuItem'.new "Open",
              MenuItem'.new "Save"
            ],
            MenuItem'.new "Edit" |>.withSubmenu #[
              MenuItem'.new "Cut",
              MenuItem'.new "Copy",
              MenuItem'.new "Paste"
            ],
            MenuItem'.new "Help"
          ]

          let menuResult ← menu' "demo-menu" menuItems {}
          let _ ← SpiderM.liftIO <| menuResult.onSelect.subscribe fun (_, label) =>
            menuSelRef.set label

          row' (gap := 1) {} do
            text' "Selected:" theme.captionStyle
            emitDynamic do
              let sel ← menuSelRef.get
              pure (RNode.text sel theme.bodyStyle)

      -- Table
      column' (gap := 1) {} do
        titledBlock' "Table Widget" .rounded theme do
          let columns := #[
            { header := "Name", width := .fixed 10 : TableColumn' },
            { header := "Role", width := .fixed 8 : TableColumn' }
          ]
          let rows := #[
            TableRow'.new #["Alice", "Engineer"],
            TableRow'.new #["Bob", "Designer"],
            TableRow'.new #["Carol", "Manager"]
          ]

          let tableResult ← table' "demo-table" columns rows {
            useAlternateColors := true
            maxHeight := some 4
          }

          row' (gap := 1) {} do
            text' "Row:" theme.captionStyle
            emitDynamic do
              let idx ← tableResult.selectedIndex.sample
              let display := match idx with
                | some i => toString i
                | none => "(none)"
              pure (RNode.text display theme.primaryStyle)

      -- Calendar
      column' (gap := 1) {} do
        titledBlock' "Calendar" .rounded theme do
          let calDateRef ← SpiderM.liftIO (IO.mkRef "2024-01-15")
          let today := CalendarDate.new 2024 1 15
          let calResult ← calendar' "demo-calendar" 2024 1 (some 15) {
            today := some today
          }

          let _ ← SpiderM.liftIO <| calResult.onSelect.subscribe fun date =>
            calDateRef.set s!"{date.year}-{date.month}-{date.day}"

          row' (gap := 1) {} do
            text' "Date:" theme.captionStyle
            emitDynamic do
              let date ← calDateRef.get
              pure (RNode.text date theme.primaryStyle)

    spacer' 0 1

    row' (gap := 2) {} do
      -- Scrolling widgets
      column' (gap := 1) {} do
        titledBlock' "ScrollView" .rounded theme do
          text' "Tab to focus, arrows to scroll" theme.captionStyle
          let scroll ← scrollView' { maxVisible := 4, focusName := "demo-scroll" } do
            for i in [1:9] do
              text' s!"Item {i}" theme.bodyStyle

          emitDynamic do
            let state ← scroll.scrollState.sample
            pure (RNode.text s!"Offset: {state.offsetY}" theme.captionStyle)

        titledBlock' "Scrollbars" .rounded theme do
          text' "Vertical:" theme.captionStyle
          scrollbar' 2 10 4 { length := 6 }
          spacer' 0 1
          text' "Horizontal:" theme.captionStyle
          hScrollbar' 3 12 6 12

      -- Grid widgets
      column' (gap := 1) {} do
        titledBlock' "Grid Widgets" .rounded theme do
          text' "Static grid:" theme.captionStyle
          grid' 3 2 (fun x y => do
            pure { content := s!"{x}{y}", style := { fg := .ansi .cyan } }
          ) { borderType := .rounded }

          spacer' 0 1

          text' "Cursor grid (arrows):" theme.captionStyle
          let cursor ← cursorGrid' 4 3 (fun _ _ isCursor => do
            if isCursor then
              pure { content := "[]", style := { fg := .ansi .yellow } }
            else
              pure { content := " .", style := { fg := .ansi .brightBlack } }
          ) { focusName := "cursor-grid", borderType := .rounded }

          emitDynamic do
            let (cx, cy) ← cursor.cursorPos.sample
            pure (RNode.text s!"Cursor: {cx},{cy}" theme.captionStyle)

          spacer' 0 1

          text' "Char grid:" theme.captionStyle
          let cells : Array (Array (Char × Style)) := #[
            #[('A', { fg := .ansi .red }), ('B', { fg := .ansi .green })],
            #[('C', { fg := .ansi .yellow }), ('D', { fg := .ansi .cyan })]
          ]
          charGrid' cells { borderType := .rounded }

/-! ## Charts Tab Content -/

def chartsContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Gauges, sparklines, and charts." theme.captionStyle

    row' (gap := 2) {} do
      -- Sparklines
      column' (gap := 1) {} do
        titledBlock' "Sparklines" .rounded theme do
          labeledSparkline' "CPU " #[30.0, 45.0, 60.0, 55.0, 70.0, 65.0, 80.0] {
            style := { fg := .ansi .green }
            showValue := true
          }
          labeledSparkline' "MEM " #[50.0, 52.0, 55.0, 60.0, 58.0, 62.0, 65.0] {
            style := { fg := .ansi .yellow }
            showValue := true
          }
          labeledSparkline' "NET " #[10.0, 25.0, 15.0, 40.0, 20.0, 35.0, 30.0] {
            style := { fg := .ansi .cyan }
            showValue := true
          }

      -- Bar chart
      column' (gap := 1) {} do
        titledBlock' "Bar Chart" .rounded theme do
          barChart' #[
            BarData.styled "Jan" 45 { fg := .ansi .blue },
            BarData.styled "Feb" 62 { fg := .ansi .blue },
            BarData.styled "Mar" 38 { fg := .ansi .blue },
            BarData.styled "Apr" 75 { fg := .ansi .green }
          ] {
            orientation := .vertical
            barWidth := 3
            size := 5
          }

      -- Pie chart
      column' (gap := 1) {} do
        titledBlock' "Pie Chart" .rounded theme do
          pieChart' #[
            PieSlice.styled "A" 35 { fg := .ansi .blue },
            PieSlice.styled "B" 28 { fg := .ansi .green },
            PieSlice.styled "C" 22 { fg := .ansi .yellow },
            PieSlice.styled "D" 15 { fg := .ansi .magenta }
          ] { radius := 6 }

    spacer' 0 1

    row' (gap := 2) {} do
      -- Line chart
      column' (gap := 1) {} do
        titledBlock' "Line Chart" .rounded theme do
          lineChart' #[
            DataSeries.labeled "Sales" #[10.0, 18.0, 25.0, 20.0, 30.0, 28.0],
            DataSeries.labeled "Costs" #[8.0, 12.0, 15.0, 14.0, 20.0, 18.0]
          ] {
            width := 32
            height := 8
            showLegend := true
          }

      -- Vertical gauge
      column' (gap := 1) {} do
        titledBlock' "Vertical Gauge" .rounded theme do
          row' (gap := 2) {} do
            vGauge' 0.65 { height := 8 }
            vGauge' 0.35 { height := 8, filledStyle := { fg := .ansi .yellow } }

/-! ## Feedback Tab Content -/

def feedbackContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "L: log info | E: error | W: warn | N: notify | D: dismiss | C: clear" theme.captionStyle
    text' "P: popup | O: modal | F: confirm | M: message | I: input | X: error | V: warning" theme.captionStyle

    let keyEvents ← useKeyEventW
    let focusedInput ← useFocusedInputW
    let logCountRef ← SpiderM.liftIO (IO.mkRef 0)
    let loggerResultRef ← SpiderM.liftIO (IO.mkRef (none : Option LoggerResult))
    let notifResultRef ← SpiderM.liftIO (IO.mkRef (none : Option NotificationResult))
    let dialogStatusRef ← SpiderM.liftIO (IO.mkRef "Idle")
    let inputValueRef ← SpiderM.liftIO (IO.mkRef "(none)")
    let popupResultRef ← SpiderM.liftIO (IO.mkRef (none : Option PopupResult))

    let (modalVisEvent, fireModalVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let modalVisible ← Reactive.holdDyn false modalVisEvent
    let (confirmVisEvent, fireConfirmVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let confirmVisible ← Reactive.holdDyn false confirmVisEvent
    let (messageVisEvent, fireMessageVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let messageVisible ← Reactive.holdDyn false messageVisEvent
    let (errorVisEvent, fireErrorVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let errorVisible ← Reactive.holdDyn false errorVisEvent
    let (warningVisEvent, fireWarningVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let warningVisible ← Reactive.holdDyn false warningVisEvent
    let (inputVisEvent, fireInputVisible) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
    let inputVisible ← Reactive.holdDyn false inputVisEvent

    row' (gap := 2) {} do
      -- Spinners
      column' (gap := 1) {} do
        titledBlock' "Spinners" .rounded theme do
          row' (gap := 1) {} do
            text' "Dots:" theme.captionStyle
            let _ ← animatedSpinner' (some "Processing") 100 { style := .dots }

          row' (gap := 1) {} do
            text' "ASCII:" theme.captionStyle
            let _ ← animatedSpinner' (some "Loading") 120 { style := .ascii }

          row' (gap := 1) {} do
            text' "Blocks:" theme.captionStyle
            let _ ← animatedSpinner' none 150 { style := .blocks }

          row' (gap := 1) {} do
            text' "Arc:" theme.captionStyle
            let _ ← animatedSpinner' none 80 { style := .arc }

      -- Logger
      column' (gap := 1) {} do
        titledBlock' "Logger" .rounded theme do
          let logResult ← logger' {
            maxLines := 6
            showLevel := true
            showTimestamp := false
          }
          SpiderM.liftIO (loggerResultRef.set (some logResult))

      -- Notifications
      column' (gap := 1) {} do
        titledBlock' "Notifications" .rounded theme do
          let notifResult ← notifications' {
            durationMs := 0
            maxVisible := 4
          }
          SpiderM.liftIO (notifResultRef.set (some notifResult))

    spacer' 0 1

    row' (gap := 2) {} do
      -- Dialogs
      column' (gap := 1) {} do
        titledBlock' "Dialogs" .rounded theme do
          text' "O: modal | F: confirm | M: message" theme.captionStyle
          text' "I: input | X: error | V: warning | Esc: close modal" theme.captionStyle

          let _ ← modalWhen' modalVisible "Modal" theme {} do
            text' "This is a modal dialog." theme.bodyStyle
            text' "Press Esc to close." theme.captionStyle

          let confirmResult ← confirmDialog' "Proceed with action?" confirmVisible theme
          let messageDismiss ← messageDialog' "Operation completed successfully!" messageVisible theme
          let errorDismiss ← errorDialog' "Something went wrong." errorVisible theme
          let warningDismiss ← warningDialog' "Please review the warning." warningVisible theme
          let inputResult ← inputDialog' "Enter your name:" inputVisible theme "Your name"

          let _ ← SpiderM.liftIO <| confirmResult.confirmed.subscribe fun _ => do
            dialogStatusRef.set "Confirmed"
            fireConfirmVisible false
          let _ ← SpiderM.liftIO <| confirmResult.cancelled.subscribe fun _ => do
            dialogStatusRef.set "Cancelled"
            fireConfirmVisible false

          let _ ← SpiderM.liftIO <| messageDismiss.subscribe fun _ => do
            dialogStatusRef.set "Message dismissed"
            fireMessageVisible false
          let _ ← SpiderM.liftIO <| errorDismiss.subscribe fun _ => do
            dialogStatusRef.set "Error dismissed"
            fireErrorVisible false
          let _ ← SpiderM.liftIO <| warningDismiss.subscribe fun _ => do
            dialogStatusRef.set "Warning dismissed"
            fireWarningVisible false

          let _ ← SpiderM.liftIO <| inputResult.submitted.subscribe fun value => do
            inputValueRef.set value
            dialogStatusRef.set "Input submitted"
            fireInputVisible false
          let _ ← SpiderM.liftIO <| inputResult.cancelled.subscribe fun _ => do
            dialogStatusRef.set "Input cancelled"
            fireInputVisible false

          spacer' 0 1

          emitDynamic do
            let status ← dialogStatusRef.get
            let inputValue ← inputValueRef.get
            pure (RNode.text s!"Last: {status} | Input: {inputValue}" theme.captionStyle)

      -- Popup
      column' (gap := 1) {} do
        titledBlock' "Popup" .rounded theme do
          text' "P: toggle popup" theme.captionStyle
          let popupResult ← popup' "demo-popup" { title := some "Popup" } do
            text' "This is a popup panel." theme.bodyStyle
            text' "Press P to toggle visibility." theme.captionStyle
          SpiderM.liftIO (popupResultRef.set (some popupResult))
          emitDynamic do
            let visible ← popupResult.visible.sample
            let label := if visible then "Visible" else "Hidden"
            pure (RNode.text s!"Status: {label}" theme.captionStyle)

    -- Key handler for logging and dialogs
    let _unsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
      let focus ← focusedInput.sample
      if focus == some "input-dialog-field" then
        pure ()
      else
        match kd.event.code with
        | .char 'l' | .char 'L' =>
          let count ← logCountRef.get
          logCountRef.set (count + 1)
          match (← loggerResultRef.get) with
          | some logger => logger.log .info s!"Info message #{count + 1}"
          | none => pure ()
        | .char 'e' | .char 'E' =>
          let count ← logCountRef.get
          logCountRef.set (count + 1)
          match (← loggerResultRef.get) with
          | some logger => logger.log .error s!"Error message #{count + 1}"
          | none => pure ()
        | .char 'w' | .char 'W' =>
          let count ← logCountRef.get
          logCountRef.set (count + 1)
          match (← loggerResultRef.get) with
          | some logger => logger.log .warn s!"Warning #{count + 1}"
          | none => pure ()
        | .char 'n' | .char 'N' =>
          let count ← logCountRef.get
          logCountRef.set (count + 1)
          match (← notifResultRef.get) with
          | some notif => notif.«show» .success s!"Notification #{count + 1}!"
          | none => pure ()
        | .char 'd' | .char 'D' =>
          match (← notifResultRef.get) with
          | some notif => notif.dismiss
          | none => pure ()
        | .char 'c' | .char 'C' =>
          match (← loggerResultRef.get) with
          | some logger => logger.clear
          | none => pure ()
          match (← notifResultRef.get) with
          | some notif => notif.dismissAll
          | none => pure ()
        | .char 'p' | .char 'P' =>
          match (← popupResultRef.get) with
          | some popup => popup.toggle
          | none => pure ()
        | .char 'o' | .char 'O' =>
          dialogStatusRef.set "Modal opened"
          fireModalVisible true
        | .char 'f' | .char 'F' =>
          dialogStatusRef.set "Confirm opened"
          fireConfirmVisible true
        | .char 'm' | .char 'M' =>
          dialogStatusRef.set "Message opened"
          fireMessageVisible true
        | .char 'i' | .char 'I' =>
          dialogStatusRef.set "Input opened"
          fireInputVisible true
        | .char 'x' | .char 'X' =>
          dialogStatusRef.set "Error opened"
          fireErrorVisible true
        | .char 'v' | .char 'V' =>
          dialogStatusRef.set "Warning opened"
          fireWarningVisible true
        | .escape =>
          let isVisible ← modalVisible.sample
          if isVisible then
            dialogStatusRef.set "Modal closed"
            fireModalVisible false
          else
            pure ()
        | _ => pure ()

/-! ## Media Tab Content -/

private def nibblePngPath : System.FilePath := "examples/nibble.png"

def mediaContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Canvas drawing, big text, and images. Press N to switch." theme.captionStyle

    -- Track media mode
    let keyEvents ← useKeyEventW
    let modeEvent ← Event.filterM (fun kd => kd.event.code == .char 'n' || kd.event.code == .char 'N') keyEvents
    let voidModeEvent ← Event.voidM modeEvent
    let modeDyn ← Reactive.foldDyn (fun _ mode => (mode + 1) % 3) 0 voidModeEvent

    let isCanvas ← Dynamic.map' modeDyn (· == 0)
    let isBigText ← Dynamic.map' modeDyn (· == 1)
    let isImage ← Dynamic.map' modeDyn (· == 2)

    emitDynamic do
      let mode ← modeDyn.sample
      let label := match mode with
        | 0 => "Canvas"
        | 1 => "BigText"
        | _ => "Image"
      pure (RNode.text s!"Current: {label}" theme.primaryStyle)

    spacer' 0 1

    -- Canvas
    when' isCanvas do
      titledBlock' "Canvas Drawing" .rounded theme do
        let grid := BrailleGrid.new 25 6
          |>.drawRect 2 2 15 15 { fg := .ansi .cyan }
          |>.drawLine 0 0 48 22 { fg := .ansi .green }
          |>.drawCircle 25 11 8 { fg := .ansi .yellow }
        staticCanvas' grid
        text' "Shapes: rect, line, circle" theme.captionStyle

    -- BigText
    when' isBigText do
      titledBlock' "BigText Rendering" .rounded theme do
        bigText' "LEAN" { font := .block, style := { fg := .ansi .cyan } }
        spacer' 0 1
        bigText' "TERMINUS" { font := .small, style := { fg := .ansi .green } }

    -- Image
    when' isImage do
      titledBlock' "Image Widget" .rounded theme do
        image' nibblePngPath {
          width := 30
          height := 10
          protocol := .iterm2
          altText := "[nibble.png - requires iTerm2/WezTerm]"
        }
        text' "Supports iTerm2, WezTerm, kitty" theme.captionStyle

/-! ## Async Tab Content -/

def simulateSlowLoad : IO String := do
  IO.sleep 1500
  pure "Data loaded successfully!"

def asyncContent (theme : Theme) : WidgetM Unit := do
  column' (gap := 1) {} do
    text' "Space: load data | S: stream chunks" theme.captionStyle

    let _ ← horizontalSplit' 50 {}
      -- Left: Async load
      (do
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
      )
      -- Right: Streaming
      (do
        titledBlock' "Streaming Demo" .rounded theme do
          text' "Press S to stream:" theme.bodyStyle

          let (_chunkEvent, _fireChunk) ← Reactive.newTriggerEvent (t := Spider) (a := String)
          let (streamingEvent, fireStreaming) ← Reactive.newTriggerEvent (t := Spider) (a := Bool)
          let (chunksEvent, fireChunks) ← Reactive.newTriggerEvent (t := Spider) (a := Array String)

          let chunksRef ← SpiderM.liftIO (IO.mkRef #[])
          let env ← SpiderM.getEnv

          let keyEvent ← useKeyEventW
          let _unsub ← SpiderM.liftIO <| keyEvent.subscribe fun kd => do
            if kd.event.code == .char 's' || kd.event.code == .char 'S' then
              chunksRef.set #[]
              env.withFrame do
                fireStreaming true
                fireChunks #[]
              let _ ← IO.asTask (prio := .dedicated) do
                for i in [1:5] do
                  IO.sleep 400
                  let chunk := s!"Chunk {i}"
                  let newChunks ← chunksRef.modifyGet fun cs => (cs.push chunk, cs.push chunk)
                  env.withFrame (fireChunks newChunks)
                env.withFrame (fireStreaming false)

          let streamingDyn ← Reactive.holdDyn false streamingEvent
          let chunksDyn ← Reactive.holdDyn #[] chunksEvent

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
      )

/-! ## Main Application -/

def app : ReactiveTermM ReactiveAppState := do
  let theme := Theme.dark
  let events ← getEvents

  -- Build the UI
  let (_, render) ← runWidget do
    let _ ← dockBottom' (footerHeight := 1)
      (content := do
        column' (gap := 1) (style := {}) do
          -- Header
          text' "═══ Terminus Widget Showcase ═══" theme.heading1Style

          -- Main tabs for navigation
          let tabResult ← tabs' DemoTab.allTabs 0 {
            focusName := "main-tabs"
            activeStyle := { fg := .ansi .cyan, modifier := { bold := true } }
            globalKeys := false
          }

          -- Help text
          row' (gap := 2) {} do
            text' "Ctrl+C: quit" theme.captionStyle
            text' "←/→: tabs" theme.captionStyle
            text' "Tab/Shift+Tab: cycle focus" theme.captionStyle

          spacer' 0 1

          -- Handle Tab key for automatic focus cycling
          let keyEvents ← useKeyEventW
          let _tabUnsub ← SpiderM.liftIO <| keyEvents.subscribe fun kd => do
            match kd.event.code with
            | .tab =>
              if kd.event.modifiers.shift then
                events.registry.focusPrev
              else
                events.registry.focusNext
            | _ => pure ()

          -- Content area based on selected tab (rebuilds on change)
          let _ ← dynWidget tabResult.activeTab fun idx => do
            match idx with
            | 0 => basicsContent theme
            | 1 => inputContent theme events
            | 2 => navigationContent theme events
            | 3 => dataContent theme events
            | 4 => chartsContent theme
            | 5 => feedbackContent theme
            | 6 => mediaContent theme
            | 7 => asyncContent theme
            | _ => basicsContent theme
      )
      (footer := do
        let focusedInput ← useFocusedInputW
        emitDynamic do
          let focused ← focusedInput.sample
          let focusName := focused.getD "(none)"
          pure (RNode.text s!"Focused: {focusName}" theme.captionStyle)
      )

  -- Focus first input widget after setup
  SpiderM.liftIO <| events.registry.focusNext

  pure { render }

def runDemo : IO Unit := runReactiveApp app { debug := true }

end Terminus.Reactive.Demos.UnifiedDemo
