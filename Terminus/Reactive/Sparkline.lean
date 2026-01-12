/-
  Terminus Reactive - Sparkline Widget
  Inline mini chart using vertical bar characters.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Sparkline Characters -/

/-- Unicode block characters for sparkline bars (8 levels). -/
private def sparklineChars : Array Char := #['▁', '▂', '▃', '▄', '▅', '▆', '▇', '█']

/-! ## Sparkline Configuration -/

/-- Configuration for sparkline widget. -/
structure SparklineConfig where
  /-- Style for the sparkline bars. -/
  style : Style := {}
  /-- Maximum width (truncates data if exceeded). -/
  maxWidth : Option Nat := none
  /-- Minimum value for scaling. -/
  minValue : Option Float := none
  /-- Maximum value for scaling. -/
  maxValue : Option Float := none
  /-- Show baseline character for zero values. -/
  showBaseline : Bool := true
  deriving Repr, Inhabited

/-! ## Helper Functions -/

/-- Compute the maximum value from data array. -/
private def computeMax (data : Array Float) (override : Option Float) : Float :=
  match override with
  | some m => m
  | none => data.foldl (fun a b => if b > a then b else a) 0.0

/-- Compute the minimum value from data array. -/
private def computeMin (data : Array Float) (override : Option Float) : Float :=
  match override with
  | some m => m
  | none => data.foldl (fun a b => if b < a then b else a) 1.0e38

/-- Map a value to a bar character index (0-7). -/
private def valueToBarIndex (value minVal maxVal : Float) : Nat :=
  let range := maxVal - minVal
  if range <= 0.0 then 0
  else
    let normalized := (value - minVal) / range
    let index := (normalized * 7.0).toUInt32.toNat
    Nat.min index 7

/-! ## Sparkline Widget -/

/-- Create a sparkline widget from float data.

    Example:
    ```
    sparkline' #[1.0, 3.0, 2.0, 5.0, 4.0, 6.0, 3.0] {}
    ```
-/
def sparkline' (data : Array Float) (config : SparklineConfig := {}) : WidgetM Unit := do
  if data.isEmpty then
    emitStatic RNode.empty
  else
    let displayData := match config.maxWidth with
      | some w => data.toList.take w |>.toArray
      | none => data

    let minVal := computeMin displayData config.minValue
    let maxVal := computeMax displayData config.maxValue

    let chars := displayData.map fun v =>
      let idx := valueToBarIndex v minVal maxVal
      sparklineChars.getD idx '▁'

    emitStatic (RNode.text (String.ofList chars.toList) config.style)

/-- Create a sparkline from integer data. -/
def sparklineInt' (data : Array Int) (config : SparklineConfig := {}) : WidgetM Unit :=
  sparkline' (data.map Float.ofInt) config

/-- Create a sparkline from natural number data. -/
def sparklineNat' (data : Array Nat) (config : SparklineConfig := {}) : WidgetM Unit :=
  sparkline' (data.map fun n => Float.ofNat n) config

/-- Create a dynamic sparkline widget.

    Example:
    ```
    let dataDyn ← someDataSource
    dynSparkline' dataDyn { style := { fg := .ansi .cyan } }
    ```
-/
def dynSparkline' (data : Reactive.Dynamic Spider (Array Float)) (config : SparklineConfig := {})
    : WidgetM Unit := do
  let node ← data.map' fun arr =>
    if arr.isEmpty then
      RNode.empty
    else
      let displayData := match config.maxWidth with
        | some w => arr.toList.take w |>.toArray
        | none => arr

      let minVal := computeMin displayData config.minValue
      let maxVal := computeMax displayData config.maxValue

      let chars := displayData.map fun v =>
        let idx := valueToBarIndex v minVal maxVal
        sparklineChars.getD idx '▁'

      RNode.text (String.ofList chars.toList) config.style
  emit node

/-! ## Labeled Sparkline -/

/-- Configuration for labeled sparkline. -/
structure LabeledSparklineConfig extends SparklineConfig where
  /-- Label to show before the sparkline. -/
  label : String := ""
  /-- Style for the label. -/
  labelStyle : Style := {}
  /-- Show current (last) value after sparkline. -/
  showValue : Bool := false
  /-- Style for the value. -/
  valueStyle : Style := {}
  deriving Repr, Inhabited

/-- Create a labeled sparkline with optional value display.

    Example:
    ```
    labeledSparkline' "CPU" #[30, 45, 60, 55, 70] {
      showValue := true
      style := { fg := .ansi .green }
    }
    ```
-/
def labeledSparkline' (label : String) (data : Array Float) (config : LabeledSparklineConfig := {})
    : WidgetM Unit := do
  if data.isEmpty then
    emitStatic (RNode.text label config.labelStyle)
  else
    let displayData := match config.maxWidth with
      | some w => data.toList.take w |>.toArray
      | none => data

    let minVal := computeMin displayData config.minValue
    let maxVal := computeMax displayData config.maxValue

    let chars := displayData.map fun v =>
      let idx := valueToBarIndex v minVal maxVal
      sparklineChars.getD idx '▁'

    let mut nodes : Array RNode := #[]

    if !label.isEmpty then
      nodes := nodes.push (RNode.text (label ++ " ") config.labelStyle)

    nodes := nodes.push (RNode.text (String.ofList chars.toList) config.style)

    if config.showValue then
      if let some lastVal := data.back? then
        nodes := nodes.push (RNode.text s!" {lastVal.toUInt32}" config.valueStyle)

    emitStatic (RNode.row 0 {} nodes)

/-- Create a dynamic labeled sparkline. -/
def dynLabeledSparkline' (label : String) (data : Reactive.Dynamic Spider (Array Float))
    (config : LabeledSparklineConfig := {}) : WidgetM Unit := do
  let node ← data.map' fun arr =>
    Id.run do
      if arr.isEmpty then
        return RNode.text label config.labelStyle
      else
        let displayData := match config.maxWidth with
          | some w => arr.toList.take w |>.toArray
          | none => arr

        let minVal := computeMin displayData config.minValue
        let maxVal := computeMax displayData config.maxValue

        let chars := displayData.map fun v =>
          let idx := valueToBarIndex v minVal maxVal
          sparklineChars.getD idx '▁'

        let mut nodes : Array RNode := #[]

        if !label.isEmpty then
          nodes := nodes.push (RNode.text (label ++ " ") config.labelStyle)

        nodes := nodes.push (RNode.text (String.ofList chars.toList) config.style)

        if config.showValue then
          if let some lastVal := arr.back? then
            nodes := nodes.push (RNode.text s!" {lastVal.toUInt32}" config.valueStyle)

        return RNode.row 0 {} nodes
  emit node

end Terminus.Reactive
