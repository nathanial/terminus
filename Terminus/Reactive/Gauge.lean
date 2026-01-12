/-
  Terminus Reactive - Gauge Widgets
  Progress bar visualizations with value display.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Gauge Configuration -/

/-- Configuration for gauge (progress bar) widget. -/
structure GaugeConfig where
  /-- Width of the gauge bar. -/
  width : Nat := 20
  /-- Character for filled portion. -/
  filledChar : Char := '█'
  /-- Character for unfilled portion. -/
  unfilledChar : Char := '░'
  /-- Style for filled portion. -/
  filledStyle : Style := { fg := .ansi .green }
  /-- Style for unfilled portion. -/
  unfilledStyle : Style := { fg := .ansi .brightBlack }
  /-- Show percentage label. -/
  showPercent : Bool := true
  /-- Style for percentage label. -/
  percentStyle : Style := {}
  /-- Optional label text. -/
  label : Option String := none
  /-- Style for label. -/
  labelStyle : Style := {}
  /-- Minimum value (for normalization). -/
  minValue : Float := 0.0
  /-- Maximum value (for normalization). -/
  maxValue : Float := 1.0
  deriving Repr, Inhabited

/-! ## Helper Functions -/

/-- Clamp a value to valid range, handling NaN and Inf. -/
private def clampValue (value : Float) (minVal maxVal : Float) : Float :=
  if value.isNaN || value == 1.0e38 || value == -1.0e38 then 0.0
  else max minVal (min maxVal value)

/-- Normalize value to 0.0-1.0 range. -/
private def normalizeValue (value : Float) (minVal maxVal : Float) : Float :=
  let clamped := clampValue value minVal maxVal
  let range := maxVal - minVal
  if range <= 0.0 then 0.0
  else (clamped - minVal) / range

/-- Convert ratio to percentage (0-100). -/
private def ratioToPercent (ratio : Float) : Nat :=
  (ratio * 100.0).toUInt32.toNat

/-! ## Gauge Widget -/

/-- Create a gauge (progress bar) widget.

    Example:
    ```
    gauge' 0.75 { width := 30, showPercent := true }
    ```
-/
def gauge' (value : Float) (config : GaugeConfig := {}) : WidgetM Unit := do
  let ratio := normalizeValue value config.minValue config.maxValue
  let filledWidth := (ratio * config.width.toFloat).toUInt32.toNat
  let unfilledWidth := config.width - filledWidth

  let filledStr := String.ofList (List.replicate filledWidth config.filledChar)
  let unfilledStr := String.ofList (List.replicate unfilledWidth config.unfilledChar)

  let mut nodes : Array RNode := #[]

  if let some label := config.label then
    nodes := nodes.push (RNode.text (label ++ " ") config.labelStyle)

  if filledWidth > 0 then
    nodes := nodes.push (RNode.text filledStr config.filledStyle)

  if unfilledWidth > 0 then
    nodes := nodes.push (RNode.text unfilledStr config.unfilledStyle)

  if config.showPercent then
    let pct := ratioToPercent ratio
    nodes := nodes.push (RNode.text s!" {pct}%" config.percentStyle)

  emitStatic (RNode.row 0 {} nodes)

/-- Create a dynamic gauge widget.

    Example:
    ```
    let progress ← someProgressDynamic
    dynGauge' progress { width := 40 }
    ```
-/
def dynGauge' (value : Reactive.Dynamic Spider Float) (config : GaugeConfig := {})
    : WidgetM Unit := do
  let node ← value.map' fun v =>
    Id.run do
      let ratio := normalizeValue v config.minValue config.maxValue
      let filledWidth := (ratio * config.width.toFloat).toUInt32.toNat
      let unfilledWidth := config.width - filledWidth

      let filledStr := String.ofList (List.replicate filledWidth config.filledChar)
      let unfilledStr := String.ofList (List.replicate unfilledWidth config.unfilledChar)

      let mut nodes : Array RNode := #[]

      if let some label := config.label then
        nodes := nodes.push (RNode.text (label ++ " ") config.labelStyle)

      if filledWidth > 0 then
        nodes := nodes.push (RNode.text filledStr config.filledStyle)

      if unfilledWidth > 0 then
        nodes := nodes.push (RNode.text unfilledStr config.unfilledStyle)

      if config.showPercent then
        let pct := ratioToPercent ratio
        nodes := nodes.push (RNode.text s!" {pct}%" config.percentStyle)

      return RNode.row 0 {} nodes
  emit node

/-! ## LineGauge Configuration -/

/-- Configuration for line gauge (thin progress bar). -/
structure LineGaugeConfig where
  /-- Width of the gauge bar. -/
  width : Nat := 20
  /-- Character for filled portion. -/
  filledChar : Char := '━'
  /-- Character for unfilled portion. -/
  unfilledChar : Char := '─'
  /-- Style for filled portion. -/
  filledStyle : Style := { fg := .ansi .green }
  /-- Style for unfilled portion. -/
  unfilledStyle : Style := { fg := .ansi .brightBlack }
  /-- Show percentage label. -/
  showPercent : Bool := false
  /-- Style for percentage label. -/
  percentStyle : Style := {}
  /-- Optional label text (appears at start). -/
  label : Option String := none
  /-- Style for label. -/
  labelStyle : Style := {}
  /-- Minimum value. -/
  minValue : Float := 0.0
  /-- Maximum value. -/
  maxValue : Float := 1.0
  deriving Repr, Inhabited

/-! ## LineGauge Widget -/

/-- Create a line gauge (thin progress bar) widget.

    Example:
    ```
    lineGauge' 0.5 { width := 30, label := some "Progress" }
    ```
-/
def lineGauge' (value : Float) (config : LineGaugeConfig := {}) : WidgetM Unit := do
  let ratio := normalizeValue value config.minValue config.maxValue
  let filledWidth := (ratio * config.width.toFloat).toUInt32.toNat
  let unfilledWidth := config.width - filledWidth

  let filledStr := String.ofList (List.replicate filledWidth config.filledChar)
  let unfilledStr := String.ofList (List.replicate unfilledWidth config.unfilledChar)

  let mut nodes : Array RNode := #[]

  if let some label := config.label then
    nodes := nodes.push (RNode.text (label ++ " ") config.labelStyle)

  if filledWidth > 0 then
    nodes := nodes.push (RNode.text filledStr config.filledStyle)

  if unfilledWidth > 0 then
    nodes := nodes.push (RNode.text unfilledStr config.unfilledStyle)

  if config.showPercent then
    let pct := ratioToPercent ratio
    nodes := nodes.push (RNode.text s!" {pct}%" config.percentStyle)

  emitStatic (RNode.row 0 {} nodes)

/-- Create a dynamic line gauge widget.

    Example:
    ```
    let progress ← someProgressDynamic
    dynLineGauge' progress { width := 40 }
    ```
-/
def dynLineGauge' (value : Reactive.Dynamic Spider Float) (config : LineGaugeConfig := {})
    : WidgetM Unit := do
  let node ← value.map' fun v =>
    Id.run do
      let ratio := normalizeValue v config.minValue config.maxValue
      let filledWidth := (ratio * config.width.toFloat).toUInt32.toNat
      let unfilledWidth := config.width - filledWidth

      let filledStr := String.ofList (List.replicate filledWidth config.filledChar)
      let unfilledStr := String.ofList (List.replicate unfilledWidth config.unfilledChar)

      let mut nodes : Array RNode := #[]

      if let some label := config.label then
        nodes := nodes.push (RNode.text (label ++ " ") config.labelStyle)

      if filledWidth > 0 then
        nodes := nodes.push (RNode.text filledStr config.filledStyle)

      if unfilledWidth > 0 then
        nodes := nodes.push (RNode.text unfilledStr config.unfilledStyle)

      if config.showPercent then
        let pct := ratioToPercent ratio
        nodes := nodes.push (RNode.text s!" {pct}%" config.percentStyle)

      return RNode.row 0 {} nodes
  emit node

/-! ## Vertical Gauge -/

/-- Configuration for vertical gauge. -/
structure VGaugeConfig where
  /-- Height of the gauge. -/
  height : Nat := 10
  /-- Character for filled portion. -/
  filledChar : Char := '█'
  /-- Character for unfilled portion. -/
  unfilledChar : Char := '░'
  /-- Style for filled portion. -/
  filledStyle : Style := { fg := .ansi .green }
  /-- Style for unfilled portion. -/
  unfilledStyle : Style := { fg := .ansi .brightBlack }
  /-- Minimum value. -/
  minValue : Float := 0.0
  /-- Maximum value. -/
  maxValue : Float := 1.0
  deriving Repr, Inhabited

/-- Create a vertical gauge widget.

    Example:
    ```
    vGauge' 0.6 { height := 8 }
    ```
-/
def vGauge' (value : Float) (config : VGaugeConfig := {}) : WidgetM Unit := do
  let ratio := normalizeValue value config.minValue config.maxValue
  let filledHeight := (ratio * config.height.toFloat).toUInt32.toNat
  let unfilledHeight := config.height - filledHeight

  let mut rows : Array RNode := #[]

  for _ in [:unfilledHeight] do
    rows := rows.push (RNode.text (String.singleton config.unfilledChar) config.unfilledStyle)

  for _ in [:filledHeight] do
    rows := rows.push (RNode.text (String.singleton config.filledChar) config.filledStyle)

  emitStatic (RNode.column 0 {} rows)

/-- Create a dynamic vertical gauge widget. -/
def dynVGauge' (value : Reactive.Dynamic Spider Float) (config : VGaugeConfig := {})
    : WidgetM Unit := do
  let node ← value.map' fun v =>
    Id.run do
      let ratio := normalizeValue v config.minValue config.maxValue
      let filledHeight := (ratio * config.height.toFloat).toUInt32.toNat
      let unfilledHeight := config.height - filledHeight

      let mut rows : Array RNode := #[]

      for _ in [:unfilledHeight] do
        rows := rows.push (RNode.text (String.singleton config.unfilledChar) config.unfilledStyle)

      for _ in [:filledHeight] do
        rows := rows.push (RNode.text (String.singleton config.filledChar) config.filledStyle)

      return RNode.column 0 {} rows
  emit node

end Terminus.Reactive
