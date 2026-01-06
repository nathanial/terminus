-- Terminus.Widgets.PieChart: Circular data visualization (cell or Braille resolution)

import Terminus.Widgets.Widget
import Terminus.Widgets.Block
import Terminus.Widgets.Canvas

namespace Terminus

/-- A single slice in a pie chart. -/
structure PieSlice where
  label : String := ""
  value : Float := 0.0
  style : Style := {}
  deriving Repr, Inhabited

namespace PieSlice

def new (label : String) (value : Float) : PieSlice := { label, value }
def withStyle (s : PieSlice) (st : Style) : PieSlice := { s with style := st }

end PieSlice

inductive PieChartResolution where
  | cell
  | braille
  deriving Repr, BEq, Inhabited

inductive LegendPosition where
  | right
  | bottom
  deriving Repr, BEq, Inhabited

/-- Pie chart widget. -/
structure PieChart where
  data : List PieSlice := []
  resolution : PieChartResolution := .braille
  startAngle : Float := (-90.0)  -- degrees, 0° points right, positive is CCW
  donutRatio : Float := 0.0      -- 0.0 = pie, 0.5 = donut
  showLegend : Bool := true
  legendPosition : Option LegendPosition := none
  showPercent : Bool := true
  marker : Char := '■'
  labelStyle : Style := {}
  percentStyle : Style := Style.dim
  block : Option Block := none
  deriving Repr, Inhabited

namespace PieChart

def new (data : List PieSlice) : PieChart := { data }

def withData (p : PieChart) (data : List PieSlice) : PieChart := { p with data := data }
def withResolution (p : PieChart) (r : PieChartResolution) : PieChart := { p with resolution := r }
def withStartAngle (p : PieChart) (deg : Float) : PieChart := { p with startAngle := deg }
def withDonutRatio (p : PieChart) (r : Float) : PieChart := { p with donutRatio := max 0.0 (min 0.9 r) }
def withShowLegend (p : PieChart) (b : Bool := true) : PieChart := { p with showLegend := b }
def withLegendPosition (p : PieChart) (pos : LegendPosition) : PieChart := { p with legendPosition := some pos }
def withShowPercent (p : PieChart) (b : Bool := true) : PieChart := { p with showPercent := b }
def withMarker (p : PieChart) (c : Char) : PieChart := { p with marker := c }
def withLabelStyle (p : PieChart) (s : Style) : PieChart := { p with labelStyle := s }
def withPercentStyle (p : PieChart) (s : Style) : PieChart := { p with percentStyle := s }
def withBlock (p : PieChart) (b : Block) : PieChart := { p with block := some b }

private def degToRad (deg : Float) : Float := deg * (3.141592653589793 / 180.0)
private def tau : Float := 2.0 * 3.141592653589793

private def sanitizeData (data : List PieSlice) : List PieSlice :=
  data.filter fun s => s.value > 0.0

private def totalValue (data : List PieSlice) : Float :=
  data.foldl (fun acc s => acc + s.value) 0.0

private def percentStr (pct : Nat) : String := s!"{pct}%"

private def percentOf (part total : Float) : Nat :=
  if total <= 0.0 then 0
  else
    let p := (part / total) * 100.0
    (max 0.0 (min 100.0 p)).toUInt32.toNat

private def legendLineLen (p : PieChart) (slice : PieSlice) (total : Float) : Nat :=
  let pct := if p.showPercent then percentStr (percentOf slice.value total) else ""
  let gap := if pct.isEmpty then 0 else 1
  2 + slice.label.length + gap + pct.length

private def computeLegendWidth (p : PieChart) (data : List PieSlice) (total : Float) : Nat :=
  data.foldl (fun acc s => Nat.max acc (legendLineLen p s total)) 0

private def splitAreas (p : PieChart) (area : Rect) (data : List PieSlice) (total : Float) : Rect × Option Rect :=
  if !p.showLegend || area.isEmpty || data.isEmpty then
    (area, none)
  else
    let desiredPos := p.legendPosition.getD (if area.width >= 30 then .right else .bottom)
    match desiredPos with
    | .right =>
      let legendW := Nat.min (area.width / 2) (Nat.max 10 (computeLegendWidth p data total + 2))
      if legendW + 2 >= area.width then
        (area, none)
      else
        let chartW := area.width - legendW - 1
        let chart := { area with width := chartW }
        let legend := { x := area.x + chartW + 1, y := area.y, width := legendW, height := area.height }
        (chart, some legend)
    | .bottom =>
      let legendH := Nat.min data.length (Nat.min 8 (area.height / 3))
      if legendH + 2 >= area.height then
        (area, none)
      else
        let chartH := area.height - legendH - 1
        let chart := { area with height := chartH }
        let legend := { x := area.x, y := area.y + chartH + 1, width := area.width, height := legendH }
        (chart, some legend)

private def chooseSliceIdx (angles : Array Float) (a : Float) : Nat :=
  -- angles are cumulative endpoints in [0, tau], strictly increasing, last ~= tau
  Id.run do
    if angles.isEmpty then
      return 0
    let mut idx : Nat := angles.size - 1
    for i in [:angles.size] do
      if a < angles.getD i 0.0 then
        idx := i
        break
    idx

private def cumulativeAngles (data : List PieSlice) (total : Float) : Array Float :=
  Id.run do
    let mut acc : Float := 0.0
    let mut ends : Array Float := #[]
    for s in data do
      let frac := if total <= 0.0 then 0.0 else s.value / total
      acc := acc + frac * tau
      ends := ends.push acc
    ends

private def renderLegend (p : PieChart) (data : List PieSlice) (total : Float) (area : Rect) (buf : Buffer) : Buffer :=
  Id.run do
    if area.isEmpty then return buf
    let mut result := buf
    let mut row := area.y
    for slice in data do
      if row >= area.y + area.height then break
      let markerX := area.x
      result := result.setStyled markerX row p.marker slice.style
      let labelX := markerX + 2
      let labelW := if area.width > 2 then area.width - 2 else 0
      result := result.writeStringBounded labelX row labelW slice.label (Style.merge p.labelStyle slice.style)
      if p.showPercent then
        let pct := percentStr (percentOf slice.value total)
        let pctX := area.x + area.width - pct.length
        if pctX > labelX && pctX < area.x + area.width then
          result := result.writeStringBounded pctX row pct.length pct p.percentStyle
      row := row + 1
    result

private def renderPieCell (p : PieChart) (data : List PieSlice) (angles : Array Float) (startRad : Float)
    (area : Rect) (buf : Buffer) : Buffer := Id.run do
  if area.isEmpty then return buf
  let mut result := buf

  let cx := (area.width.toFloat - 1.0) / 2.0
  let cy := (area.height.toFloat - 1.0) / 2.0
  let r := if cx < cy then cx else cy
  let r2 := r * r
  let innerR := r * p.donutRatio
  let innerR2 := innerR * innerR

  for dy in [:area.height] do
    for dx in [:area.width] do
      let fx := dx.toFloat - cx
      let fy := cy - dy.toFloat
      let d2 := fx * fx + fy * fy
      if d2 <= r2 && d2 >= innerR2 then
        let raw := Float.atan2 fy fx
        let ang := if raw < 0.0 then raw + tau else raw
        let rel := if ang - startRad < 0.0 then ang - startRad + tau else ang - startRad
        let idx := chooseSliceIdx angles rel
        let slice := data.getD idx {}
        result := result.setStyled (area.x + dx) (area.y + dy) '█' slice.style
  result

private def renderPieBraille (p : PieChart) (data : List PieSlice) (angles : Array Float) (startRad : Float)
    (area : Rect) (buf : Buffer) : Buffer := Id.run do
  if area.isEmpty then return buf
  if area.width == 0 || area.height == 0 then return buf

  let grid := BrailleGrid.new area.width area.height
  let pw := grid.pixelWidth
  let ph := grid.pixelHeight
  let cx := (pw.toFloat - 1.0) / 2.0
  let cy := (ph.toFloat - 1.0) / 2.0
  let r := if cx < cy then cx else cy
  let r2 := r * r
  let innerR := r * p.donutRatio
  let innerR2 := innerR * innerR

  let mut g := grid
  for py in [:ph] do
    for px in [:pw] do
      let fx := px.toFloat - cx
      let fy := cy - py.toFloat
      let d2 := fx * fx + fy * fy
      if d2 <= r2 && d2 >= innerR2 then
        let raw := Float.atan2 fy fx
        let ang := if raw < 0.0 then raw + tau else raw
        let rel := if ang - startRad < 0.0 then ang - startRad + tau else ang - startRad
        let idx := chooseSliceIdx angles rel
        let slice := data.getD idx {}
        g := g.setPixel px py slice.style

  let mut result := buf
  for cellY in [:area.height] do
    for cellX in [:area.width] do
      let (ch, st) := g.getCell cellX cellY
      if ch != '⠀' then
        result := result.setStyled (area.x + cellX) (area.y + cellY) ch st
  result

end PieChart

instance : Widget PieChart where
  render p area buf := Id.run do
    let mut result := match p.block with
      | some block => Widget.render block area buf
      | none => buf

    let contentArea := match p.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty then return result

    let data := PieChart.sanitizeData p.data
    if data.isEmpty then return result

    let total := PieChart.totalValue data
    if total <= 0.0 then return result

    let (chartArea, legendArea?) := PieChart.splitAreas p contentArea data total
    let startRad := PieChart.degToRad p.startAngle
    let startRad := if startRad < 0.0 then startRad + PieChart.tau else startRad
    let angles := PieChart.cumulativeAngles data total

    match p.resolution with
    | .cell =>
      result := PieChart.renderPieCell p data angles startRad chartArea result
    | .braille =>
      result := PieChart.renderPieBraille p data angles startRad chartArea result

    match legendArea? with
    | some legendArea => PieChart.renderLegend p data total legendArea result
    | none => result

end Terminus
