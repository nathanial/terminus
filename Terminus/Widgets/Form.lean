-- Terminus.Widgets.Form: Container widget for grouped input widgets

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- A type-erased widget wrapper, useful for building heterogeneous containers. -/
structure AnyWidget where
  α : Type
  inst : Widget α
  value : α

namespace AnyWidget

def of {α : Type} (w : α) [Widget α] : AnyWidget :=
  { α := α, inst := inferInstance, value := w }

def empty : AnyWidget := AnyWidget.of ({} : Empty)

def handleEvent (w : AnyWidget) (event : Event) : AnyWidget :=
  { w with value := w.inst.handleEvent w.value event }

def preferredSize (w : AnyWidget) : Option (Nat × Nat) :=
  w.inst.preferredSize w.value

end AnyWidget

instance : Inhabited AnyWidget where
  default := AnyWidget.empty

instance : Widget AnyWidget where
  render w area buf := w.inst.render w.value area buf
  preferredSize w := w.inst.preferredSize w.value
  handleEvent w event := { w with value := w.inst.handleEvent w.value event }
  focusable w := w.inst.focusable w.value
  setFocused w focused := { w with value := w.inst.setFocused w.value focused }

/-- A single row in a form. -/
inductive FormRow where
  /-- A labeled row where the content renders to the right of the label column. -/
  | field (label : String) (content : AnyWidget) (height : Nat := 1)
  /-- A full-width row (no label column). -/
  | full (content : AnyWidget) (height : Nat := 1)
  /-- A vertical spacer row. -/
  | spacer (height : Nat := 1)
  deriving Inhabited

namespace FormRow

def fieldWidget (label : String) (content : AnyWidget) (height : Nat := 1) : FormRow :=
  .field label content height

def fieldOf {α : Type} (label : String) (content : α) [Widget α] (height : Nat := 1) : FormRow :=
  .field label (AnyWidget.of content) height

def fullWidget (content : AnyWidget) (height : Nat := 1) : FormRow :=
  .full content height

def fullOf {α : Type} (content : α) [Widget α] (height : Nat := 1) : FormRow :=
  .full (AnyWidget.of content) height

def gap (n : Nat := 1) : FormRow :=
  .spacer n

def height : FormRow → Nat
  | .field _ _ h => h
  | .full _ h => h
  | .spacer h => h

end FormRow

/-- Form widget: renders a vertical list of rows, optionally with an outer block,
and aligns labeled rows to a shared label column. -/
structure Form where
  rows : List FormRow := []
  /-- Suffix appended to each `FormRow.field` label (default `":"`). -/
  labelSuffix : String := ":"
  /-- Gap (in cells) between label column and row content. -/
  labelGap : Nat := 2
  /-- Vertical spacing (in cells) between rows. -/
  rowSpacing : Nat := 0
  labelStyle : Style := Style.bold
  block : Option Block := none
  deriving Inhabited

namespace Form

def new : Form := {}

def withRows (f : Form) (rows : List FormRow) : Form := { f with rows := rows }
def withBlock (f : Form) (b : Block) : Form := { f with block := some b }
def withLabelSuffix (f : Form) (s : String) : Form := { f with labelSuffix := s }
def withLabelGap (f : Form) (n : Nat) : Form := { f with labelGap := n }
def withRowSpacing (f : Form) (n : Nat) : Form := { f with rowSpacing := n }
def withLabelStyle (f : Form) (s : Style) : Form := { f with labelStyle := s }

private def labelText (f : Form) (label : String) : String :=
  label ++ f.labelSuffix

private def computeLabelWidth (f : Form) : Nat :=
  f.rows.foldl
    (fun acc row =>
      match row with
      | .field lbl _ _ => max acc (f.labelText lbl).length
      | _ => acc)
    0

private def renderLabel (f : Form) (label : String) (area : Rect) (buf : Buffer) : Buffer :=
  if area.isEmpty then buf
  else
    let y := area.y + (area.height / 2)
    buf.writeStringBounded area.x y area.width (f.labelText label) f.labelStyle

end Form

instance : Widget Form where
  render f area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner f.block area buf
    if contentArea.isEmpty || f.rows.isEmpty then return buf'
    let mut result := buf'

    let maxLabelW := min (Form.computeLabelWidth f) contentArea.width

    let endY := contentArea.y + contentArea.height
    let mut y := contentArea.y

    for row in f.rows do
      if y >= endY then break

      let desiredH := row.height
      let h := min desiredH (endY - y)
      let rowArea : Rect := { x := contentArea.x, y := y, width := contentArea.width, height := h }

      match row with
      | .spacer _ =>
        pure ()
      | .full w _ =>
        result := Widget.render w rowArea result
      | .field label w _ =>
        let labelW := min maxLabelW rowArea.width
        let gapW := if rowArea.width > labelW then min f.labelGap (rowArea.width - labelW) else 0
        let contentW := if rowArea.width > labelW + gapW then rowArea.width - labelW - gapW else 0
        let labelArea : Rect := { x := rowArea.x, y := rowArea.y, width := labelW, height := rowArea.height }
        let contentArea : Rect := {
          x := rowArea.x + labelW + gapW,
          y := rowArea.y,
          width := contentW,
          height := rowArea.height
        }
        result := Form.renderLabel f label labelArea result
        if !contentArea.isEmpty then
          result := Widget.render w contentArea result

      y := y + desiredH + f.rowSpacing

    result

end Terminus
