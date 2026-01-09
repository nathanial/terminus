-- Terminus.Widgets.Checkbox: Checkbox and radio button widgets

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Checkbox widget -/
structure Checkbox where
  label : String := ""
  checked : Bool := false
  checkedSymbol : String := "[x]"
  uncheckedSymbol : String := "[ ]"
  labelSeparator : String := " "
  symbolStyle : Style := {}
  labelStyle : Style := {}
  checkedStyle : Style := {}
  uncheckedStyle : Style := {}
  block : Option Block := none
  deriving Repr, Inhabited

namespace Checkbox

def new (label : String) : Checkbox := { label }
def withLabel (c : Checkbox) (label : String) : Checkbox := { c with label := label }
def withChecked (c : Checkbox) (checked : Bool := true) : Checkbox := { c with checked := checked }
def toggle (c : Checkbox) : Checkbox := { c with checked := !c.checked }
def withSymbols (c : Checkbox) (checked unchecked : String) : Checkbox :=
  { c with checkedSymbol := checked, uncheckedSymbol := unchecked }
def withLabelSeparator (c : Checkbox) (sep : String) : Checkbox := { c with labelSeparator := sep }
def withSymbolStyle (c : Checkbox) (s : Style) : Checkbox := { c with symbolStyle := s }
def withLabelStyle (c : Checkbox) (s : Style) : Checkbox := { c with labelStyle := s }
def withCheckedStyle (c : Checkbox) (s : Style) : Checkbox := { c with checkedStyle := s }
def withUncheckedStyle (c : Checkbox) (s : Style) : Checkbox := { c with uncheckedStyle := s }
def withBlock (c : Checkbox) (b : Block) : Checkbox := { c with block := some b }

end Checkbox

instance : Widget Checkbox where
  render c area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner c.block area buf
    if contentArea.isEmpty then return buf'
    let mut result := buf'

    let symbol := if c.checked then c.checkedSymbol else c.uncheckedSymbol
    let stateStyle := if c.checked then c.checkedStyle else c.uncheckedStyle
    let symbolStyle := Style.merge c.symbolStyle stateStyle
    let labelStyle := Style.merge c.labelStyle stateStyle

    let y := contentArea.y
    let x := contentArea.x
    let endX := contentArea.x + contentArea.width

    let symLen := symbol.length
    let symWrite := min symLen contentArea.width
    result := result.writeStringBounded x y symWrite symbol symbolStyle

    let mut nextX := x + symLen
    if nextX < endX then
      let sepLen := c.labelSeparator.length
      let sepWrite := min sepLen (endX - nextX)
      result := result.writeStringBounded nextX y sepWrite c.labelSeparator labelStyle
      nextX := nextX + sepLen

      if nextX < endX then
        let labelWidth := endX - nextX
        result := result.writeStringBounded nextX y labelWidth c.label labelStyle

    result
  handleEvent c event :=
    match event with
    | .key k =>
      match k.code with
      | .space | .enter => c.toggle
      | _ => c
    | _ => c
  focusable _ := true

/-- Single radio option -/
structure RadioOption where
  label : String
  style : Style := {}
  deriving Repr, Inhabited

namespace RadioOption

def new (label : String) : RadioOption := { label }
def styled (label : String) (style : Style) : RadioOption := { label, style }

end RadioOption

/-- Grouped radio buttons with a single selection -/
structure RadioGroup where
  options : List RadioOption := []
  selected : Option Nat := none
  selectedSymbol : String := "(•)"
  unselectedSymbol : String := "( )"
  labelSeparator : String := " "
  symbolStyle : Style := {}
  style : Style := {}
  selectedStyle : Style := {}
  unselectedStyle : Style := {}
  block : Option Block := none
  startOffset : Nat := 0
  deriving Repr, Inhabited

namespace RadioGroup

def new (labels : List String) : RadioGroup := {
  options := labels.map RadioOption.new
}

def fromOptions (options : List RadioOption) : RadioGroup := { options }

def withSelected (r : RadioGroup) (idx : Nat) : RadioGroup :=
  if idx < r.options.length then { r with selected := some idx } else r

def clearSelected (r : RadioGroup) : RadioGroup := { r with selected := none }

def withSymbols (r : RadioGroup) (selected unselected : String) : RadioGroup :=
  { r with selectedSymbol := selected, unselectedSymbol := unselected }

def withLabelSeparator (r : RadioGroup) (sep : String) : RadioGroup := { r with labelSeparator := sep }
def withStyle (r : RadioGroup) (s : Style) : RadioGroup := { r with style := s }
def withSymbolStyle (r : RadioGroup) (s : Style) : RadioGroup := { r with symbolStyle := s }
def withSelectedStyle (r : RadioGroup) (s : Style) : RadioGroup := { r with selectedStyle := s }
def withUnselectedStyle (r : RadioGroup) (s : Style) : RadioGroup := { r with unselectedStyle := s }
def withBlock (r : RadioGroup) (b : Block) : RadioGroup := { r with block := some b }
def withStartOffset (r : RadioGroup) (offset : Nat) : RadioGroup := { r with startOffset := offset }

def selectNext (r : RadioGroup) : RadioGroup :=
  match r.selected with
  | none => if r.options.isEmpty then r else { r with selected := some 0 }
  | some idx =>
    let newIdx := if idx + 1 >= r.options.length then idx else idx + 1
    { r with selected := some newIdx }

def selectPrev (r : RadioGroup) : RadioGroup :=
  match r.selected with
  | none => if r.options.isEmpty then r else { r with selected := some (r.options.length - 1) }
  | some idx =>
    let newIdx := if idx == 0 then 0 else idx - 1
    { r with selected := some newIdx }

def getSelected (r : RadioGroup) : Option RadioOption :=
  match r.selected with
  | none => none
  | some idx => r.options[idx]?

/-- Adjust scroll offset to keep selection visible -/
def adjustScroll (r : RadioGroup) (visibleHeight : Nat) : RadioGroup :=
  match r.selected with
  | none => r
  | some idx =>
    let newOffset :=
      if idx < r.startOffset then idx
      else if idx >= r.startOffset + visibleHeight then idx - visibleHeight + 1
      else r.startOffset
    { r with startOffset := newOffset }

end RadioGroup

instance : Widget RadioGroup where
  render r area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner r.block area buf
    if contentArea.isEmpty then return buf'
    let mut result := buf'

    -- Adjust scroll to keep selection visible
    let adjusted := r.adjustScroll contentArea.height

    let mut row := contentArea.y
    for i in [adjusted.startOffset : adjusted.startOffset + contentArea.height] do
      if i >= adjusted.options.length then break
      if row >= contentArea.y + contentArea.height then break

      let option := adjusted.options.getD i (RadioOption.new "")
      let isSelected := adjusted.selected == some i

      let symbol := if isSelected then adjusted.selectedSymbol else adjusted.unselectedSymbol
      let stateStyle := if isSelected then adjusted.selectedStyle else adjusted.unselectedStyle
      let symbolStyle := Style.merge adjusted.symbolStyle stateStyle
      let labelStyle := Style.merge (Style.merge adjusted.style option.style) stateStyle

      let x := contentArea.x
      let endX := contentArea.x + contentArea.width

      let symLen := symbol.length
      let symWrite := min symLen contentArea.width
      result := result.writeStringBounded x row symWrite symbol symbolStyle

      let mut nextX := x + symLen
      if nextX < endX then
        let sepLen := adjusted.labelSeparator.length
        let sepWrite := min sepLen (endX - nextX)
        result := result.writeStringBounded nextX row sepWrite adjusted.labelSeparator labelStyle
        nextX := nextX + sepLen

        if nextX < endX then
          let labelWidth := endX - nextX
          result := result.writeStringBounded nextX row labelWidth option.label labelStyle

      row := row + 1

    result
  handleEvent r event :=
    match event with
    | .key k =>
      match k.code with
      | .up | .left => r.selectPrev
      | .down | .right => r.selectNext
      | _ => r
    | _ => r
  focusable _ := true

end Terminus
