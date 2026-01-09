-- Terminus.Widgets.Tabs: Tab bar widget

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Individual tab in a tab bar -/
structure Tab where
  title : String
  style : Style := {}
  deriving Repr, Inhabited

namespace Tab

def new (title : String) : Tab := { title }
def styled (title : String) (s : Style) : Tab := { title, style := s }

end Tab

/-- Tab bar widget -/
structure Tabs where
  tabs : List Tab := []
  selected : Nat := 0
  selectedStyle : Style := Style.bold.withFg .yellow
  normalStyle : Style := {}
  divider : String := " | "
  highlightSymbol : String := "▶ "
  block : Option Block := none
  deriving Repr, Inhabited

namespace Tabs

def new (titles : List String) : Tabs := {
  tabs := titles.map Tab.new
}

def fromTabs (tabs : List Tab) : Tabs := { tabs }

def withSelected (t : Tabs) (idx : Nat) : Tabs :=
  if idx < t.tabs.length then { t with selected := idx }
  else t

def withSelectedStyle (t : Tabs) (s : Style) : Tabs := { t with selectedStyle := s }
def withNormalStyle (t : Tabs) (s : Style) : Tabs := { t with normalStyle := s }
def withDivider (t : Tabs) (d : String) : Tabs := { t with divider := d }
def withBlock (t : Tabs) (b : Block) : Tabs := { t with block := some b }

def selectNext (t : Tabs) : Tabs :=
  if t.tabs.isEmpty then t
  else { t with selected := (t.selected + 1) % t.tabs.length }

def selectPrev (t : Tabs) : Tabs :=
  if t.tabs.isEmpty then t
  else { t with selected := if t.selected == 0 then t.tabs.length - 1 else t.selected - 1 }

def getSelected (t : Tabs) : Option Tab := t.tabs[t.selected]?

end Tabs

instance : Widget Tabs where
  render t area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner t.block area buf
    if contentArea.isEmpty || t.tabs.isEmpty then return buf'
    let mut result := buf'

    let mut x := contentArea.x
    let y := contentArea.y

    for i in [0 : t.tabs.length] do
      if x >= contentArea.x + contentArea.width then break

      let tab := t.tabs.getD i (Tab.new "")
      let isSelected := i == t.selected

      -- Add highlight symbol for selected tab
      if isSelected then
        let symLen := min t.highlightSymbol.length (contentArea.x + contentArea.width - x)
        result := result.writeStringBounded x y symLen t.highlightSymbol t.selectedStyle
        x := x + t.highlightSymbol.length

      -- Render tab title
      let style := if isSelected
                   then Style.merge tab.style t.selectedStyle
                   else Style.merge tab.style t.normalStyle
      let titleLen := min tab.title.length (contentArea.x + contentArea.width - x)
      result := result.writeStringBounded x y titleLen tab.title style
      x := x + tab.title.length

      -- Add divider (except after last tab)
      if i < t.tabs.length - 1 && x < contentArea.x + contentArea.width then
        let divLen := min t.divider.length (contentArea.x + contentArea.width - x)
        result := result.writeStringBounded x y divLen t.divider t.normalStyle
        x := x + t.divider.length

    result
  handleEvent t event :=
    match event with
    | .key k =>
      match k.code with
      | .left => t.selectPrev
      | .right => t.selectNext
      | _ => t
    | _ => t
  focusable _ := true

end Terminus
