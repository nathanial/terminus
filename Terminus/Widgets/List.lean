-- Terminus.Widgets.List: Selectable list widget

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- A single item in a list -/
structure ListItem where
  content : String
  style : Style := {}
  deriving Repr, Inhabited

namespace ListItem

def new (s : String) : ListItem := { content := s }
def styled (s : String) (st : Style) : ListItem := { content := s, style := st }

end ListItem

/-- Highlight style for selected items -/
structure HighlightStyle where
  style : Style := Style.reversed
  symbol : String := ">> "
  deriving Repr, Inhabited

/-- Selectable list widget -/
structure ListWidget where
  items : List ListItem := []
  selected : Option Nat := none
  highlightStyle : HighlightStyle := {}
  block : Option Block := none
  startOffset : Nat := 0 -- For scrolling
  deriving Repr, Inhabited

namespace ListWidget

def new (items : List String) : ListWidget := {
  items := items.map ListItem.new
}

def fromItems (items : List ListItem) : ListWidget := { items }

def withSelected (l : ListWidget) (idx : Nat) : ListWidget :=
  if idx < l.items.length then { l with selected := some idx }
  else l

def withHighlight (l : ListWidget) (hs : HighlightStyle) : ListWidget := { l with highlightStyle := hs }
def withBlock (l : ListWidget) (b : Block) : ListWidget := { l with block := some b }

def selectNext (l : ListWidget) : ListWidget :=
  match l.selected with
  | none => if l.items.isEmpty then l else { l with selected := some 0 }
  | some idx =>
    let newIdx := if idx + 1 >= l.items.length then idx else idx + 1
    { l with selected := some newIdx }

def selectPrev (l : ListWidget) : ListWidget :=
  match l.selected with
  | none => if l.items.isEmpty then l else { l with selected := some (l.items.length - 1) }
  | some idx =>
    let newIdx := if idx == 0 then 0 else idx - 1
    { l with selected := some newIdx }

def getSelected (l : ListWidget) : Option ListItem :=
  match l.selected with
  | none => none
  | some idx => l.items[idx]?

/-- Adjust scroll offset to keep selection visible -/
def adjustScroll (l : ListWidget) (visibleHeight : Nat) : ListWidget :=
  match l.selected with
  | none => l
  | some idx =>
    let newOffset :=
      if idx < l.startOffset then idx
      else if idx >= l.startOffset + visibleHeight then idx - visibleHeight + 1
      else l.startOffset
    { l with startOffset := newOffset }

end ListWidget

instance : Widget ListWidget where
  render l area buf := Id.run do
    let (contentArea, buf') := renderBlockAndGetInner l.block area buf
    if contentArea.isEmpty then return buf'
    let mut result := buf'

    -- Adjust scroll to keep selection visible
    let adjustedList := l.adjustScroll contentArea.height

    -- Render visible items
    let symbolWidth := adjustedList.highlightStyle.symbol.length
    let mut row := contentArea.y

    for i in [adjustedList.startOffset : adjustedList.startOffset + contentArea.height] do
      if i >= adjustedList.items.length then break
      if row >= contentArea.y + contentArea.height then break

      let item := adjustedList.items.getD i (ListItem.new "")
      let isSelected := adjustedList.selected == some i

      -- Render highlight symbol and content
      if isSelected then
        result := result.writeStringBounded contentArea.x row symbolWidth
                    adjustedList.highlightStyle.symbol adjustedList.highlightStyle.style
        let contentX := contentArea.x + symbolWidth
        let contentWidth := if contentArea.width > symbolWidth then contentArea.width - symbolWidth else 0
        let style := Style.merge item.style adjustedList.highlightStyle.style
        result := result.writeStringBounded contentX row contentWidth item.content style
      else
        -- Add spacing for alignment
        let contentX := contentArea.x + symbolWidth
        let contentWidth := if contentArea.width > symbolWidth then contentArea.width - symbolWidth else 0
        result := result.writeStringBounded contentX row contentWidth item.content item.style

      row := row + 1

    result

end Terminus
