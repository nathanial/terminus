-- Terminus.Widgets.Menu: Dropdown/popup menu widget with submenus

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Menu item definition -/
structure MenuItem where
  label : String := ""
  hotkey : Option String := none
  enabled : Bool := true
  style : Style := {}
  submenu : List MenuItem := []
  isSeparator : Bool := false
  deriving Repr, Inhabited

namespace MenuItem

def new (label : String) : MenuItem := { label }
def separator : MenuItem := { isSeparator := true }
def disabled (label : String) : MenuItem := { label, enabled := false }

def withHotkey (i : MenuItem) (hk : String) : MenuItem := { i with hotkey := some hk }
def withStyle (i : MenuItem) (s : Style) : MenuItem := { i with style := s }
def withSubmenu (i : MenuItem) (items : List MenuItem) : MenuItem := { i with submenu := items }
def withEnabled (i : MenuItem) (enabled : Bool := true) : MenuItem := { i with enabled := enabled }

def hasSubmenu (i : MenuItem) : Bool := !i.submenu.isEmpty

end MenuItem

/-- Menu widget -/
structure Menu where
  items : List MenuItem := []
  selectedPath : List Nat := []
  block : Option Block := none
  submenuBlock : Option Block := some Block.single
  openOnSelect : Bool := true
  highlightStyle : Style := Style.reversed
  normalStyle : Style := {}
  disabledStyle : Style := Style.dim
  hotkeyStyle : Style := Style.dim
  separatorStyle : Style := {}
  separatorChar : Char := '─'
  submenuIndicator : String := "▶"
  padding : Nat := 1
  deriving Repr, Inhabited

namespace Menu

def new (items : List MenuItem) : Menu := { items }
def withItems (m : Menu) (items : List MenuItem) : Menu := { m with items := items }
def withSelected (m : Menu) (idx : Nat) : Menu := { m with selectedPath := [idx] }
def withSelectedPath (m : Menu) (path : List Nat) : Menu := { m with selectedPath := path }
def withBlock (m : Menu) (b : Block) : Menu := { m with block := some b }
def withSubmenuBlock (m : Menu) (b : Option Block) : Menu := { m with submenuBlock := b }
def withHighlightStyle (m : Menu) (s : Style) : Menu := { m with highlightStyle := s }
def withNormalStyle (m : Menu) (s : Style) : Menu := { m with normalStyle := s }
def withDisabledStyle (m : Menu) (s : Style) : Menu := { m with disabledStyle := s }
def withHotkeyStyle (m : Menu) (s : Style) : Menu := { m with hotkeyStyle := s }
def withSeparator (m : Menu) (c : Char) : Menu := { m with separatorChar := c }
def withSeparatorStyle (m : Menu) (s : Style) : Menu := { m with separatorStyle := s }
def withIndicator (m : Menu) (s : String) : Menu := { m with submenuIndicator := s }
def withPadding (m : Menu) (p : Nat) : Menu := { m with padding := p }
def withOpenOnSelect (m : Menu) (enabled : Bool := true) : Menu := { m with openOnSelect := enabled }

private def borderSize (block : Option Block) : Nat :=
  match block with
  | some b => if b.borderType == .none then 0 else 2
  | none => 0

private def selectedIndex (path : List Nat) (level count : Nat) : Nat :=
  match path[level]? with
  | some idx => if count == 0 then 0 else min idx (count - 1)
  | none => 0

private def itemWidth (m : Menu) (item : MenuItem) : Nat :=
  if item.isSeparator then
    1
  else
    let labelLen := item.label.length
    let hotLen := match item.hotkey with | some hk => hk.length | none => 0
    let hotGap := if hotLen > 0 then 1 else 0
    let indLen := if item.hasSubmenu then m.submenuIndicator.length else 0
    let indGap := if indLen > 0 then 1 else 0
    let base := m.padding * 2 + labelLen + hotGap + hotLen + indGap + indLen
    max base 1

private def contentWidth (m : Menu) (items : List MenuItem) : Nat :=
  if items.isEmpty then 0
  else items.foldl (fun acc item => max acc (itemWidth m item)) (m.padding * 2 + 1)

private def desiredOuterSize (m : Menu) (items : List MenuItem) (block : Option Block) : Nat × Nat :=
  let w := contentWidth m items + borderSize block
  let h := items.length + borderSize block
  (w, h)

private def fillRow (buf : Buffer) (x y width : Nat) (style : Style) : Buffer := Id.run do
  let mut result := buf
  for col in [x : x + width] do
    result := result.setStyled col y ' ' style
  result

private def renderRow (m : Menu) (item : MenuItem) (isSelected : Bool) (x y width : Nat) (buf : Buffer) : Buffer := Id.run do
  let baseStyle := if item.enabled then m.normalStyle else m.disabledStyle
  let itemStyle := Style.merge baseStyle item.style
  let rowStyle := if isSelected then Style.merge itemStyle m.highlightStyle else itemStyle

  let mut result := fillRow buf x y width rowStyle

  if item.isSeparator then
    let sepStyle := Style.merge rowStyle m.separatorStyle
    for col in [x : x + width] do
      result := result.setStyled col y m.separatorChar sepStyle
    return result

  if width == 0 then return result

  let indicatorLen := if item.hasSubmenu then m.submenuIndicator.length else 0
  let indicatorGap := if indicatorLen > 0 then 1 else 0
  let hotkeyLen := match item.hotkey with | some hk => hk.length | none => 0
  let hotkeyGap := if hotkeyLen > 0 then 1 else 0
  let rightReserved := m.padding + indicatorLen + indicatorGap + hotkeyLen + hotkeyGap

  let labelX := x + m.padding
  let labelWidth := if width > m.padding + rightReserved then
    width - m.padding - rightReserved
  else
    0
  if labelWidth > 0 then
    result := result.writeStringBounded labelX y labelWidth item.label rowStyle

  if indicatorLen > 0 && width >= m.padding + indicatorLen then
    let indX := x + width - m.padding - indicatorLen
    result := result.writeStringBounded indX y indicatorLen m.submenuIndicator rowStyle

  match item.hotkey with
  | some hk =>
    if hotkeyLen > 0 && width >= m.padding + indicatorLen + indicatorGap + hotkeyLen then
      let hkX := x + width - m.padding - indicatorLen - indicatorGap - hotkeyLen
      let hkStyle := Style.merge rowStyle m.hotkeyStyle
      result := result.writeStringBounded hkX y hotkeyLen hk hkStyle
  | none => pure ()

  result

structure MenuRender where
  outer : Rect
  content : Rect
  selectedIdx : Nat
  selectedRowY : Nat
  deriving Inhabited

private def renderMenuAt (m : Menu) (items : List MenuItem) (selectedIdx : Nat) (area : Rect)
    (block : Option Block) (buf : Buffer) : Buffer × MenuRender := Id.run do
  if area.isEmpty || items.isEmpty then
    return (buf, { outer := area, content := area, selectedIdx := 0, selectedRowY := area.y })

  let (desiredW, desiredH) := desiredOuterSize m items block
  let width := min area.width desiredW
  let height := min area.height desiredH
  let outer := { x := area.x, y := area.y, width := width, height := height }

  if outer.isEmpty then
    return (buf, { outer := outer, content := outer, selectedIdx := 0, selectedRowY := outer.y })

  let mut result := match block with
    | some b => Widget.render b outer buf
    | none => buf

  let content := match block with
    | some b => b.innerArea outer
    | none => outer

  if content.isEmpty then
    return (result, { outer := outer, content := content, selectedIdx := 0, selectedRowY := content.y })

  let visibleCount := min items.length content.height
  let selectedRow := if visibleCount == 0 then 0 else min selectedIdx (visibleCount - 1)

  for i in [0 : visibleCount] do
    let item := items.getD i MenuItem.separator
    let isSelected := i == selectedIdx
    result := renderRow m item isSelected content.x (content.y + i) content.width result

  (result, { outer := outer, content := content, selectedIdx := selectedIdx, selectedRowY := content.y + selectedRow })

private def clipToRoot (root r : Rect) : Rect := Rect.intersect root r

partial def renderMenus (m : Menu) (items : List MenuItem) (path : List Nat) (root : Rect)
    (area : Rect) (block : Option Block) (buf : Buffer) : Buffer := Id.run do
  if area.isEmpty || items.isEmpty then return buf

  let selectedIdx := selectedIndex path 0 items.length
  let (buf, info) := renderMenuAt m items selectedIdx area block buf

  let selectedItem := items.getD selectedIdx MenuItem.separator
  if !selectedItem.hasSubmenu then
    return buf

  let openSubmenu := m.openOnSelect || path.length > 1
  if !openSubmenu then return buf

  let subItems := selectedItem.submenu
  if subItems.isEmpty then return buf

  let subPath := match path with
    | _ :: rest => rest
    | [] => []

  let (subW, subH) := desiredOuterSize m subItems m.submenuBlock
  let subX := info.outer.x + info.outer.width + 1
  let subY := info.selectedRowY
  let subRect := clipToRoot root { x := subX, y := subY, width := subW, height := subH }

  if subRect.isEmpty then
    return buf

  renderMenus m subItems subPath root subRect m.submenuBlock buf

end Menu

instance : Widget Menu where
  render m area buf := Menu.renderMenus m m.items m.selectedPath area area m.block buf

end Terminus
