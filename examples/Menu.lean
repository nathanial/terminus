-- Menu: Dropdown menu demo with nested submenus
-- Arrow keys to navigate, Enter/Right to open submenu, Left/Backspace to close, q to quit

import Terminus

open Terminus

namespace MenuDemo

def menuItems : List MenuItem := [
  (MenuItem.new "File").withSubmenu [
    (MenuItem.new "New").withHotkey "Ctrl+N",
    (MenuItem.new "Open").withHotkey "Ctrl+O",
    MenuItem.separator,
    (MenuItem.new "Save").withHotkey "Ctrl+S",
    (MenuItem.new "Quit").withHotkey "Ctrl+Q"
  ],
  (MenuItem.new "Edit").withSubmenu [
    (MenuItem.new "Undo").withHotkey "Ctrl+Z",
    (MenuItem.new "Redo").withHotkey "Ctrl+Y",
    MenuItem.separator,
    (MenuItem.disabled "Cut").withHotkey "Ctrl+X",
    (MenuItem.new "Copy").withHotkey "Ctrl+C",
    (MenuItem.new "Paste").withHotkey "Ctrl+V"
  ],
  (MenuItem.new "View").withSubmenu [
    (MenuItem.new "Zoom").withSubmenu [
      (MenuItem.new "Zoom In").withHotkey "Ctrl+Plus",
      (MenuItem.new "Zoom Out").withHotkey "Ctrl+-",
      (MenuItem.new "Reset").withHotkey "Ctrl+0"
    ],
    (MenuItem.new "Fullscreen").withHotkey "F11"
  ],
  (MenuItem.new "Help").withSubmenu [
    (MenuItem.new "Documentation"),
    (MenuItem.new "About")
  ]
]

structure State where
  path : List Nat := [0]
  deriving Inhabited

def dropLast : List Nat → List Nat
  | [] => []
  | [_] => []
  | x :: xs => x :: dropLast xs

def lastIndex : List Nat → Nat
  | [] => 0
  | [x] => x
  | _ :: xs => lastIndex xs

def setLast : List Nat → Nat → List Nat
  | [], n => [n]
  | [_], n => [n]
  | x :: xs, n => x :: setLast xs n

def itemsAtPath (items : List MenuItem) : List Nat → List MenuItem
  | [] => items
  | idx :: rest =>
    match items[idx]? with
    | some item => itemsAtPath item.submenu rest
    | none => []

def currentItems (state : State) : List MenuItem :=
  itemsAtPath menuItems (dropLast state.path)

def currentIndex (state : State) : Nat :=
  lastIndex state.path

def isSelectable (item : MenuItem) : Bool :=
  !item.isSeparator

partial def moveIndex (items : List MenuItem) (idx : Nat) (forward : Bool) (fuel : Nat) : Nat :=
  if fuel == 0 || items.isEmpty then idx
  else
    let len := items.length
    let idx := min idx (len - 1)
    let next := if forward then
      if idx + 1 < len then idx + 1 else idx
    else
      if idx == 0 then 0 else idx - 1
    let item := items.getD next MenuItem.separator
    if isSelectable item then next else moveIndex items next forward (fuel - 1)

def moveSelection (state : State) (forward : Bool) : State :=
  let items := currentItems state
  let idx := currentIndex state
  let newIdx := moveIndex items idx forward items.length
  { state with path := setLast state.path newIdx }

def openSubmenu (state : State) : State :=
  let items := currentItems state
  let idx := currentIndex state
  let item := items.getD idx MenuItem.separator
  if item.hasSubmenu then
    { state with path := state.path ++ [0] }
  else
    state

def closeSubmenu (state : State) : State :=
  if state.path.length <= 1 then state else { state with path := dropLast state.path }

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame

  let mainBlock := Block.double
    |>.withTitle "Menu Demo"
    |>.withTitleStyle (Style.bold.withFg Color.cyan)
    |>.withBorderStyle (Style.fgColor Color.blue)
  f := f.render mainBlock area

  let inner := mainBlock.innerArea area
  if inner.isEmpty then return f

  let menu := Menu.new menuItems
    |>.withSelectedPath state.path
    |>.withOpenOnSelect false
    |>.withBlock Block.single
    |>.withSubmenuBlock (some Block.single)
    |>.withHighlightStyle Style.reversed
    |>.withIndicator "▶"

  f := f.render menu inner

  let help := "↑↓: Move  →/Enter: Open  ←/Backspace: Close  q: Quit"
  if inner.height > 1 then
    let helpY := inner.y + inner.height - 1
    let helpX := inner.x
    f := f.writeString helpX helpY help Style.dim

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  match key with
  | none => (state, false)
  | some k =>
    match k.code with
    | .char 'q' => (state, true)
    | .up => (moveSelection state false, false)
    | .down => (moveSelection state true, false)
    | .right | .enter => (openSubmenu state, false)
    | .left | .backspace => (closeSubmenu state, false)
    | _ =>
      if k.isCtrlC || k.isCtrlQ then (state, true)
      else (state, false)

end MenuDemo

def main : IO Unit := do
  App.runApp ({} : MenuDemo.State) MenuDemo.draw MenuDemo.update
