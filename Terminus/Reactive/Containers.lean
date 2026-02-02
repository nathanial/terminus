/-
  Terminus Reactive - Container Combinators
  Layout containers for composing reactive terminal widgets.
-/
import Terminus.Reactive.Monad
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Basic Containers

These combinators run child WidgetM computations and wrap their renders
in container widgets. They enable declarative nesting like Reflex-DOM.
-/

/-- Create a column container that collects children's renders.
    Children are laid out vertically with the specified gap. -/
def column' (gap : Nat := 0) (style : RStyle := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let node ← childrenList.map' fun nodes =>
    RNode.column gap style nodes.toArray
  emit node
  pure result

/-- Create a row container that collects children's renders.
    Children are laid out horizontally with the specified gap. -/
def row' (gap : Nat := 0) (style : RStyle := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let node ← childrenList.map' fun nodes =>
    RNode.row gap style nodes.toArray
  emit node
  pure result

/-! ## Docked Containers -/

/-- Dock a footer at the bottom with fixed height. -/
def dockBottom' (footerHeight : Nat := 1)
    (content : WidgetM α) (footer : WidgetM β) : WidgetM (α × β) := do
  let (contentResult, contentRenders) ← runWidgetChildren content
  let (footerResult, footerRenders) ← runWidgetChildren footer

  let mkNode (children : Array ComponentRender) : WidgetM ComponentRender := do
    let list ← Reactive.Dynamic.sequence children.toList
    list.map' fun kids =>
      if kids.isEmpty then
        RNode.empty
      else if _ : kids.length = 1 then
        kids.head!
      else
        RNode.column 0 {} kids.toArray

  let contentNode ← mkNode contentRenders
  let footerNode ← mkNode footerRenders
  let node ← contentNode.zipWith' (fun c f => RNode.dockBottom footerHeight c f) footerNode
  emit node

  pure (contentResult, footerResult)

/-! ## Block Containers

Blocks add borders and optional titles around content.
-/

private def panelBlock (title : Option String) (borderType : BorderType) (theme : Theme)
    (fillStyle : Option Style := none) (gap : Nat := 0) (children : WidgetM α) : WidgetM α := do
  let events ← getEventsW
  let panelGroup ← SpiderM.liftIO <| events.registry.newGroup "panel"
  let (result, childRenders) ← withFocusGroupW panelGroup (runWidgetChildren children)
  let childrenList ← Reactive.Dynamic.sequence childRenders.toList
  let isFocusedDyn ← events.registry.focusedGroups.map' (·.contains panelGroup)
  let node ← childrenList.zipWith' (fun nodes isFocused =>
    let borderStyle :=
      if isFocused then
        { fg := theme.primary, modifier := { bold := true } }
      else
        theme.borderStyle
    let inner := RNode.column gap {} nodes.toArray
    RNode.block title borderType borderStyle fillStyle inner
  ) isFocusedDyn
  emit node
  pure result

/-- Create a block container with a border. -/
def block' (borderType : BorderType := .rounded) (theme : Theme)
    (fillStyle : Option Style := none) (children : WidgetM α) : WidgetM α := do
  panelBlock none borderType theme fillStyle 0 children

/-- Create a block container with a title and border. -/
def titledBlock' (title : String) (borderType : BorderType := .rounded) (theme : Theme)
    (fillStyle : Option Style := none) (children : WidgetM α) : WidgetM α := do
  panelBlock (some title) borderType theme fillStyle 0 children

/-- Create a single-line bordered block. -/
def singleBlock' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .single) theme none children

/-- Create a double-line bordered block. -/
def doubleBlock' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .double) theme none children

/-- Create a rounded bordered block. -/
def roundedBlock' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .rounded) theme none children

/-- Create a thick bordered block. -/
def thickBlock' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .thick) theme none children

/-! ## Panel Containers

Panels are styled blocks with padding and themed borders.
-/

/-- Create a titled panel container. -/
def titledPanel' (title : String) (borderType : BorderType := .rounded) (theme : Theme)
    (fillStyle : Option Style := none) (children : WidgetM α) : WidgetM α := do
  panelBlock (some title) borderType theme fillStyle 1 children

/-- Create an outlined panel (simple border). -/
def outlinedPanel' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .single) theme none children

/-- Create an elevated panel (double border for emphasis). -/
def elevatedPanel' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .double) theme none children

/-! ## Flex Containers

Simplified flex-style containers.
-/

/-- Create a flex row with gap. -/
def flexRow' (gap : Nat := 0) (style : RStyle := {})
    (children : WidgetM α) : WidgetM α :=
  row' gap style children

/-- Create a flex column with gap. -/
def flexColumn' (gap : Nat := 0) (style : RStyle := {})
    (children : WidgetM α) : WidgetM α :=
  column' gap style children

/-! ## Utility Containers -/

/-- Create a container with padding. -/
def padded' (padding : Nat) (children : WidgetM α) : WidgetM α := do
  let style : RStyle := { padding }
  column' (gap := 0) (style := style) children

/-- Create a container with uniform padding (alias for padded'). -/
def padded (amount : Nat) (children : WidgetM α) : WidgetM α :=
  padded' amount children

/-- Create a centered container. -/
def centered' (children : WidgetM α) : WidgetM α :=
  column' (gap := 0) (style := {}) children

end Terminus.Reactive
