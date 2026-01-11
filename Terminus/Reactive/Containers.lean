/-
  Terminus Reactive - Container Combinators
  Layout containers for composing reactive terminal widgets.
-/
import Terminus.Reactive.Monad

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
  emit do
    let nodes ← childRenders.mapM id
    pure (RNode.column gap style nodes)
  pure result

/-- Create a row container that collects children's renders.
    Children are laid out horizontally with the specified gap. -/
def row' (gap : Nat := 0) (style : RStyle := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let nodes ← childRenders.mapM id
    pure (RNode.row gap style nodes)
  pure result

/-! ## Docked Containers -/

/-- Dock a footer at the bottom with fixed height. -/
def dockBottom' (footerHeight : Nat := 1)
    (content : WidgetM α) (footer : WidgetM β) : WidgetM (α × β) := do
  let (contentResult, contentRenders) ← runWidgetChildren content
  let (footerResult, footerRenders) ← runWidgetChildren footer

  let mkNode (children : Array ComponentRender) : ComponentRender := do
    if children.isEmpty then
      pure RNode.empty
    else if h : children.size = 1 then
      children[0]
    else
      let nodes ← children.mapM id
      pure (RNode.column 0 {} nodes)

  emit do
    let contentNode ← mkNode contentRenders
    let footerNode ← mkNode footerRenders
    pure (RNode.dockBottom footerHeight contentNode footerNode)

  pure (contentResult, footerResult)

/-! ## Block Containers

Blocks add borders and optional titles around content.
-/

/-- Create a block container with a border. -/
def block' (borderType : BorderType := .rounded) (theme : Theme)
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let nodes ← childRenders.mapM id
    let inner := RNode.column 0 {} nodes
    pure (RNode.block none borderType (theme.borderStyle) inner)
  pure result

/-- Create a block container with a title and border. -/
def titledBlock' (title : String) (borderType : BorderType := .rounded) (theme : Theme)
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let nodes ← childRenders.mapM id
    let inner := RNode.column 0 {} nodes
    pure (RNode.block (some title) borderType (theme.borderStyle) inner)
  pure result

/-- Create a single-line bordered block. -/
def singleBlock' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .single) theme children

/-- Create a double-line bordered block. -/
def doubleBlock' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .double) theme children

/-- Create a rounded bordered block. -/
def roundedBlock' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .rounded) theme children

/-- Create a thick bordered block. -/
def thickBlock' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .thick) theme children

/-! ## Panel Containers

Panels are styled blocks with padding and themed borders.
-/

/-- Create a titled panel container. -/
def titledPanel' (title : String) (borderType : BorderType := .rounded) (theme : Theme)
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let nodes ← childRenders.mapM id
    let inner := RNode.column 1 {} nodes  -- 1 cell gap between children
    pure (RNode.block (some title) borderType (theme.borderStyle) inner)
  pure result

/-- Create an outlined panel (simple border). -/
def outlinedPanel' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .single) theme children

/-- Create an elevated panel (double border for emphasis). -/
def elevatedPanel' (theme : Theme) (children : WidgetM α) : WidgetM α :=
  block' (borderType := .double) theme children

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
