/-
  Terminus Reactive - Container Combinators
  Layout containers for composing reactive terminal widgets.
-/
import Terminus.Reactive.Monad
import Trellis

namespace Terminus.Reactive

open Trellis

/-! ## Basic Containers

These combinators run child WidgetM computations and wrap their renders
in container widgets. They enable declarative nesting like Reflex-DOM.
-/

/-- Create a column container that collects children's renders.
    Children are laid out vertically with the specified gap. -/
def column' (gap : Nat := 0) (style : BoxConstraints := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let nodes ← childRenders.mapM id
    pure (RNode.column gap style nodes)
  pure result

/-- Create a row container that collects children's renders.
    Children are laid out horizontally with the specified gap. -/
def row' (gap : Nat := 0) (style : BoxConstraints := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let nodes ← childRenders.mapM id
    pure (RNode.row gap style nodes)
  pure result

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

These provide more control over layout using Trellis flexbox properties.
-/

/-- Create a flex row with custom flex container properties. -/
def flexRow' (props : FlexContainer) (style : BoxConstraints := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let nodes ← childRenders.mapM id
    -- Use gap from flex props, convert Float to Nat
    let gap := props.gap.toUInt32.toNat
    pure (RNode.row gap style nodes)
  pure result

/-- Create a flex column with custom flex container properties. -/
def flexColumn' (props : FlexContainer) (style : BoxConstraints := {})
    (children : WidgetM α) : WidgetM α := do
  let (result, childRenders) ← runWidgetChildren children
  emit do
    let nodes ← childRenders.mapM id
    let gap := props.gap.toUInt32.toNat
    pure (RNode.column gap style nodes)
  pure result

/-! ## Utility Containers -/

/-- Create a container with padding (using Trellis BoxConstraints). -/
def padded' (padding : EdgeInsets) (children : WidgetM α) : WidgetM α := do
  let style : BoxConstraints := { padding }
  column' (gap := 0) (style := style) children

/-- Create a container with uniform padding. -/
def padded (amount : Nat) (children : WidgetM α) : WidgetM α :=
  padded' (EdgeInsets.uniform amount.toFloat) children

/-- Create a centered container (using flex alignment). -/
def centered' (children : WidgetM α) : WidgetM α := do
  let style : BoxConstraints := {
    -- Flex properties would go here but we're using simpler layout for now
  }
  column' (gap := 0) (style := style) children

end Terminus.Reactive
