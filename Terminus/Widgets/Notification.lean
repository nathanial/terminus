-- Terminus.Widgets.Notification: Toast-style notification widget

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Position for notification display -/
inductive NotificationPosition where
  | topLeft
  | topCenter
  | topRight
  | bottomLeft
  | bottomCenter
  | bottomRight
  deriving Repr, BEq, Inhabited

/-- Notification severity level -/
inductive NotificationLevel where
  | info
  | success
  | warning
  | error
  deriving Repr, BEq, Inhabited

namespace NotificationLevel

/-- Get the default style for a notification level -/
def toStyle : NotificationLevel → Style
  | .info => { fg := .ansi .cyan }
  | .success => { fg := .ansi .green }
  | .warning => { fg := .ansi .yellow }
  | .error => { fg := .ansi .red, modifier := { bold := true } }

/-- Get the default border style for a notification level -/
def toBorderStyle : NotificationLevel → Style
  | .info => {}
  | .success => { fg := .ansi .green }
  | .warning => { fg := .ansi .yellow }
  | .error => { fg := .ansi .red }

end NotificationLevel

/-- A single notification/toast message -/
structure Notification where
  lines : List String := []
  level : NotificationLevel := .info
  style : Option Style := none       -- Overrides level default if set
  block : Option Block := some Block.rounded
  width : Option Nat := none         -- Fixed width, or auto-size if none
  maxWidth : Nat := 50               -- Cap auto-sized width
  deriving Repr, Inhabited

namespace Notification

/-- Create a notification with a single message -/
def message (msg : String) : Notification :=
  { lines := [msg] }

/-- Create a notification with multiple lines -/
def multiline (ls : List String) : Notification :=
  { lines := ls }

/-- Builder methods -/
def withLevel (n : Notification) (l : NotificationLevel) : Notification := { n with level := l }
def withStyle (n : Notification) (s : Style) : Notification := { n with style := some s }
def withBlock (n : Notification) (b : Block) : Notification := { n with block := some b }
def withWidth (n : Notification) (w : Nat) : Notification := { n with width := some w }
def withMaxWidth (n : Notification) (w : Nat) : Notification := { n with maxWidth := w }
def noBorder (n : Notification) : Notification := { n with block := none }

/-- Convenience constructors for each level -/
def info (msg : String) : Notification := (message msg).withLevel .info
def success (msg : String) : Notification := (message msg).withLevel .success
def warning (msg : String) : Notification := (message msg).withLevel .warning
def error (msg : String) : Notification := (message msg).withLevel .error

/-- Get the effective text style -/
def textStyle (n : Notification) : Style :=
  n.style.getD n.level.toStyle

/-- Compute the size needed for this notification -/
def computeSize (n : Notification) (maxW maxH : Nat) : (Nat × Nat) :=
  -- Calculate content width (max line length, capped by maxWidth)
  let contentWidth := n.lines.foldl (fun acc line => max acc line.length) 0
  let cappedWidth := min contentWidth n.maxWidth
  let finalWidth := n.width.getD cappedWidth

  -- Add border padding if block is present (2 for left+right border)
  let borderPadding := if n.block.isSome then 2 else 0
  let totalWidth := min (finalWidth + borderPadding) maxW

  -- Height is number of lines plus border padding
  let contentHeight := n.lines.length
  let totalHeight := min (contentHeight + borderPadding) maxH

  (totalWidth, totalHeight)

end Notification

/-- A stack of notifications displayed together -/
structure NotificationStack where
  notifications : Array Notification := #[]
  position : NotificationPosition := .topRight
  margin : Nat := 1                  -- Distance from screen edge
  spacing : Nat := 1                 -- Space between notifications
  deriving Repr, Inhabited

namespace NotificationStack

/-- Create an empty stack at a position -/
def atPosition (pos : NotificationPosition) : NotificationStack :=
  { position := pos }

/-- Add a notification to the stack -/
def push (s : NotificationStack) (n : Notification) : NotificationStack :=
  { s with notifications := s.notifications.push n }

/-- Remove the oldest notification -/
def pop (s : NotificationStack) : NotificationStack :=
  if s.notifications.isEmpty then s
  else { s with notifications := s.notifications.extract 1 s.notifications.size }

/-- Remove a notification at index -/
def remove (s : NotificationStack) (idx : Nat) : NotificationStack :=
  if idx >= s.notifications.size then s
  else
    let before := s.notifications.extract 0 idx
    let after := s.notifications.extract (idx + 1) s.notifications.size
    { s with notifications := before ++ after }

/-- Clear all notifications -/
def clear (s : NotificationStack) : NotificationStack :=
  { s with notifications := #[] }

/-- Check if stack is empty -/
def isEmpty (s : NotificationStack) : Bool :=
  s.notifications.isEmpty

/-- Get number of notifications -/
def size (s : NotificationStack) : Nat :=
  s.notifications.size

def withMargin (s : NotificationStack) (m : Nat) : NotificationStack := { s with margin := m }
def withSpacing (s : NotificationStack) (sp : Nat) : NotificationStack := { s with spacing := sp }

/-- Compute the position for a single notification in the stack -/
def computePosition (s : NotificationStack) (idx : Nat) (parentArea : Rect)
    (sizes : Array (Nat × Nat)) : Rect :=
  -- Get this notification's size
  let (w, h) := sizes[idx]?.getD (20, 3)

  -- Calculate X position based on horizontal alignment
  let x := match s.position with
    | .topLeft | .bottomLeft => parentArea.x + s.margin
    | .topCenter | .bottomCenter => parentArea.x + (parentArea.width - w) / 2
    | .topRight | .bottomRight =>
        if parentArea.width > w + s.margin then parentArea.x + parentArea.width - w - s.margin
        else parentArea.x

  -- Calculate Y offset from stacking previous notifications
  let stackOffset := Id.run do
    let mut offset : Nat := 0
    for i in [:idx] do
      let (_, prevH) := sizes[i]?.getD (0, 0)
      offset := offset + prevH + s.spacing
    offset

  -- Calculate Y position based on vertical alignment
  let y := match s.position with
    | .topLeft | .topCenter | .topRight =>
        parentArea.y + s.margin + stackOffset
    | .bottomLeft | .bottomCenter | .bottomRight =>
        -- Stack upward from bottom
        let totalStackHeight := Id.run do
          let mut total : Nat := 0
          for i in [:sizes.size] do
            let (_, notifH) := sizes[i]?.getD (0, 0)
            total := total + notifH
            if i + 1 < sizes.size then total := total + s.spacing
          total
        if parentArea.height > totalStackHeight + s.margin then
          parentArea.y + parentArea.height - totalStackHeight - s.margin + stackOffset
        else
          parentArea.y + stackOffset

  { x := x, y := y, width := w, height := h }

end NotificationStack

/-- Widget instance for a single Notification -/
instance : Widget Notification where
  render n area buf := Id.run do
    if area.isEmpty then return buf

    let mut result := buf

    -- Get the notification's computed size (clamped to area)
    let (w, h) := n.computeSize area.width area.height
    let notifArea := { x := area.x, y := area.y, width := w, height := h }

    -- Clear background
    result := result.fillRect notifArea Cell.empty

    -- Render block border if present, with level-appropriate border style
    match n.block with
    | some block =>
      let styledBlock := block.withBorderStyle n.level.toBorderStyle
      result := Widget.render styledBlock notifArea result
    | none => pure ()

    -- Get content area (inside border)
    let contentArea := match n.block with
      | some b => b.innerArea notifArea
      | none => notifArea

    -- Render lines
    let style := n.textStyle
    for hi : i in [:n.lines.length] do
      if i < contentArea.height then
        match n.lines[i]? with
        | some line =>
          result := result.writeStringBounded contentArea.x (contentArea.y + i)
                           contentArea.width line style
        | none => pure ()

    result

/-- Widget instance for NotificationStack -/
instance : Widget NotificationStack where
  render s area buf := Id.run do
    if s.isEmpty then return buf

    let mut result := buf

    -- Compute sizes for all notifications
    let sizes := s.notifications.map fun n => n.computeSize area.width area.height

    -- Render each notification at its computed position
    for hi : i in [:s.notifications.size] do
      let pos := s.computePosition i area sizes
      match s.notifications[i]? with
      | some n => result := Widget.render n pos result
      | none => pure ()

    result

end Terminus
