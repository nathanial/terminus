-- Terminus.Widgets.Logger: Live log display with levels and filtering

import Terminus.Widgets.Widget
import Terminus.Widgets.Block

namespace Terminus

/-- Log severity level. -/
inductive LogLevel where
  | trace
  | debug
  | info
  | warn
  | error
  deriving Repr, BEq, Inhabited

namespace LogLevel

def severity : LogLevel → Nat
  | .trace => 0
  | .debug => 1
  | .info  => 2
  | .warn  => 3
  | .error => 4

def label : LogLevel → String
  | .trace => "TRACE"
  | .debug => "DEBUG"
  | .info  => "INFO"
  | .warn  => "WARN"
  | .error => "ERROR"

def tag (lvl : LogLevel) : String :=
  s!"[{lvl.label}]"

end LogLevel

/-- A single log entry. -/
structure LogEntry where
  level : LogLevel := .info
  timestamp : Option String := none
  message : String := ""
  messageStyle : Style := {}
  deriving Repr, Inhabited

namespace LogEntry

def new (level : LogLevel) (message : String) (timestamp : Option String := none) : LogEntry :=
  { level, timestamp, message }

def withStyle (e : LogEntry) (st : Style) : LogEntry := { e with messageStyle := st }

end LogEntry

/-- Styling configuration for the logger. -/
structure LoggerStyles where
  trace : Style := (Style.dim.withFg Color.gray)
  debug : Style := (Style.fgColor Color.cyan)
  info : Style := (Style.fgColor Color.green)
  warn : Style := (Style.bold.withFg Color.yellow)
  error : Style := (Style.bold.withFg Color.red)
  timestamp : Style := (Style.dim.withFg Color.gray)
  message : Style := Style.default
  deriving Repr, Inhabited

namespace LoggerStyles

def level (s : LoggerStyles) : LogLevel → Style
  | .trace => s.trace
  | .debug => s.debug
  | .info  => s.info
  | .warn  => s.warn
  | .error => s.error

end LoggerStyles

/-- Simple log level filter. If `allowedLevels` is set, only those levels pass.
If `minLevel` is set, only levels with `severity ≥ minLevel` pass. -/
structure LogLevelFilter where
  minLevel : Option LogLevel := none
  allowedLevels : Option (List LogLevel) := none
  deriving Repr, Inhabited

namespace LogLevelFilter

def allows (f : LogLevelFilter) (lvl : LogLevel) : Bool :=
  let minOk :=
    match f.minLevel with
    | none => true
    | some minLvl => minLvl.severity <= lvl.severity
  let allowedOk :=
    match f.allowedLevels with
    | none => true
    | some ls => ls.contains lvl
  minOk && allowedOk

end LogLevelFilter

/-- Logger widget: renders a scrollable list of log entries with optional follow mode. -/
structure Logger where
  entries : List LogEntry := []
  scroll : Nat := 0
  follow : Bool := true
  filter : LogLevelFilter := {}
  showTimestamp : Bool := true
  showLevel : Bool := true
  levelWidth : Nat := 7          -- e.g. "[ERROR]"
  timestampWidth : Nat := 8      -- e.g. "12:03:45"
  style : Style := {}
  styles : LoggerStyles := {}
  block : Option Block := none
  deriving Repr, Inhabited

namespace Logger

def new : Logger := {}

def withBlock (l : Logger) (b : Block) : Logger := { l with block := some b }
def withFollow (l : Logger) (on : Bool) : Logger := { l with follow := on }
def toggleFollow (l : Logger) : Logger := { l with follow := !l.follow }
def withFilter (l : Logger) (f : LogLevelFilter) : Logger := { l with filter := f }
def withMinLevel (l : Logger) (minLevel : Option LogLevel) : Logger :=
  { l with filter := { l.filter with minLevel := minLevel } }
def withAllowedLevels (l : Logger) (allowed : Option (List LogLevel)) : Logger :=
  { l with filter := { l.filter with allowedLevels := allowed } }

def withStyles (l : Logger) (s : LoggerStyles) : Logger := { l with styles := s }
def withStyle (l : Logger) (s : Style) : Logger := { l with style := s }
def withShowTimestamp (l : Logger) (b : Bool) : Logger := { l with showTimestamp := b }
def withShowLevel (l : Logger) (b : Bool) : Logger := { l with showLevel := b }
def withLevelWidth (l : Logger) (w : Nat) : Logger := { l with levelWidth := w }
def withTimestampWidth (l : Logger) (w : Nat) : Logger := { l with timestampWidth := w }

def add (l : Logger) (e : LogEntry) : Logger := { l with entries := l.entries ++ [e] }
def extend (l : Logger) (es : List LogEntry) : Logger := { l with entries := l.entries ++ es }
def clear (l : Logger) : Logger := { l with entries := [] , scroll := 0, follow := true }

def scrollUp (l : Logger) (n : Nat := 1) : Logger :=
  { l with scroll := l.scroll - n, follow := false }

def scrollDown (l : Logger) (n : Nat := 1) : Logger :=
  { l with scroll := l.scroll + n, follow := false }

def home (l : Logger) : Logger := { l with scroll := 0, follow := false }

def end_ (l : Logger) : Logger := { l with scroll := Nat.succ l.scroll, follow := true }

private def padRight (s : String) (w : Nat) : String :=
  if s.length >= w then s else
    s ++ String.ofList (List.replicate (w - s.length) ' ')

private def writeSegments (buf : Buffer) (x y maxWidth : Nat) (segs : List (String × Style)) : Buffer := Id.run do
  let mut out := buf
  let mut col := x
  let endCol := x + maxWidth
  for (txt, st) in segs do
    for c in txt.toList do
      if col >= endCol then break
      out := out.setStyled col y c st
      col := col + 1
  out

private def filteredEntries (l : Logger) : List LogEntry :=
  l.entries.filter (fun e => l.filter.allows e.level)

end Logger

instance : Widget Logger where
  render l area buf := Id.run do
    let mut result := match l.block with
      | some block => Widget.render block area buf
      | none => buf

    let contentArea := match l.block with
      | some block => block.innerArea area
      | none => area

    if contentArea.isEmpty then return result

    let entries := l.filteredEntries
    let total := entries.length
    let visible := contentArea.height

    let maxScroll := if total > visible then total - visible else 0
    let scroll := if l.follow then maxScroll else min l.scroll maxScroll

    let mut row := contentArea.y
    for i in [0 : visible] do
      let idx := scroll + i
      if idx >= total then break
      if row >= contentArea.y + visible then break

      let entry := entries.getD idx ({ level := .info, message := "" } : LogEntry)

      let base := l.style
      let levelStyle := Style.merge base (l.styles.level entry.level)
      let tsStyle := Style.merge base l.styles.timestamp
      let msgStyle := Style.merge (Style.merge base l.styles.message) entry.messageStyle

      let mut segs : List (String × Style) := []
      if l.showLevel then
        let tag := Logger.padRight (entry.level.tag) l.levelWidth
        segs := segs ++ [(tag ++ " ", levelStyle)]

      if l.showTimestamp then
        let ts := Logger.padRight (entry.timestamp.getD "") l.timestampWidth
        segs := segs ++ [(ts ++ " ", tsStyle)]

      segs := segs ++ [(entry.message, msgStyle)]

      result := Logger.writeSegments result contentArea.x row contentArea.width segs
      row := row + 1

    result

end Terminus
