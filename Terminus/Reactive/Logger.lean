/-
  Terminus Reactive - Logger Widget
  Scrolling log display with level-based styling.
-/
import Terminus.Reactive.Monad
import Terminus.Reactive.Hooks
import Terminus.Reactive.Components
import Reactive

open Reactive Reactive.Host

namespace Terminus.Reactive

/-! ## Log Level -/

/-- Log severity level. -/
inductive LogLevel where
  | debug
  | info
  | warn
  | error
  deriving Repr, BEq, Inhabited

namespace LogLevel

/-- Get the display label for a log level. -/
def label : LogLevel → String
  | .debug => "DEBUG"
  | .info  => "INFO"
  | .warn  => "WARN"
  | .error => "ERROR"

/-- Get a bracketed tag for a log level. -/
def tag (lvl : LogLevel) : String := s!"[{lvl.label}]"

end LogLevel

/-! ## Log Entry -/

/-- A single log entry. -/
structure LogEntry where
  /-- Severity level. -/
  level : LogLevel
  /-- Log message. -/
  message : String
  /-- Optional timestamp string. -/
  timestamp : Option String := none
  deriving Repr, Inhabited

namespace LogEntry

/-- Create a new log entry. -/
def new (level : LogLevel) (message : String) (timestamp : Option String := none) : LogEntry :=
  { level, message, timestamp }

/-- Create a debug log entry. -/
def debug (message : String) (timestamp : Option String := none) : LogEntry :=
  { level := .debug, message, timestamp }

/-- Create an info log entry. -/
def info (message : String) (timestamp : Option String := none) : LogEntry :=
  { level := .info, message, timestamp }

/-- Create a warning log entry. -/
def warn (message : String) (timestamp : Option String := none) : LogEntry :=
  { level := .warn, message, timestamp }

/-- Create an error log entry. -/
def error (message : String) (timestamp : Option String := none) : LogEntry :=
  { level := .error, message, timestamp }

end LogEntry

/-! ## Logger Configuration -/

/-- Configuration for logger appearance. -/
structure LoggerConfig where
  /-- Maximum number of entries to keep. -/
  maxLines : Nat := 100
  /-- Whether to show timestamps. -/
  showTimestamp : Bool := true
  /-- Whether to show level tags. -/
  showLevel : Bool := true
  /-- Style for debug messages. -/
  debugStyle : Style := { fg := .ansi .brightBlack }
  /-- Style for info messages. -/
  infoStyle : Style := {}
  /-- Style for warning messages. -/
  warnStyle : Style := { fg := .ansi .yellow }
  /-- Style for error messages. -/
  errorStyle : Style := { fg := .ansi .red }
  /-- Style for timestamps. -/
  timestampStyle : Style := { fg := .ansi .brightBlack }
  deriving Repr, Inhabited

namespace LoggerConfig

/-- Get the style for a log level. -/
def styleFor (config : LoggerConfig) : LogLevel → Style
  | .debug => config.debugStyle
  | .info  => config.infoStyle
  | .warn  => config.warnStyle
  | .error => config.errorStyle

end LoggerConfig

/-! ## Logger Result -/

/-- Result returned by logger widget. -/
structure LoggerResult where
  /-- Current entries as a Dynamic. -/
  entries : Reactive.Dynamic Spider (Array LogEntry)
  /-- Function to log a message (safe to call from background tasks). -/
  log : LogLevel → String → IO Unit
  /-- Function to clear all entries. -/
  clear : IO Unit

/-! ## Logger Widget -/

/-- Create a scrolling log display widget.

    The widget handles:
    - Auto-scroll to newest entries
    - Level-based coloring
    - Max line buffer with truncation
    - Timestamp support

    The `log` and `clear` functions can be safely called from background
    tasks - they will properly synchronize with the reactive framework.

    Example:
    ```
    let logger ← logger' { maxLines := 50 }
    -- Later, from any IO context:
    logger.log .info "Application started"
    logger.log .error "Connection failed"
    ```
-/
def logger' (config : LoggerConfig := {}) : WidgetM LoggerResult := do
  -- Get environment for frame-safe updates
  let env ← SpiderM.getEnv

  -- Create trigger events
  let (entriesEvent, fireEntries) ← newTriggerEvent (t := Spider) (a := Array LogEntry)

  -- State ref for entries
  let entriesRef ← SpiderM.liftIO (IO.mkRef (#[] : Array LogEntry))

  -- Create dynamic
  let entriesDyn ← holdDyn #[] entriesEvent

  -- Log function (wrapped for background task safety)
  let logFn : LogLevel → String → IO Unit := fun level message => do
    let timestamp := if config.showTimestamp then
      -- Simple timestamp (could be enhanced with actual time)
      some ""
    else
      none
    let entry := LogEntry.new level message timestamp

    -- Update entries with truncation
    env.withFrame do
      let entries ← entriesRef.get
      let newEntries := if entries.size >= config.maxLines then
        entries.extract 1 entries.size |>.push entry
      else
        entries.push entry
      entriesRef.set newEntries
      fireEntries newEntries

  -- Clear function
  let clearFn : IO Unit := do
    env.withFrame do
      entriesRef.set #[]
      fireEntries #[]

  -- Emit render function
  emit do
    let entries ← entriesRef.get

    if entries.isEmpty then
      pure (RNode.text "(no log entries)" config.debugStyle)
    else
      -- Build log lines
      let mut nodes : Array RNode := #[]
      for entry in entries do
        let levelStyle := config.styleFor entry.level

        -- Build the line
        let mut parts : Array RNode := #[]

        -- Timestamp
        if config.showTimestamp then
          match entry.timestamp with
          | some ts =>
            if !ts.isEmpty then
              parts := parts.push (RNode.text (ts ++ " ") config.timestampStyle)
          | none => pure ()

        -- Level tag
        if config.showLevel then
          parts := parts.push (RNode.text (entry.level.tag ++ " ") levelStyle)

        -- Message
        parts := parts.push (RNode.text entry.message levelStyle)

        nodes := nodes.push (RNode.row 0 {} parts)

      pure (RNode.column 0 {} nodes)

  pure {
    entries := entriesDyn
    log := logFn
    clear := clearFn
  }

/-! ## Convenience Functions -/

/-- Create a simple logger with default settings. -/
def simpleLogger' : WidgetM LoggerResult := logger' {}

/-- Create a compact logger without timestamps. -/
def compactLogger' (maxLines : Nat := 50) : WidgetM LoggerResult :=
  logger' { maxLines, showTimestamp := false }

/-- Create a minimal logger showing only messages (no level, no timestamp). -/
def minimalLogger' (maxLines : Nat := 50) : WidgetM LoggerResult :=
  logger' { maxLines, showTimestamp := false, showLevel := false }

end Terminus.Reactive
