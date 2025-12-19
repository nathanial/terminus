-- Logger: Demo of the Logger widget
-- Up/Down/PgUp/PgDn scroll, f toggles follow, 0-5 set minimum level, Esc quits

import Terminus

open Terminus

namespace LoggerDemo

structure State where
  logger : Logger := (Logger.new
    |>.withBlock (Block.rounded.withTitle "Logs" |>.withBorderStyle (Style.fgColor Color.blue)))
  tick : Nat := 0
  deriving Inhabited

private def pad2 (n : Nat) : String :=
  if n < 10 then s!"0{n}" else s!"{n}"

private def formatTime (seconds : Nat) : String :=
  let s := seconds % 60
  let m := (seconds / 60) % 60
  let h := (seconds / 3600) % 24
  s!"{pad2 h}:{pad2 m}:{pad2 s}"

private def genLevel (tick : Nat) : LogLevel :=
  match tick % 5 with
  | 0 => .info
  | 1 => .debug
  | 2 => .warn
  | 3 => .error
  | _ => .trace

private def genMessage (tick : Nat) : String :=
  match tick % 8 with
  | 0 => "Server started"
  | 1 => "Connection from 10.0.0.42"
  | 2 => "GET /api/v1/items 200"
  | 3 => "Cache miss: user profile"
  | 4 => "Rate limit exceeded"
  | 5 => "Database timeout"
  | 6 => "Retrying request..."
  | _ => s!"Tick {tick}"

def draw (frame : Frame) (state : State) : Frame := Id.run do
  let area := frame.area
  let mut f := frame
  if area.isEmpty then return f

  let sections := vsplit area [.fill, .fixed 1]

  if h : 0 < sections.length then
    let main := sections[0]
    let panels := hsplit main [.fill, .fixed 30]
    if hp : 0 < panels.length then
      f := f.render state.logger panels[0]
    if hp : 1 < panels.length then
      let infoArea := panels[1]
      let minTxt :=
        match state.logger.filter.minLevel with
        | none => "none"
        | some lvl => lvl.label
      let followTxt := if state.logger.follow then "on" else "off"
      let tsTxt := if state.logger.showTimestamp then "on" else "off"
      let lvlTxt := if state.logger.showLevel then "on" else "off"
      let info := Paragraph.fromLines [
        "Controls:",
        "  ↑/↓/PgUp/PgDn : scroll",
        "  Home/End      : top/bottom",
        "  (scrollbar is shown on the right)",
        "  f             : toggle follow",
        "  0-5           : min level",
        "  t             : timestamps",
        "  l             : level tags",
        "  c             : clear",
        "  Esc           : quit",
        "",
        s!"follow: {followTxt}",
        s!"minLevel: {minTxt}",
        s!"timestamps: {tsTxt}",
        s!"level tags: {lvlTxt}"
      ]
        |>.withStyle Style.dim
        |>.withBlock (Block.rounded.withTitle "Help" |>.withBorderStyle (Style.fgColor Color.cyan))
      f := f.render info infoArea

  if h : 1 < sections.length then
    let status := sections[1]
    let text := "Up/Down/PgUp/PgDn scroll | f follow | 0-5 filter | Esc quit"
    f := f.writeString (status.x + 1) status.y text Style.dim

  f

def update (state : State) (key : Option KeyEvent) : State × Bool :=
  let tick := state.tick + 1
  let state := { state with tick := tick }

  let state :=
    if tick % 18 == 0 then
      let seconds := tick / 60
      let entry := LogEntry.new (genLevel tick) (genMessage tick) (some (formatTime seconds))
      { state with logger := state.logger.add entry }
    else
      state

  match key with
  | none => (state, false)
  | some k =>
    if k.isCtrlC || k.isCtrlQ then (state, true)
    else
      match k.code with
      | .escape => (state, true)
      | .up => ({ state with logger := state.logger.scrollUp }, false)
      | .down => ({ state with logger := state.logger.scrollDown }, false)
      | .pageUp => ({ state with logger := state.logger.scrollUp 10 }, false)
      | .pageDown => ({ state with logger := state.logger.scrollDown 10 }, false)
      | .home => ({ state with logger := state.logger.home }, false)
      | .«end» => ({ state with logger := state.logger.withFollow true }, false)
      | .char 'f' => ({ state with logger := state.logger.toggleFollow }, false)
      | .char 't' => ({ state with logger := state.logger.withShowTimestamp (!state.logger.showTimestamp) }, false)
      | .char 'l' => ({ state with logger := state.logger.withShowLevel (!state.logger.showLevel) }, false)
      | .char 'c' => ({ state with logger := state.logger.clear }, false)
      | .char '0' => ({ state with logger := state.logger.withMinLevel none }, false)
      | .char '1' => ({ state with logger := state.logger.withMinLevel (some .trace) }, false)
      | .char '2' => ({ state with logger := state.logger.withMinLevel (some .debug) }, false)
      | .char '3' => ({ state with logger := state.logger.withMinLevel (some .info) }, false)
      | .char '4' => ({ state with logger := state.logger.withMinLevel (some .warn) }, false)
      | .char '5' => ({ state with logger := state.logger.withMinLevel (some .error) }, false)
      | _ => (state, false)

end LoggerDemo

def main : IO Unit :=
  App.runApp ({} : LoggerDemo.State) LoggerDemo.draw LoggerDemo.update
