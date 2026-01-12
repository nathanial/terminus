-- TerminusTests.Reactive.Common: Shared setup for Reactive tests

import Terminus.Reactive
import Terminus.Backend.TerminalMock
import Reactive
import Staple

namespace TerminusTests.Reactive.Common

open Terminus
open Terminus.Reactive
open Reactive Reactive.Host

abbrev MockTerminalIO := StateT MockTerminalState IO

instance : TerminalEffect MockTerminalIO where
  enableRawMode := modify fun s => { s with rawModeEnabled := true }
  disableRawMode := modify fun s => { s with rawModeEnabled := false }
  getTerminalSize := do
    let s ← get
    pure s.terminalSize
  readByte := do
    let s ← get
    match s.inputQueue with
    | [] => pure none
    | b :: rest =>
      set { s with inputQueue := rest }
      pure (some b)
  readByteBlocking := do
    let s ← get
    match s.inputQueue with
    | [] => pure none
    | b :: rest =>
      set { s with inputQueue := rest }
      pure (some b)
  unreadByte b := modify fun s =>
    { s with inputQueue := b :: s.inputQueue }
  writeStdout str := modify fun s =>
    { s with outputBuffer := s.outputBuffer ++ str, flushed := false }
  flushStdout := modify fun s => { s with flushed := true }
  readFileBytes path := do
    let s ← get
    pure (s.files.getD path.toString ByteArray.empty)
  decodeImageBytes _ := pure none

def bufferHasChar (buf : Buffer) (c : Char) : Bool :=
  buf.cells.any (fun cell => cell.char == c)

def bufferRowPrefix (buf : Buffer) (row len : Nat) : String :=
  let chars := (List.range len).map (fun x => (buf.get x row).char)
  String.ofList chars

partial def rnodeHasChar (node : RNode) (c : Char) : Bool :=
  match node with
  | .text content _ => content.toList.any (fun ch => ch == c)
  | .block _ _ _ _ child => rnodeHasChar child c
  | .row _ _ children => children.any (fun child => rnodeHasChar child c)
  | .column _ _ children => children.any (fun child => rnodeHasChar child c)
  | .clipped child => rnodeHasChar child c
  | .scrolled _ _ child => rnodeHasChar child c
  | .dockBottom _ content footer => rnodeHasChar content c || rnodeHasChar footer c
  | .overlay base content _ => rnodeHasChar base c || rnodeHasChar content c
  | .image _ _ _ _ _ altText => altText.toList.any (fun ch => ch == c)
  | .spacer _ _ | .empty => false

partial def rnodeHasText (node : RNode) (needle : String) : Bool :=
  match node with
  | .text content _ => content == needle
  | .block _ _ _ _ child => rnodeHasText child needle
  | .row _ _ children => children.any (fun child => rnodeHasText child needle)
  | .column _ _ children => children.any (fun child => rnodeHasText child needle)
  | .clipped child => rnodeHasText child needle
  | .scrolled _ _ child => rnodeHasText child needle
  | .dockBottom _ content footer => rnodeHasText content needle || rnodeHasText footer needle
  | .overlay base content _ => rnodeHasText base needle || rnodeHasText content needle
  | .image _ _ _ _ _ altText => altText == needle
  | .spacer _ _ | .empty => false

/-- Check if any text node in the RNode tree contains the given substring. -/
partial def rnodeContainsText (node : RNode) (needle : String) : Bool :=
  match node with
  | .text content _ => Staple.String.containsSubstr content needle
  | .block title _ _ _ child =>
    let titleMatch := match title with
      | some t => Staple.String.containsSubstr t needle
      | none => false
    titleMatch || rnodeContainsText child needle
  | .row _ _ children => children.any (fun child => rnodeContainsText child needle)
  | .column _ _ children => children.any (fun child => rnodeContainsText child needle)
  | .clipped child => rnodeContainsText child needle
  | .scrolled _ _ child => rnodeContainsText child needle
  | .dockBottom _ content footer => rnodeContainsText content needle || rnodeContainsText footer needle
  | .overlay base content _ => rnodeContainsText base needle || rnodeContainsText content needle
  | .image _ _ _ _ _ altText => Staple.String.containsSubstr altText needle
  | .spacer _ _ | .empty => false

/-! ## Test Utilities for Event Capture -/

/-- Capture the latest value from an event as a Dynamic.
    Returns a Dynamic that holds `none` initially, then `some value` after each event fires.
    Use this in SpiderM to track what events were fired, then read the Dynamic in IO to check. -/
def captureLatest (e : Reactive.Event Spider a) : SpiderM (Reactive.Dynamic Spider (Option a)) := do
  let mapped ← Reactive.Host.Event.mapM some e
  Reactive.holdDyn none mapped

/-- Capture all values from an event as a Dynamic list.
    Returns a Dynamic that accumulates all fired values in order. -/
def captureAll (e : Reactive.Event Spider a) : SpiderM (Reactive.Dynamic Spider (List a)) :=
  Reactive.foldDyn (fun v acc => acc ++ [v]) [] e

/-- Capture whether an event has fired at least once.
    Returns a Dynamic Bool that becomes true after the first event.
-/
def captureFired (e : Reactive.Event Spider a) : SpiderM (Reactive.Dynamic Spider Bool) :=
  Reactive.foldDyn (fun _ _ => true) false e

end TerminusTests.Reactive.Common
