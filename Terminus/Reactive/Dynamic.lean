/-
  Terminus Reactive - Dynamic Extensions
  SpiderM-friendly combinators for retained FRP widgets.
-/
import Reactive

open Reactive Reactive.Host

namespace Reactive.Host.Dynamic

/-- Combine a list of Dynamics into a Dynamic list. -/
def sequence [BEq a] (dynamics : List (Dynamic Spider a)) : SpiderM (Dynamic Spider (List a)) := do
  let init ← Dynamic.pureM []
  dynamics.foldrM (fun d acc => Dynamic.zipWithM (· :: ·) d acc) init

/-- Switch a Dynamic of Dynamics into a Dynamic, tracking subscriptions in scope. -/
def switch (dd : Dynamic Spider (Dynamic Spider a)) : SpiderM (Dynamic Spider a) := ⟨fun env => do
  let _ ← env.incrementDepth "Dynamic.switch"
  let nodeId ← env.timelineCtx.freshNodeId
  let initialInner ← dd.sample
  let initialValue ← initialInner.sample
  let (result, updateResult) ← Reactive.Dynamic.newWithId initialValue nodeId
  let currentUnsubRef ← IO.mkRef (pure () : IO Unit)

  let subscribeToInner := fun (inner : Dynamic Spider a) => do
    let oldUnsub ← currentUnsubRef.get
    oldUnsub
    let unsub ← Reactive.Event.subscribe inner.updated fun newValue => updateResult newValue
    currentUnsubRef.set unsub
    let currentValue ← inner.sample
    updateResult currentValue

  let unsubInner ← Reactive.Event.subscribe initialInner.updated fun newValue => updateResult newValue
  currentUnsubRef.set unsubInner

  let unsubOuter ← Reactive.Event.subscribe dd.updated subscribeToInner

  env.currentScope.register unsubOuter
  env.currentScope.register do
    let unsub ← currentUnsubRef.get
    unsub

  env.decrementDepth
  pure result⟩

end Reactive.Host.Dynamic

namespace Reactive.Dynamic

/-- Map a function over a Dynamic (dot-notation friendly). -/
def map' [BEq b] (da : Dynamic Spider a) (f : a → b) : SpiderM (Dynamic Spider b) :=
  Reactive.Host.Dynamic.map' da f

/-- Combine two Dynamics (dot-notation friendly). -/
def zipWith' [BEq c] (da : Dynamic Spider a) (f : a → b → c) (db : Dynamic Spider b)
    : SpiderM (Dynamic Spider c) :=
  Reactive.Host.Dynamic.zipWith' da f db

/-- Pair two Dynamics (dot-notation friendly). -/
def zip' [BEq a] [BEq b] (da : Dynamic Spider a) (db : Dynamic Spider b)
    : SpiderM (Dynamic Spider (a × b)) :=
  Reactive.Host.Dynamic.zip' da db

/-- Combine three Dynamics (dot-notation friendly). -/
def zipWith3' [BEq a] [BEq b] [BEq d] (da : Dynamic Spider a) (f : a → b → c → d)
    (db : Dynamic Spider b) (dc : Dynamic Spider c) : SpiderM (Dynamic Spider d) :=
  Reactive.Host.Dynamic.zipWith3' da f db dc

/-- Combine a list of Dynamics into a Dynamic list. -/
def sequence [BEq a] (dynamics : List (Dynamic Spider a)) : SpiderM (Dynamic Spider (List a)) :=
  Reactive.Host.Dynamic.sequence dynamics

/-- Switch a Dynamic of Dynamics into a Dynamic. -/
def switch (dd : Dynamic Spider (Dynamic Spider a)) : SpiderM (Dynamic Spider a) :=
  Reactive.Host.Dynamic.switch dd

end Reactive.Dynamic
