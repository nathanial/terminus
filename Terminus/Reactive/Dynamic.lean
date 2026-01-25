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

/-- Switch a Dynamic of Dynamics into a Dynamic, tracking subscriptions in scope.
    Delegates to the reactive library's scope-managed implementation. -/
def switch (dd : Dynamic Spider (Dynamic Spider a)) : SpiderM (Dynamic Spider a) :=
  Dynamic.switchM dd

end Reactive.Host.Dynamic

namespace Reactive.Dynamic

/-- Map a function over a Dynamic (dot-notation friendly).
    No deduplication - fires on every source update.
    Use `mapUniq'` if you want deduplication. -/
def map' (da : Dynamic Spider a) (f : a → b) : SpiderM (Dynamic Spider b) :=
  Reactive.Host.Dynamic.map' da f

/-- Map a function over a Dynamic with deduplication (dot-notation friendly).
    Only fires when the mapped value actually changes (requires BEq). -/
def mapUniq' [BEq b] (da : Dynamic Spider a) (f : a → b) : SpiderM (Dynamic Spider b) :=
  Reactive.Host.Dynamic.mapUniq' da f

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
