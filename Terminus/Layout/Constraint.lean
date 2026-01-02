-- Terminus.Layout.Constraint: Layout constraints for flexible sizing

namespace Terminus

/-- Constraint types for layout calculations -/
inductive Constraint where
  /-- Fixed size in cells -/
  | fixed (size : Nat)
  /-- Percentage of available space (0-100) -/
  | percent (pct : Nat)
  /-- Minimum size -/
  | min (size : Nat)
  /-- Maximum size -/
  | max (size : Nat)
  /-- Ratio relative to other ratio constraints -/
  | ratio (num denom : Nat)
  /-- Fill remaining space (like ratio 1 1) -/
  | fill
  deriving Repr, BEq, Inhabited

namespace Constraint

/-- Shorthand constructors -/
def pct (p : Nat) : Constraint := .percent p
def len (n : Nat) : Constraint := .fixed n

/-- Calculate the size for this constraint given available space -/
def resolve (c : Constraint) (available : Nat) : Nat :=
  match c with
  | .fixed size => Nat.min size available
  | .percent pct => (available * (Nat.min pct 100)) / 100
  | .min size => Nat.min size available
  | .max size => Nat.min size available
  | .ratio num denom => if denom == 0 then 0 else (available * num) / denom
  | .fill => available

end Constraint

/-- Direction for layout splitting -/
inductive Direction where
  | horizontal
  | vertical
  deriving Repr, BEq, Inhabited

end Terminus
