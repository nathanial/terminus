-- TerminusTests.StyleTests: Tests for Style operations

import Crucible
import Terminus.Core.Style

namespace TerminusTests.StyleTests

open Terminus
open Crucible

testSuite "Style Tests"

test "Style.default has default colors" := do
  Style.default.fg ≡ Color.default
  Style.default.bg ≡ Color.default

test "Style.bold has bold modifier" := do
  Style.bold.modifier.bold ≡ true

test "Style.withFg sets foreground color" := do
  let s := Style.default.withFg Color.red
  s.fg ≡ Color.red

test "Style.merge combines styles" := do
  let s1 := Style.bold
  let s2 := Style.fgColor Color.green
  let merged := Style.merge s1 s2
  merged.modifier.bold ≡ true
  merged.fg ≡ Color.green



end TerminusTests.StyleTests
