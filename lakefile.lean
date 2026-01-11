import Lake
open Lake DSL

package «terminus» where
  -- precompileModules := true  -- disabled to test TerminusTests build

require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.7"
require raster from git "https://github.com/nathanial/raster" @ "v0.0.2"
require reactive from git "https://github.com/nathanial/reactive" @ "v0.0.4"
require chronicle from git "https://github.com/nathanial/chronicle" @ "v0.0.2"

lean_lib «Terminus» where
  roots := #[`Terminus]

-- Test library using globs pattern (matches collimator)
lean_lib TerminusTests where
  globs := #[.submodules `TerminusTests]
  precompileModules := false

@[default_target]
lean_exe «reactive_demo» where
  root := `examples.ReactiveDemo

lean_exe «reactive_input» where
  root := `examples.ReactiveInput

lean_exe «reactive_nav» where
  root := `examples.ReactiveNav

lean_exe «reactive_game» where
  root := `examples.ReactiveGame

lean_exe «reactive_editor» where
  root := `examples.ReactiveEditor

lean_exe «reactive_async» where
  root := `examples.ReactiveAsync

lean_exe «reactive_data» where
  root := `examples.ReactiveData

lean_exe «reactive_feedback» where
  root := `examples.ReactiveFeedback

lean_exe «reactive_charts» where
  root := `examples.ReactiveCharts

lean_exe «reactive_media» where
  root := `examples.ReactiveMedia

@[test_driver]
lean_exe terminus_tests where
  root := `TerminusTests.Main

-- FFI: Build C code and link it
target ffi.o pkg : System.FilePath := do
  let oFile := pkg.buildDir / "ffi" / "terminus.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "terminus.c"
  let weakArgs := #["-I", (← getLeanIncludeDir).toString]
  buildO oFile srcJob weakArgs #["-fPIC"] "cc" getLeanTrace

extern_lib «terminus_ffi» pkg := do
  let name := nameToStaticLib "terminus_ffi"
  let ffiO ← ffi.o.fetch
  buildStaticLib (pkg.buildDir / "lib" / name) #[ffiO]
