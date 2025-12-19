import Lake
open Lake DSL

package «terminus» where
  precompileModules := true

require LSpec from git
  "https://github.com/argumentcomputer/LSpec" @ "main"

lean_lib «Terminus» where
  roots := #[`Terminus]

lean_lib «Tests» where
  roots := #[`Tests]


@[default_target]
lean_exe «hello» where
  root := `examples.HelloWorld

lean_exe «bigtext» where
  root := `examples.BigText

lean_exe «counter» where
  root := `examples.Counter

lean_exe «dashboard» where
  root := `examples.Dashboard

lean_exe «charts» where
  root := `examples.Charts

lean_exe «fileexplorer» where
  root := `examples.FileExplorer

lean_exe «texteditor» where
  root := `examples.TextEditor

lean_exe «kitchensink» where
  root := `examples.KitchenSink

@[test_driver]
lean_exe «tests» where
  root := `Tests.Main

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
