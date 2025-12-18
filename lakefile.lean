import Lake
open Lake DSL

package «terminus» where
  precompileModules := true

lean_lib «Terminus» where
  roots := #[`Terminus]

@[default_target]
lean_exe «hello» where
  root := `examples.HelloWorld

lean_exe «counter» where
  root := `examples.Counter

lean_exe «dashboard» where
  root := `examples.Dashboard

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
