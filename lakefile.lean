import Lake
open Lake DSL System

package citadel where
  version := v!"0.1.0"

require herald from ".." / "herald"
require crucible from ".." / "crucible"

@[default_target]
lean_lib Citadel where
  roots := #[`Citadel]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe citadel_tests where
  root := `Tests.Main

-- FFI: Build socket C code
target socket_ffi_o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "socket.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "socket.c"
  let leanIncludeDir ← getLeanIncludeDir
  let weakArgs := #["-I", leanIncludeDir.toString]
  buildO oFile srcJob weakArgs #["-fPIC", "-O2"] "cc" getLeanTrace

extern_lib citadel_native pkg := do
  let name := nameToStaticLib "citadel_native"
  let ffiO ← socket_ffi_o.fetch
  buildStaticLib (pkg.buildDir / "lib" / name) #[ffiO]
