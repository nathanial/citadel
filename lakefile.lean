import Lake
open Lake DSL System

package citadel where
  version := v!"0.1.0"
  -- OpenSSL linking for TLS support
  moreLinkArgs := #[
    "-L/opt/homebrew/opt/openssl@3/lib",
    "-lssl",
    "-lcrypto"
  ]

require herald from git "https://github.com/nathanial/herald" @ "v0.0.2"
require crucible from git "https://github.com/nathanial/crucible" @ "v0.0.7"
require staple from git "https://github.com/nathanial/staple" @ "v0.0.2"

@[default_target]
lean_lib Citadel where
  roots := #[`Citadel]
  -- OpenSSL linking for TLS - propagates to downstream packages
  moreLinkArgs := #[
    "-L/opt/homebrew/opt/openssl@3/lib",
    "-lssl",
    "-lcrypto"
  ]

lean_lib Tests where
  roots := #[`Tests]

@[test_driver]
lean_exe citadel_tests where
  root := `Tests.Main

lean_exe static_site where
  root := `examples.StaticSite

-- FFI: Build socket C code with OpenSSL support
target socket_ffi_o pkg : FilePath := do
  let oFile := pkg.buildDir / "ffi" / "socket.o"
  let srcJob ← inputTextFile <| pkg.dir / "ffi" / "socket.c"
  let leanIncludeDir ← getLeanIncludeDir
  let weakArgs := #[
    "-I", leanIncludeDir.toString,
    "-I/opt/homebrew/opt/openssl@3/include"
  ]
  buildO oFile srcJob weakArgs #["-fPIC", "-O2"] "cc" getLeanTrace

extern_lib citadel_native pkg := do
  let name := nameToStaticLib "citadel_native"
  let ffiO ← socket_ffi_o.fetch
  buildStaticLib (pkg.buildDir / "lib" / name) #[ffiO]
