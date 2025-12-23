import Lake
open Lake DSL

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
