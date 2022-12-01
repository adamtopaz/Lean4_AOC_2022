import Lake
open Lake DSL

package aoc2022

@[default_target]
lean_lib Aoc2022

@[default_target]
lean_exe aoc2022 where
  root := `Main

require std from git "https://github.com/leanprover/std4" @ "main"