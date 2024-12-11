import Lake
open Lake DSL

/- package "aoc2024-lean4" where -/
package "aoc2024" where
  -- add package configuration options here

/- lean_lib «Aoc2024Lean4» where -/
  -- add library configuration options here

/- @[default_target] -/
/- lean_exe "aoc2024-lean4" where -/
/-   root := `Main -/
  -- Enables the use of the Lean interpreter by the executable (e.g.,
  -- `runFrontend`) at the expense of increased binary size on Linux.
  -- Remove this line if you do not need such functionality.
  /- supportInterpreter := true -/

lean_lib "aoc2024"

lean_exe "day1" where
  root := `aoc2024.day1.part1

lean_exe "day2" where
  root := `aoc2024.day2.part1
