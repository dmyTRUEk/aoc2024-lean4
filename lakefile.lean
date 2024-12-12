import Lake
open Lake DSL

package "aoc2024"

lean_lib "aoc2024"



lean_exe "day1" where
  root := `aoc2024.day1.part1

lean_exe "day2" where
  root := `aoc2024.day2.part1

lean_exe "day11" where
  root := `aoc2024.day11.part1
lean_exe "day11p2" where
  root := `aoc2024.day11.part2

