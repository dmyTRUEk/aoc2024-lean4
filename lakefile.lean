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

lean_exe "day12" where
  root := `aoc2024.day12.part1
lean_exe "day12p2" where
  root := `aoc2024.day12.part2

lean_exe "day13" where
  root := `aoc2024.day13.part1
lean_exe "day13p2" where
  root := `aoc2024.day13.part2

lean_exe "day14" where
  root := `aoc2024.day14.part1
lean_exe "day14p2" where
  root := `aoc2024.day14.part2

lean_exe "day16" where
  root := `aoc2024.day16.part1
lean_exe "day16p2" where
  root := `aoc2024.day16.part2

lean_exe "day17" where
  root := `aoc2024.day17.part1
lean_exe "day17p2" where
  root := `aoc2024.day17.part2



/- lean_exe "dayXX" where -/
/-   root := `aoc2024.dayXX.part1 -/
/- lean_exe "dayXXp2" where -/
/-   root := `aoc2024.dayXX.part2 -/

