import aoc2024.mylib.mylib

import aoc2024.day22.examples
/- import aoc2024.day22.input -/



def parse_input (input : String) : Array Nat :=
    input
        |>.split_lines'
        |>.map String.toNat!



def evolve (s : Nat) : Nat :=
    let s := (s <<< 6).xor s |>.mod 16777216 -- | (1 <<< 24)
    let s := (s >>> 5).xor s |>.mod 16777216
    let s := (s <<< 11).xor s |>.mod 16777216
    s



def solve_part_one (input : String) : OutputTypePartOne :=
    input
        |> parse_input
        |>.map (apply_n 2000 evolve)
        |>.sum

#eval solve_part_one example_1
#guard example_1_answer_part_one == solve_part_one example_1



/- def solve_part_two (input : String) : OutputTypePartTwo := -/
/-     input -/
/-         |> parse_input -/
/-         |> sorry -/

/- #eval solve_part_two example_1 -/
/- #guard example_1_answer_part_two == solve_part_two example_1 -/

