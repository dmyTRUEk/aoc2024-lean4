import aoc2024.mylib
import aoc2024.day1.example
import aoc2024.day1.input

/- set_option diagnostics true -/


def solve (input : String) : Int :=
    input
        |>.splitOn "\n"
        |>.map (.|>.splitOn "   ")
        |>.map (.|>.map String.toInt!) -- TODO: map_rec
        |> transpose
        |>.map sort
        |> transpose
        |>.map (fun ab => ab[0]! - ab[1]!)
        |>.map Int.abs
        |> sum


#eval solve example_
#guard solve example_ == 11
#eval solve input_


/- #guard false -/
def main : IO Unit :=
  IO.println $ solve input_
