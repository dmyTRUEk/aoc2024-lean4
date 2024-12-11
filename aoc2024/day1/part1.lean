/- import day1_example -/
/- def input_ := IO.FS.readFile "./day1_input.txt" -/

/- import Mathlib -/

import aoc2024.day1.example
import aoc2024.day1.input

/- set_option diagnostics true -/


/- def partition_n (l : List T) : List (List T) := -/
/-     sorry -/

def transpose (list2d : List (List T)) [Inhabited T] : List (List T) :=
    assert! true -- TODO: all have same length
    let ly := list2d.length
    let lx := list2d[0]!.length
    let ry := List.range ly
    let rx := List.range lx
    rx.map (fun x =>
        ry.map (fun y =>
            list2d[y]![x]!
        )
    )

/- #guard transpose [] == [] -/
/- #guard transpose [[]] == [[]] -/
/- /- #guard transpose [[],[]] == [[]] -/ -/
/- /- #guard transpose [[],[],[]] == [[]] -/ -/
#eval transpose [[1,2],[3,4],[5,6]]
#eval transpose [[1,3,5],[2,4,6]]
#guard transpose [[1,2],[3,4],[5,6]] == [[1,3,5],[2,4,6]]
#guard transpose [[1,3,5],[2,4,6]] == [[1,2],[3,4],[5,6]]

partial def sort [Ord T] : List T -> List T
    | [] => []
    | head :: tail =>
        let (l, r) := tail.partition (fun x => (compare x head).isLE)
        sort l ++ [head] ++ sort r


def sum [Add T] [OfNat T 0] : List T -> T
    | [] => 0
    | head :: tail => head + sum tail

#eval sum [1, 2, 3]
#eval sum [0.1, 0.2]
#eval List.range 101 |> sum


def solve (input: String) :=
    input
        |>.split (. == '\n')
        |>.map (.|>.splitOn "   ")
        |>.map (.|>.map String.toInt!) -- TODO: map_rec
        |> transpose
        |>.map sort
        |> transpose
        |>.map (fun ab => ab[0]! - ab[1]!)
        |>.map Int.natAbs
        |> sum


#eval solve example_
#guard solve example_ == 11
#eval solve input_


/- #guard false -/
def main : IO Unit :=
  IO.println $ solve input_
