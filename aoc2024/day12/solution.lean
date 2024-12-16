import aoc2024.mylib

import aoc2024.day12.example
import aoc2024.day12.input



def List2d T := List (List T)
abbrev Map := List2d Char
def Used := List2d Bool


def parse_input (input : String) : Map :=
    input.to_list2d



structure RegionBounds where
    xmin : Nat
    xmax : Nat
    ymin : Nat
    ymax : Nat
deriving Repr

def RegionBounds.width  (rb : RegionBounds) : Nat := rb.xmax - rb.xmin + 1
def RegionBounds.height (rb : RegionBounds) : Nat := rb.ymax - rb.ymin + 1
def RegionBounds.area     (rb : RegionBounds) : Nat := rb.width * rb.height
def RegionBounds.perimeter(rb : RegionBounds) : Nat := 2 * (rb.width + rb.height)

#guard 4 == { xmin:=3, xmax:=3, ymin:=4, ymax:=4 : RegionBounds }.perimeter
#guard 1 == { xmin:=3, xmax:=3, ymin:=4, ymax:=4 : RegionBounds }.area
#guard 6 == { xmin:=3, xmax:=4, ymin:=4, ymax:=4 : RegionBounds }.perimeter
#guard 2 == { xmin:=3, xmax:=4, ymin:=4, ymax:=4 : RegionBounds }.area
#guard 10== { xmin:=3, xmax:=5, ymin:=4, ymax:=5 : RegionBounds }.perimeter
#guard 6 == { xmin:=3, xmax:=5, ymin:=4, ymax:=5 : RegionBounds }.area



def find_regions_ (m : Map) (used : Used) : (List RegionBounds × Used) :=
    if let some (y, x) := used.index2d_of_first? (eq false) then
        /- sorry -/
        /- let plant_type := m[y]![x]! -- why doesn't work? -/
        let plant_type := m.get! y |>.get! x
        let rec expand : Endomorphism RegionBounds := fun (rb : RegionBounds) =>
            let x_min_max := range_ rb.xmin rb.xmax
            /- if x_min_max.todo_any ? then -/
            sorry
        sorry
    else
        ([], used)



def find_regions (m : Map) : List RegionBounds :=
    let used := m.map_rec $ const_fun false
    find_regions_ m used |>.fst



def solve_part_one (input : String) : Nat :=
    let m := input |> parse_input
    let plant_types := m.flatten.unique
    0

#eval solve_part_one example_1
#eval solve_part_one example_2
#eval solve_part_one example_3
#guard example_1_answer == solve_part_one example_1
#guard example_2_answer == solve_part_one example_2
#guard example_3_answer == solve_part_one example_3



/- def solve_part_two (input : String) := -/
/-     input -/
/-         |> parse_input -/
/-         /1- |> sorry -1/ -/

/- #eval solve_part_two example_1 -/
/- #eval solve_part_two example_2 -/
/- #eval solve_part_two example_3 -/
/- #guard example_1_answer_ptwo == solve_part_two example_ -/
/- #guard example_2_answer_ptwo == solve_part_two example_ -/
/- #guard example_3_answer_ptwo == solve_part_two example_ -/

