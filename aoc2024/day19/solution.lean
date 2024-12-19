import aoc2024.mylib.mylib

import aoc2024.day19.examples
/- import aoc2024.day19.input -/


def Color := UInt8
@[inline] def Color.get (color : Color) : UInt8 := color

instance : BEq Color where
    beq := (fun a b => a.get == b.get)

def Color.from_char! : Char -> Color
    | 'w' => (1 : UInt8)
    | 'u' => (2 : UInt8)
    | 'b' => (3 : UInt8)
    | 'r' => (4 : UInt8)
    | 'g' => (5 : UInt8)
    |  _  => (0 : UInt8)

def Color.to_char! (color : Color) : Char :=
    match color.get with
    | 1 => 'w'
    | 2 => 'u'
    | 3 => 'b'
    | 4 => 'r'
    | 5 => 'g'
    | _ => '?'


def Towel := Array Color
def Towels := Array Towel

def Towel.from_string! (s : String) : Towel :=
    s.toList.toArray.map Color.from_char!

def Towels.from_string! (s : String) : Towels :=
    s.splitOn ", " |>.map Towel.from_string! |>.toArray

def Towel.to_string! (towel : Towel) : String :=
    towel.map Color.to_char! |>.join_chars' ""

def Towels.to_string! (towels : Towels) : String :=
    towels.map Towel.to_string! |>.join_ ", "


def Design := Array Color
def Designs := Array Design

def Design.from_string! (s : String) : Design :=
    s.toList.toArray.map Color.from_char!

def Designs.from_string! (s : String) : Designs :=
    s.split_lines' |>.map Design.from_string!

def Design.to_string! (design : Design) : String :=
    design.map Color.to_char! |>.join_chars' ""


def parse_input (input : String) : (Towels × Designs) :=
    let towels_designs := input.split_chunks
    let towels  := Towels.from_string!  towels_designs[0]!
    let designs := Designs.from_string! towels_designs[1]!
    (towels, designs)


/- #eval (#[3] : Array Nat).isEmpty -/


partial def Design.is_possible (towels : Towels) (design : Design) (depth : Nat := 0) : Bool :=
    /- dbg_trace "towels = {towels.to_string!}" -/
    /- dbg_trace "design = {design.to_string!}" -/
    /- dbg_trace "depth = {depth}" -/
    /- dbg_trace "design.size = {design.size}" -/
    if design.isEmpty then true else
    let tmp := towels
        |>.any (fun towel =>
            if !towel.isPrefixOf design then false else
                design.drop towel.size |> (Design.is_possible towels . (depth+1))
        )
    /- dbg_trace "tmp = {tmp.map Towel.to_string!}" -/
    tmp
    /-     |>.isEmpty -/
    /-     |>.not -/

#eval Design.from_string! "rgrgr" |>.is_possible $ Towels.from_string! "r, g"


def solve_part_one (input : String) : OutputTypePartOne :=
    let (towels, designs) := parse_input input
    /- dbg_trace "towels = {towels.to_string!}" -/
    /- dbg_trace "designs = {designs.map Design.to_string!}" -/
    designs
        |>.enumerate
        |>.filter (fun i_design =>
            let (i, design) := i_design
            /- dbg_trace "i = {i}" -/
            design.is_possible towels
        )
        |>.size

#eval solve_part_one example_1
#guard example_1_answer_part_one == solve_part_one example_1



/- def solve_part_two (input : String) : OutputTypePartTwo := -/
/-     input -/
/-         |> parse_input -/
/-         |> sorry -/

/- #eval solve_part_two example_1 -/
/- #guard example_1_answer_part_two == solve_part_two example_1 -/

