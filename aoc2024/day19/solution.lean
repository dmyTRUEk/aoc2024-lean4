import Lean.Data.HashMap

import aoc2024.mylib.mylib

import aoc2024.day19.examples
/- import aoc2024.day19.input -/


def Color := UInt8
@[inline] def Color.get_ (color : Color) : UInt8 := color

instance : BEq Color where
    beq := (fun a b => a.get_ == b.get_)

instance : Hashable Color where
    hash := fun color => Hashable.hash color.get_

def Color.from_char! : Char -> Color
    | 'w' => (1 : UInt8)
    | 'u' => (2 : UInt8)
    | 'b' => (3 : UInt8)
    | 'r' => (4 : UInt8)
    | 'g' => (5 : UInt8)
    |  _  => (0 : UInt8)

def Color.to_char! (color : Color) : Char :=
    match color.get_ with
    | 1 => 'w'
    | 2 => 'u'
    | 3 => 'b'
    | 4 => 'r'
    | 5 => 'g'
    | _ => '?'


def Towel := List Color
def Towels := List Towel

def Towel.from_string! (s : String) : Towel :=
    s.toList.map Color.from_char!

def Towels.from_string! (s : String) : Towels :=
    s.splitOn ", " |>.map Towel.from_string!

def Towel.to_string! (towel : Towel) : String :=
    towel.map Color.to_char! |>.join_chars ""

def Towels.to_string! (towels : Towels) : String :=
    towels.map Towel.to_string! |>.join_ ", "


def Design := List Color
def Designs := List Design

def Design.get_ (design : Design) : List Color := design

instance : BEq Design where
    beq := (fun a b => a.get_ == b.get_)

instance : Hashable Design where
    hash := fun design => Hashable.hash design.get_

def Design.from_string! (s : String) : Design :=
    s.toList.map Color.from_char!

def Designs.from_string! (s : String) : Designs :=
    s.split_lines |>.map Design.from_string!

def Design.to_string! (design : Design) : String :=
    design.map Color.to_char! |>.join_chars ""


def parse_input (input : String) : (Towels × Designs) :=
    let towels_designs := input.split_chunks
    let towels  := Towels.from_string!  towels_designs[0]!
    let designs := Designs.from_string! towels_designs[1]!
    (towels, designs)



partial def Design.is_possible (towels : Towels) (design : Design) (depth : Nat := 0) : Bool :=
    /- dbg_trace "towels = {towels.to_string!}" -/
    /- dbg_trace "design = {design.to_string!}" -/
    /- dbg_trace "depth = {depth}" -/
    /- dbg_trace "design.size = {design.size}" -/
    if design.isEmpty then true else
    let tmp := towels
        |>.any (fun towel =>
            if !towel.isPrefixOf design then false else
                design.drop towel.length |> (Design.is_possible towels . (depth+1))
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
        |>.length

#eval solve_part_one example_1
#guard example_1_answer_part_one == solve_part_one example_1




/- partial def Design.number_of_ways_raw (towels : Towels) (design : Design) : Nat := -/
/-     if design.isEmpty then 1 else -/
/-     towels -/
/-         |>.map (fun towel => -/
/-             if !towel.isPrefixOf design then 0 else -/
/-                 if !design.is_possible towels then 0 else -/
/-                     design.drop towel.length |> Design.number_of_ways_raw towels -/
/-         ) -/
/-         |>.sum -/

/- abbrev Cache := Lean.HashMap Design Towels -/
/- partial def Design.number_of_ways -/
/-     (all_towels : Towels) -/
/-     (design : Design) -/
/-     /1- (depth : Nat := 0) -1/ -/
/-     (cache : Cache) -/
/-     : Nat × Cache := -/
/-     /1- dbg_trace "towels = {towels.to_string!}" -1/ -/
/-     /1- dbg_trace "design = {design.to_string!}" -1/ -/
/-     /1- dbg_trace "depth = {depth}" -1/ -/
/-     /1- dbg_trace "design.size = {design.length}   cache.size = {cache.size}" -1/ -/
/-     if design.isEmpty then (1, cache) else -/
/-     let (towels, cache) := -/
/-         if let some towels := cache.find? $ design.take 8 then -/
/-             /1- dbg_trace "CACHE HIT" -1/ -/
/-             (towels, cache) -/
/-         else -/
/-             /1- dbg_trace "CACHE MISS" -1/ -/
/-             let towels : Towels := all_towels.filter $ fun t => t.isPrefixOf design -/
/-             let cache := cache.insert (design.take 8) towels -/
/-             (towels, cache) -/
/-     let (n, cache) := towels.foldl (fun acc el => -/
/-             let cache := acc.2 -/
/-             let t := el -/
/-             let (n, cache) := design.drop t.length |> (Design.number_of_ways all_towels . cache) -/
/-             (acc.1 + n, cache) -/
/-         ) -/
/-         (0, cache) -/
/-     let cache : Cache := cache -/
/-     (n, cache) -/
/-     /1- (0, cache) -1/ -/

abbrev Cache := Lean.HashMap Design Nat
partial def Design.number_of_ways
    (all_towels : Towels)
    (design : Design)
    /- (depth : Nat := 0) -/
    (cache : Cache)
    : Nat × Cache :=
    /- dbg_trace "towels = {towels.to_string!}" -/
    /- dbg_trace "design = {design.to_string!}" -/
    /- dbg_trace "depth = {depth}" -/
    /- dbg_trace "design.size = {design.length}   cache.size = {cache.size}" -/
    if design.isEmpty then (1, cache) else
    if let some res := cache.find? $ design then
        /- dbg_trace "CACHE HIT" -/
        (res, cache)
    else
        /- dbg_trace "CACHE MISS" -/
        let (res, cache) := all_towels
            |>.filter (fun t => t.isPrefixOf design)
            |>.foldl (fun acc el =>
                let (n_old, cache) := acc
                let t := el
                let d : Design := design.drop t.length
                let (n, cache) := d.number_of_ways all_towels cache
                (n_old + n, cache)
                /- (0, cache) -/
            )
            (0, cache)
        let cache := cache.insert design res
        (res, cache)



def solve_part_two (input : String) : OutputTypePartTwo :=
    let (towels, designs) := parse_input input
    let towels := towels.sort_by_key $ fun t => t.length
    /- let tmp := designs -/
    /-     |>.enumerate -/
    /-     |>.foldl (fun acc el => -/
    /-         let (n_old, cache) := acc -/
    /-         let (i, design) := el -/
    /-         dbg_trace "i = {i}" -/
    /-         let (n, cache) := if !design.is_possible towels then (0, cache) else -/
    /-             design.number_of_ways towels cache -/
    /-         (n_old + n, cache) -/
    /-     ) -/
    /-     (0, Lean.HashMap.empty) -/
    /- tmp.1 -/
    designs
        |>.enumerate
        |>.map (fun i_design =>
            let (i, design) := i_design
            dbg_trace "i = {i}"
            Design.number_of_ways towels design Lean.HashMap.empty
        )
        |>.map Prod.fst
        |>.sum

#eval solve_part_two example_1
#guard example_1_answer_part_two == solve_part_two example_1

