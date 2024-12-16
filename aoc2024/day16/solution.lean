import Lean.Data.HashMap

import aoc2024.mylib

import aoc2024.day16.examples
/- import aoc2024.day16.input -/


inductive Dir
    | East
    | North
    | West
    | South
deriving Repr, BEq, Hashable

def Dir.to_char : Dir -> Char
    | Dir.East  => '>'
    | Dir.North => '^'
    | Dir.West  => '<'
    | Dir.South => 'v'

def Dir.to_vec : Dir -> Vec2i
    | East =>  { x:=1, y:=0 }
    | North => { x:=0, y:=-1}
    | West =>  { x:=-1,y:=0 }
    | South => { x:=0, y:=1 }

def Dir.left : Endomorphism Dir
    | East  => North
    | North => West
    | West  => South
    | South => East

def Dir.right : Endomorphism Dir
    | East  => South
    | North => East
    | West  => North
    | South => West


structure Reindeer where
    p : Vec2n
    dir : Dir
deriving Repr, BEq, Hashable

def Reindeer.forward (r : Reindeer) : Reindeer :=
    { r with p := r.p +! r.dir.to_vec }

def Reindeer.left (r : Reindeer) : Reindeer :=
    { r with dir := r.dir.left }

def Reindeer.right (r : Reindeer) : Reindeer :=
    { r with dir := r.dir.right }


def Map := List $ List Char


def parse_input (input : String) : (Map × Reindeer) :=
    let m := input.to_list2d
    let rp := Vec2n.from_prod_yx $ m.index2d_of_first! $ eq 'S'
    let rd := Dir.East
    let r := { p:=rp, dir:=rd }
    (m, r)



def m_with_reindeer_to_string (m : Map) (r : Reindeer) : String :=
    let m_with_reindeer := m.set2d_v r.p r.dir.to_char
    m_with_reindeer.as_map_to_string



def StatesHistory := Lean.HashMap Reindeer Nat

instance : Inhabited StatesHistory where
    default := Lean.HashMap.empty

partial def dfs_calc_score (m : Map) (r : Reindeer) (score : Nat) (sh : StatesHistory) : Option Nat × StatesHistory :=
    /- dbg_trace "\nr.x = {r.p.x}   r.y = {r.p.y}   dir = {r.dir.to_char}   m here = {m.get2d_v! r.p}" -/
    /- dbg_trace m_with_reindeer_to_string m r -/
    /- if score > 10^5 then (none, sh) else -- FIXME -/
    if m.get2d_v! r.p == '#' then
        (none, sh)
    else if m.get2d_v! r.p == 'E' then
        (some score, sh)
    else
        let (is_worse, sh) :=
            if let some score_recorded := sh.find? r then
                if score < score_recorded then
                    (false, sh.insert r score)
                else
                    (true, sh)
            else
                (false, sh.insert r score)
        if is_worse then (none, sh) else
        let (score_f, sh) := dfs_calc_score m r.forward (score+1)  sh
        -- TODO(optimization): if `# v #` or `# ^ #` or same vertical then dont check `.left` and `.right`
        let (score_l, sh) := dfs_calc_score m r.left  (score+1000) sh
        let (score_r, sh) := dfs_calc_score m r.right (score+1000) sh
        ([score_f, score_l, score_r].minimum?.flatten, sh)

def solve_part_one (input : String) : Option Nat :=
    let (m, r) := parse_input input
    dfs_calc_score m r 0 (default : StatesHistory) |>.1

#eval solve_part_one example_1
#guard example_1_answer == solve_part_one example_1

#eval solve_part_one example_2
#guard example_2_answer == solve_part_one example_2



/- def solve_part_two (input : String) : Nat := -/
/-     input -/
/-         |> parse_input -/
/-         |> sorry -/

/- #eval solve_part_two example_2 -/
/- #guard example_1_answer_part_two == solve_part_two example_1 -/

