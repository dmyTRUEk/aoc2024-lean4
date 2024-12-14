import aoc2024.mylib

import aoc2024.day14.example
/- import aoc2024.day14.input -/


structure Robot where
    p : Vec2n
    v : Vec2i
deriving Repr, Inhabited



def Robot.from_string! (s : String) : Robot :=
    if let [ps, vs] := s.splitOn " " then
        { p := Vec2n.from_string! ps "p=" ",", v := Vec2i.from_string! vs "v=" "," }
    else
        default



def parse_input (input : String) : List Robot :=
    input
        |>.split_lines
        |>.map Robot.from_string!



def get_robots_initial_positions (input : String) : List Robot :=
    input |> parse_input



def get_robots_positions_after_n_seconds (seconds : Nat) (wh : Nat × Nat) (robots: List Robot) : List Vec2n :=
    let (w, h) := wh
    if w % 2 != 1 || h % 2 != 1 then [] else
    robots.map (fun r => {
        x := (r.p.x + seconds * r.v.x : Int) % w |>.toNat
        y := (r.p.y + seconds * r.v.y : Int) % h |>.toNat
        : Vec2n
    })



def solve_part_one (input : String) (wh : Nat × Nat) : Nat :=
    let (w, h) := wh
    if w % 2 != 1 || h % 2 != 1 then 0 else
    let seconds := 100
    let final_positions := input
        |> parse_input
        |> get_robots_positions_after_n_seconds seconds wh
    let w' := w / 2
    let h' := h / 2
    let tl := final_positions.partition (fun p => p.x < w' && p.y < h') |>.1.length
    let tr := final_positions.partition (fun p => p.x > w' && p.y < h') |>.1.length
    let bl := final_positions.partition (fun p => p.x < w' && p.y > h') |>.1.length
    let br := final_positions.partition (fun p => p.x > w' && p.y > h') |>.1.length
    tl * tr * bl * br

#eval solve_part_one example_1 example_1_wh
#guard example_1_answer == solve_part_one example_1 example_1_wh



def positions_to_map (positions : List Vec2n) (wh : Nat × Nat) : String :=
    let (w, h) := wh
    /- let line := ".".replicate w -/
    let m := List.replicate h $ List.replicate w 0
    let m := positions.foldl
        (fun m p =>
            let v := m[p.y]![p.x]!
            m.replace2d p.y p.x (v+1)
        )
        m
    m.map_rec (fun n => if n == 0 then "." else s!"{n}")
        |>.map (fun l => l.join_ " ")
        |>.join_ "\n"



partial def fff (seconds seconds_max : Nat) (robots : List Robot) (wh : Nat × Nat) : String :=
    if seconds > seconds_max then s!"{seconds}" else
    let (w, h) := wh
    if w % 2 != 1 || h % 2 != 1 then "ERROR" else
    dbg_trace "\n\n\n\n\nseconds = {seconds}"
    let final_positions := robots |> get_robots_positions_after_n_seconds seconds wh
    dbg_trace positions_to_map final_positions wh
    /- dbgSleep 200 (fun _ => fff (seconds+1) input wh) -/
    /- dbgSleep 1 (fun _ => fff (seconds+1) seconds_max input wh) -/
    fff (seconds+1) seconds_max robots wh


def solve_part_two (input : String) (wh : Nat × Nat) : String :=
    let (w, h) := wh
    if w % 2 != 1 || h % 2 != 1 then "ERROR" else
    let robots := get_robots_initial_positions input
    fff 0 10000 robots wh

/- #eval solve_part_two example_2 -/
/- #guard example_1_answer_part_two == solve_part_two example_1 -/

