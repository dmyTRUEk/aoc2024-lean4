import Lean.Data.HashMap

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



def positions_to_map (positions : List Vec2n) (wh : Nat × Nat) : List $ List Nat :=
    let (w, h) := wh
    let m0 := List.replicate h $ List.replicate w 0
    positions.foldl
        (fun m p =>
            let v := m[p.y]![p.x]!
            m.set2d_yx (p.y, p.x) (v+1)
        )
        m0

def positions_to_map_str (positions : List Vec2n) (wh : Nat × Nat) : String :=
    let m := positions_to_map positions wh
    m.map_rec (fun n => if n == 0 then "." else s!"{n}")
        |>.map (fun l => l.join_ " ")
        |>.join_ "\n"



partial def fff (seconds seconds_max : Nat) (robots : List Robot) (wh : Nat × Nat) : String :=
    if seconds > seconds_max then s!"{seconds}" else
    let (w, h) := wh
    if w % 2 != 1 || h % 2 != 1 then "ERROR" else
    dbg_trace "\n\n\n\n\nseconds = {seconds}"
    let final_positions := robots |> get_robots_positions_after_n_seconds seconds wh
    dbg_trace positions_to_map_str final_positions wh
    /- dbgSleep 200 (fun _ => fff (seconds+1) input wh) -/
    /- dbgSleep 1 (fun _ => fff (seconds+1) seconds_max input wh) -/
    fff (seconds+1) seconds_max robots wh



def calc_occ (m : List $ List Nat) (window_wh : Nat × Nat) : Lean.HashMap (List $ List Nat) Nat :=
    m.windows2d window_wh
        |>.foldl (fun acc el =>
            let occ := acc.find! el
            acc.insert el (occ+1)
        )
        Lean.HashMap.empty



/- def isPositive (x : Float) : Prop := -/
/-     x > 0 -/

/- -- Function that accepts only positive Float -/
/- def positiveFloatFn (x : Float) (h : isPositive x) : Float := -/
/-     x -/

/- -- Proof that converting a Float to Int and comparing against 0 ensures positivity -/
/- lemma positiveFloatToIntProof (x : Float) (h : isPositive x) -/
/-     : (Int.ofFloat x > 0) := by -/
/-     -- Start the proof -/
/-     have hx : x > 0 := h -/
/-     -- Convert x to an Int and show that it is greater than 0 -/
/-     have hInt : Int.ofFloat x > 0 := -/
/-     Int.ofFloat_eq_floor x ▸ Int.floor_pos.mpr hx -/
/-     exact hInt -/

/- def positiveFloat (x : Float) : Prop := x > 0 -/

/- def myFunction (x : Float) (h : x > 0 := by -/
/-     have xi : Int := x.toInt; -/
/-     exact xi > 0 -/
/- ) : Float := -/
/-     x * 2 -/
/- #eval myFunction 10. -/
/- #eval myFunction (-10.) -/
/- /1- #eval 5.4.toInt -1/ -/
/- /1- #eval Int.Nat -1/ -/

/- #eval 5.2.toUInt64 -/

/- #eval let x := 100.; x.log / 10..log -/

-- TODO: const assert `x > 0`.
def log (base : Float) (x : Float) : Float :=
    x.log / base.log



def fisher_information (occ : List Nat) (base : String) : Float :=
    let log := match base with
        | "e" => Float.log
        | "2" => Float.log2
        | "10" => Float.log10
        | base => log base.toNat!.toFloat
    let total_occ := occ.sum
    occ.map Nat.toFloat
        |>.map (Float.div_ total_occ.toFloat)
        |>.map (fun p => -p * log p)
        |>.sum

def fisher_information_e (occ : List Nat) : Float :=
    fisher_information occ "e"
#guard 0.6931471805599453 == fisher_information_e [1, 1]
#guard 0.6931471805599453 == fisher_information_e [50, 50]
#guard 0.3250829733914482 == fisher_information_e [90, 10]
#guard 0.3250829733914482 == fisher_information_e [10, 90]
#guard 0.056001534354847345 == fisher_information_e [99, 1]
#guard 0.056001534354847345 == fisher_information_e [1, 99]

def fisher_information_2 (occ : List Nat) : Float :=
    fisher_information occ "2"
#guard 1. == fisher_information_2 [1, 1]
#guard 1. == fisher_information_2 [50, 50]
#guard 0.4689955935892812 == fisher_information_2 [90, 10]
#guard 0.4689955935892812 == fisher_information_2 [10, 90]
#guard 0.08079313589591118 == fisher_information_2 [99, 1]
#guard 0.08079313589591118 == fisher_information_2 [1, 99]

def fisher_information_10 (occ : List Nat) : Float :=
    fisher_information occ "10"
#guard 0.3010299956639812 == fisher_information_10 [1, 1]
#guard 0.3010299956639812 == fisher_information_10 [50, 50]
#guard 0.1411817415046076 == fisher_information_10 [90, 10]
#guard 0.1411817415046076 == fisher_information_10 [10, 90]
#guard 0.024321157348425586 == fisher_information_10 [99, 1]
#guard 0.024321157348425586 == fisher_information_10 [1, 99]



def calc_fisher_information (m : List $ List Nat) (window_wh : Nat × Nat) : Float :=
    let occ := calc_occ m window_wh |>.toList |>.map Prod.snd
    fisher_information_2 occ


/- def seconds_max : Nat := 10 -/
/- def seconds_max : Nat := 100 -/
/- def seconds_max : Nat := 1000 -/
def seconds_max : Nat := 10403


partial def ffff (seconds : Nat) (robots : List Robot) (wh : Nat × Nat) (window_wh : Nat × Nat) : List Float :=
    if seconds > seconds_max then [] else
    let (w, h) := wh
    if w % 2 != 1 || h % 2 != 1 then [] else
    let final_positions := robots |> get_robots_positions_after_n_seconds seconds wh
    let m := positions_to_map final_positions wh
    calc_fisher_information m window_wh :: ffff (seconds+1) robots wh window_wh


def solve_part_two (input : String) (wh : Nat × Nat) : String :=
    let (w, h) := wh
    if w % 2 != 1 || h % 2 != 1 then "ERROR" else
    let robots := get_robots_initial_positions input
    /- fff 0 100 robots wh -/
    ffff 0 robots wh (2,2) |>.toString
    /- ffff 0 robots wh (3,3) |>.toString -/

/- #eval solve_part_two example_2 -/
/- #guard example_1_answer_part_two == solve_part_two example_1 -/

