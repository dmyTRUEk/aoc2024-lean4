import aoc2024.mylib

import aoc2024.day13.example
import aoc2024.day13.input



def Vec2n.xyp_from_string (s : String) : Vec2n :=
    Vec2n.from_string! s "Button A: X+" ", Y+"

#eval Vec2n.xyp_from_string "Button A: X+94, Y+34"

def Vec2n.xye_from_string (s : String) : Vec2n :=
    Vec2n.from_string! s "Prize: X=" ", Y="

#eval Vec2n.xye_from_string "Prize: X=8400, Y=5400"



structure ClawMachine where
    a : Vec2n
    b : Vec2n
    prize : Vec2n
deriving Repr, BEq

def ClawMachine.toString (cm : ClawMachine) : String :=
    let { a, b, prize } := cm
    s!"ClawMachine = \{a=[{a.x} {a.y}], b=[{b.x} {b.y}], prize=[{prize.x}, {prize.y}]}"



structure CMSolution where
    a : Nat
    b : Nat
deriving Repr, BEq

def CMSolution.toString (cms : CMSolution) : String :=
    s!"CMSolution = \{a = {cms.a}, b = {cms.b}}"

def CMSolution.cost (cms : CMSolution) :=
    cms.a * 3 + cms.b * 1



def parse_input (input : String) : List ClawMachine :=
    input.split_chunks
        |>.map String.split_lines
        |>.filterMap (fun lines =>
            if let [a_str, b_str, p_str] := lines then
            some {
                a := Vec2n.xyp_from_string a_str
                b := Vec2n.xyp_from_string b_str
                prize := Vec2n.xye_from_string p_str
            }
            else none
        )



def find_solution (cm : ClawMachine) : Option CMSolution :=
    let { a, b, prize } := cm
    let a'_numer : Int := prize.x*b.y - prize.y*b.x
    let b'_numer : Int := prize.y*a.x - prize.x*a.y
    let denom    : Int := a.x*b.y - a.y*b.x
    if denom != 0 && a'_numer % denom == 0 && b'_numer % denom == 0 then
        let a' := a'_numer / denom
        let b' := b'_numer / denom
        if let [some a', some b'] := [a', b'].map Int.toNat' then
            some { a := a', b := b' }
        else
            none
    else
        none

#eval find_solution { a:={x:=94,y:=34}, b:={x:=22,y:=67}, prize:={x:=8400,y:=5400} }
#guard some { a := 80, b := 40 } == find_solution { a:={x:=94,y:=34}, b:={x:=22,y:=67}, prize:={x:=8400,y:=5400} }

#eval find_solution { a:={x:=26,y:=66}, b:={x:=67,y:=21}, prize:={x:=12748,y:=12176} }
#guard none == find_solution { a:={x:=26,y:=66}, b:={x:=67,y:=21}, prize:={x:=12748,y:=12176} }

#eval find_solution { a:={x:=17,y:=86}, b:={x:=84,y:=37}, prize:={x:=7870,y:=6450} }
#guard some { a := 38, b := 86 } == find_solution { a:={x:=17,y:=86}, b:={x:=84,y:=37}, prize:={x:=7870,y:=6450} }

#eval find_solution { a:={x:=69,y:=23}, b:={x:=27,y:=71}, prize:={x:=18641,y:=10279} }
#guard none == find_solution { a:={x:=69,y:=23}, b:={x:=27,y:=71}, prize:={x:=18641,y:=10279} }



def solve_part_one (input : String) : Nat :=
    input
        |> parse_input
        |>.filterMap find_solution
        |>.map CMSolution.cost
        |>.sum

#eval solve_part_one example_1
#guard example_1_answer == solve_part_one example_1



def solve_part_two (input : String) : Nat :=
    input
        |> parse_input
        |>.map (fun cm => { cm with prize := cm.prize.add_to_xy $ 10^13 })
        |>.filterMap find_solution
        |>.map CMSolution.cost
        |>.sum

#eval solve_part_two example_1
/- #guard example_1_answer_part_two == solve_part_two example_1 -/

