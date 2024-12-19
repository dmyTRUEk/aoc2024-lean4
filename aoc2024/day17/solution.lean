import aoc2024.mylib.mylib

import aoc2024.day17.examples
/- import aoc2024.day17.input -/



structure Registers where
    a : Nat
    b : Nat
    c : Nat
deriving Repr, BEq

def Registers.from_string (s : String) : Registers :=
    let abc := s.split_lines
    let a := abc[0]!.drop 12 |>.toNat!
    let b := abc[1]!.drop 12 |>.toNat!
    let c := abc[2]!.drop 12 |>.toNat!
    { a, b, c : Registers }



abbrev Program := Array Nat

def Program.from_string (s : String) : Program :=
    s.drop 9
        |>.splitOn ","
        |>.toArray
        |>.map String.toNat!

def Program.to_string (pr : Program) : String :=
    pr.map (fun n => s!"{n}") |>.join_ ","



def parse_input (input : String) : (Registers × Program) :=
    let rs_pr := input.split_chunks
    let rs := rs_pr[0]!
    let pr := rs_pr[1]!
    (Registers.from_string rs, Program.from_string pr)



partial def exe (pr : Program) (rs : Registers) (ip : Nat) (output : Array Nat) : String :=
    if let (some i, some o) := (pr.get? ip, pr.get? (ip+1)) then
        let lo := o
        let (co, err) := match o with
            | 0 | 1 | 2 | 3 => (o, none)
            | 4 => (rs.a, none)
            | 5 => (rs.b, none)
            | 6 => (rs.c, none)
            | _ => (0, some "COMBO OPERAND SKILL ISSUE")
        if let some err := err then err else
        let o := () -- clear `o` to avoid mistakes, use `lo` or `co` explicitly.
        match i with
        | 0 => -- adv
            /- let rs := { rs with a := rs.a / 2^co } -/
            let rs := { rs with a := rs.a >>> co }
            exe pr rs (ip+2) output
        | 1 => -- bxl
            let rs := { rs with b := rs.b.xor lo }
            exe pr rs (ip+2) output
        | 2 => -- bst
            let rs := { rs with b := co % 8 }
            exe pr rs (ip+2) output
        | 3 => -- jnz
            if rs.a == 0 then
                exe pr rs (ip+2) output
            else
                let ip := lo
                exe pr rs ip output
        | 4 => -- bxc
            let rs := { rs with b := rs.b.xor rs.c }
            exe pr rs (ip+2) output
        | 5 => -- out
            let res := co % 8
            let output := output.push res
            exe pr rs (ip+2) output
        | 6 => -- bdv
            let rs := { rs with b := rs.a >>> co }
            exe pr rs (ip+2) output
        | 7 => -- cdv
            let rs := { rs with c := rs.a >>> co }
            exe pr rs (ip+2) output
        | _ => "INSTRUCTION SKILL ISSUE"
    else
        output.map (fun n => s!"{n}") |>.join_ ","



def solve_part_one (input : String) : OutputTypePartOne :=
    let (rs, pr) := parse_input input
    /- dbg_trace rs.a -/
    /- dbg_trace rs.b -/
    /- dbg_trace rs.c -/
    /- dbg_trace pr.to_string -/
    exe pr rs 0 #[]

#eval solve_part_one example_1
#guard example_1_answer_part_one == solve_part_one example_1



partial def exe' (pr : Program) (rs : Registers) (ip : Nat) (output : Array Nat) : String :=
    if !output.isPrefixOf pr then "NO" else
    if let (some i, some o) := (pr.get? ip, pr.get? (ip+1)) then
        let lo := o
        let (co, err) := match o with
            | 0 | 1 | 2 | 3 => (o, none)
            | 4 => (rs.a, none)
            | 5 => (rs.b, none)
            | 6 => (rs.c, none)
            | _ => (0, some "COMBO OPERAND SKILL ISSUE")
        if let some err := err then err else
        let o := () -- clear `o` to avoid mistakes, use `lo` or `co` explicitly.
        match i with
        | 0 => -- adv
            /- let rs := { rs with a := rs.a / 2^co } -/
            let rs := { rs with a := rs.a >>> co }
            exe' pr rs (ip+2) output
        | 1 => -- bxl
            let rs := { rs with b := rs.b.xor lo }
            exe' pr rs (ip+2) output
        | 2 => -- bst
            let rs := { rs with b := co % 8 }
            exe' pr rs (ip+2) output
        | 3 => -- jnz
            if rs.a == 0 then
                exe' pr rs (ip+2) output
            else
                let ip := lo
                exe' pr rs ip output
        | 4 => -- bxc
            let rs := { rs with b := rs.b.xor rs.c }
            exe' pr rs (ip+2) output
        | 5 => -- out
            let res := co % 8
            let output := output.push res
            exe' pr rs (ip+2) output
        | 6 => -- bdv
            let rs := { rs with b := rs.a >>> co }
            exe' pr rs (ip+2) output
        | 7 => -- cdv
            let rs := { rs with c := rs.a >>> co }
            exe' pr rs (ip+2) output
        | _ => "INSTRUCTION SKILL ISSUE"
    else
        output.map (fun n => s!"{n}") |>.join_ ","


partial def exe_real_input (a : Nat) : Array Nat :=
    let b := a % 8
    let b := b.xor 1
    let c := a >>> b
    let b := b.xor c
    let a := a >>> 3
    let b := b.xor 6
    let b_mod_8 := b % 8
    if a == 0 then
        #[b_mod_8]
    else
        #[b_mod_8].append $ exe_real_input a


partial def find_min_reg_a (pr : Program) (a : Nat) : Nat :=
    dbg_trace a
    let res := exe_real_input a
    dbg_trace res
    if pr == res then
        a
    else
        find_min_reg_a pr $ a+1

def solve_part_two (input : String) : OutputTypePartTwo :=
    let (_rs, pr) := parse_input input
    find_min_reg_a pr 10000000000005

/- #eval solve_part_two example_1_part_two -/
/- #guard example_1_answer_part_two == solve_part_two example_1_part_two -/

#eval #[2,4,1,1,7,5,4,0,0,3,1,6,5,5,3,0].size
#eval exe #[2,4,1,1,7,5,4,0,0,3,1,6,5,5,3,0] { a:=35184500000000, b:=0, c:=0 } 0 #[]
#eval exe_real_input 10000000000000 |>.size
#eval exe_real_input 10000000000005
#eval exe_real_input  5000000000000

