import Lean.Data.HashMap

import aoc2024.mylib

import aoc2024.day11.example
import aoc2024.day11.input

/- set_option diagnostics true -/



def Nat.length10 (n : Nat) : Nat :=
    s!"{n}".length -- TODO: optimize?

def is_even_number_of_digits (n : Nat) : Bool :=
    n.length10 % 2 == 0

def left_digits (n : Nat) : Nat :=
    n / 10^(n.length10 / 2)

#eval left_digits 12
#eval left_digits 1234
#eval left_digits 123456
#eval left_digits 12345678
#guard 1 == left_digits 12
#guard 12 == left_digits 1234
#guard 123 == left_digits 123456
#guard 1234 == left_digits 12345678

def right_digits (n : Nat) : Nat :=
    n % 10^(n.length10 / 2)

#eval right_digits 12
#eval right_digits 1234
#eval right_digits 123456
#eval right_digits 12345678
#guard 2 == right_digits 12
#guard 34 == right_digits 1234
#guard 456 == right_digits 123456
#guard 5678 == right_digits 12345678


def parse_input (input : String) : List Nat :=
    input
        |>.split (eq ' ')
        |>.map String.toNat!


def blink (stones : List Nat) : List Nat :=
    stones
        |>.map (fun stone =>
            if stone == 0 then
                [1]
            else if is_even_number_of_digits stone then
                [left_digits stone, right_digits stone]
            else
                [stone * 2024]
        )
        |>.flatten

def blink_n (n : Nat) (stones : List Nat) : List Nat :=
    apply_n n blink stones



/- structure MyStruct where -/
/-     x : T -/
/-     y : T -/


abbrev Stone_x_N := Nat × Nat
-- TODO: make alias, so methods also can be used (`abbrev`?)
def Cache := Lean.HashMap Stone_x_N Nat


def blink_one_once (stone : Nat) : List Nat :=
    if stone == 0 then
        [1]
    else if is_even_number_of_digits stone then
        [left_digits stone, right_digits stone]
    else
        [stone * 2024]

#eval blink_one_once 0
#eval blink_one_once 1
#eval blink_one_once 2024
#guard [1] == blink_one_once 0
#guard [2024] == blink_one_once 1
#guard [20, 24] == blink_one_once 2024


-- returns number of stones after `n` blinks starting from "stone value = `stone`"
def blink_n'_' (cache : Cache) (n : Nat) (stone : Nat) : (Nat × Cache) :=
    /- (0, cache) -/
    /- dbg_trace "" -/
    /- dbg_trace "stone = {stone}" -/
    /- dbg_trace "n = {n}" -/
    /- dbg_trace "cache = {cache.toList}" -/
    if n <= 0 then
        (1, cache)
    else if let some res := cache.find? (stone, n) then
        /- dbg_trace "cache hit!" -/
        (res, cache)
    else
        let res := blink_one_once stone
        -- TODO(refactor): rewrite using fold
        let (res, cache) :=
            /- dbg_trace "before if: res = {res}" -/
            if res.length == 1 then
                /- dbg_trace "len = 1" -/
                blink_n'_' cache (n-1) res[0]!
            else if res.length == 2 then
                /- dbg_trace "len = 2" -/
                let (res1, cache) := blink_n'_' cache (n-1) res[0]!
                /- dbg_trace "cache[{res[1]!}] = {cache.find? (res[1]!)}" -/
                let (res2, cache) := blink_n'_' cache (n-1) res[1]!
                (res1+res2, cache)
            else
                /- dbg_trace "PANIC!!!!!!!!!!!!" -/
                (0, Lean.HashMap.empty) -- aka panic
        let cache := cache.insert (stone, n) res
        /- dbg_trace "cache before return: {cache.toList}" -/
        (res, cache)

def tmp := blink_n'_' Lean.HashMap.empty 3 0
#eval tmp.1
#eval tmp.2.toList

def Prod.map1 (p : α × β) (f : α → γ) : γ × β := (f p.fst, p.snd)
def Prod.map2 (p : α × β) (f : β → γ) : α × γ := (p.fst, f p.snd)

#eval (10, 11).map2 (. * 2)

#eval blink_n'_' Lean.HashMap.empty 0 0 |>.map2 Lean.HashMap.toList -- 0 => 1
#eval blink_n'_' Lean.HashMap.empty 1 0 |>.map2 Lean.HashMap.toList -- 1 => 1
#eval blink_n'_' Lean.HashMap.empty 2 0 |>.map2 Lean.HashMap.toList -- 2024 => 1
#eval blink_n'_' Lean.HashMap.empty 3 0 |>.map2 Lean.HashMap.toList -- 20 24 => 2
#eval blink_n'_' Lean.HashMap.empty 4 0 |>.map2 Lean.HashMap.toList -- 2 0 2 4 => 4
#eval blink_n'_' Lean.HashMap.empty 5 0 |>.map2 Lean.HashMap.toList -- 4048 1 4048 8096 => 4
#eval blink_n'_' Lean.HashMap.empty 6 0 |>.map2 Lean.HashMap.toList -- 40 48 2024 40 48 80 96 => 7
#eval blink_n'_' Lean.HashMap.empty 7 0 |>.map2 Lean.HashMap.toList -- 4 0 4 8 20 24 4 0 4 8 8 0 9 6 => 
#eval blink_n'_' Lean.HashMap.empty 8 0 |>.map2 Lean.HashMap.toList
#eval blink_n'_' Lean.HashMap.empty 9 0 |>.map2 Lean.HashMap.toList


def blink_n'_ (cache : Cache) (n : Nat) (stones : List Nat) : Nat :=
    match stones with
    | [] => 0
    | head :: tail =>
        let (k, cache) := blink_n'_' cache n head
        k + blink_n'_ cache n tail


def blink_n' (n : Nat) (stones : List Nat) : Nat :=
    /- blink_n n stones |>.length -/
    blink_n'_ Lean.HashMap.empty n stones


def solve_part1 (input : String) :=
    input
        |> parse_input
        |> blink_n' 25


#eval solve_part1 example_
#guard solve_part1 example_ == 55312


#eval parse_input example_ |> blink_n 0 |>.length
#eval parse_input example_ |> blink_n 1 |>.length
#eval parse_input example_ |> blink_n 2 |>.length
#eval parse_input example_ |> blink_n 3 |>.length
#eval parse_input example_ |> blink_n 4 |>.length
#eval parse_input example_ |> blink_n 5 |>.length
/- #eval parse_input example_ |> blink_n 6 |>.length -/
/- #eval parse_input example_ |> blink_n 7 |>.length -/
/- #eval parse_input example_ |> blink_n 8 |>.length -/
/- #eval parse_input example_ |> blink_n 9 |>.length -/
/- #eval parse_input example_ |> blink_n 10 |>.length -/
/- #eval parse_input example_ |> blink_n 11 |>.length -/
/- #eval parse_input example_ |> blink_n 12 |>.length -/
/- #eval parse_input example_ |> blink_n 13 |>.length -/
/- #eval parse_input example_ |> blink_n 14 |>.length -/
/- #eval parse_input example_ |> blink_n 15 |>.length -/
/- #eval parse_input example_ |> blink_n 16 |>.length -/
/- #eval parse_input example_ |> blink_n 17 |>.length -/
/- #eval parse_input example_ |> blink_n 18 |>.length -/
/- #eval parse_input example_ |> blink_n 19 |>.length -/
/- #eval parse_input example_ |> blink_n 20 |>.length -/
/- #eval parse_input example_ |> blink_n 21 |>.length -/
/- #eval parse_input example_ |> blink_n 22 |>.length -/
/- #eval parse_input example_ |> blink_n 23 |>.length -/
/- #eval parse_input example_ |> blink_n 24 |>.length -/
/- #eval parse_input example_ |> blink_n 25 |>.length -/


def solve_part2 (input : String) :=
    input
        |> parse_input
        |> blink_n' 75

