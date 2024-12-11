import aoc2024.mylib
import aoc2024.day11.example
import aoc2024.day11.input

/- set_option diagnostics true -/



def Int.length10 (n : Int) : Nat :=
    s!"{n}".length -- TODO: optimize?

def is_even_number_of_digits (n : Int) : Bool :=
    n.length10 % 2 == 0

def left_digits (n : Int) : Int :=
    n / 10^(n.length10 / 2)

#eval left_digits 12
#eval left_digits 1234
#eval left_digits 123456
#eval left_digits 12345678
#guard 1 == left_digits 12
#guard 12 == left_digits 1234
#guard 123 == left_digits 123456
#guard 1234 == left_digits 12345678

def right_digits (n : Int) : Int :=
    n % 10^(n.length10 / 2)

#eval right_digits 12
#eval right_digits 1234
#eval right_digits 123456
#eval right_digits 12345678
#guard 2 == right_digits 12
#guard 34 == right_digits 1234
#guard 456 == right_digits 123456
#guard 5678 == right_digits 12345678


def parse_input (input : String) : List Int :=
    input
        |>.split (eq ' ')
        |>.map String.toInt!


def blink (stones : List Int) : List Int :=
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


def blink_n (n : Nat) (stones : List Int) : List Int :=
    apply_n n blink stones


def solve (input : String) :=
    input
        |> parse_input
        |> blink_n 25
        |>.length


#eval solve example_
#guard solve example_ == 55312


#eval blink_n 0 $ parse_input example_
#eval blink_n 1 $ parse_input example_
#eval blink_n 2 $ parse_input example_
#eval blink_n 3 $ parse_input example_
#eval blink_n 4 $ parse_input example_
/- #eval blink_n 5 $ parse_input example_ -/
/- #eval blink_n 6 $ parse_input example_ -/
/- #eval blink_n 7 $ parse_input example_ -/
/- #eval blink_n 8 $ parse_input example_ -/
/- #eval blink_n 9 $ parse_input example_ -/
/- #eval blink_n 10 $ parse_input example_ -/
/- #eval blink_n 11 $ parse_input example_ -/
/- #eval blink_n 12 $ parse_input example_ -/
/- #eval blink_n 13 $ parse_input example_ -/
/- #eval blink_n 14 $ parse_input example_ -/
/- #eval blink_n 15 $ parse_input example_ -/
/- #eval blink_n 16 $ parse_input example_ -/
/- #eval blink_n 17 $ parse_input example_ -/
/- #eval blink_n 18 $ parse_input example_ -/
/- #eval blink_n 19 $ parse_input example_ -/
/- #eval blink_n 20 $ parse_input example_ -/
/- #eval blink_n 21 $ parse_input example_ -/
/- #eval blink_n 22 $ parse_input example_ -/
/- #eval blink_n 23 $ parse_input example_ -/
/- #eval blink_n 24 $ parse_input example_ -/
/- #eval blink_n 25 $ parse_input example_ -/

/- #eval solve input_ -/


/- #guard false -/
def main : IO Unit :=
  IO.println $ solve input_
