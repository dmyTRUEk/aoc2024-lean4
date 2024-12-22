import aoc2024.mylib.mylib

import aoc2024.day22.examples
/- import aoc2024.day22.input -/



def parse_input (input : String) : Array Nat :=
    input
        |>.split_lines'
        |>.map String.toNat!



def evolve (s : Nat) : Nat :=
    let s := (s <<< 6).xor s |>.mod 16777216 -- | (1 <<< 24)
    let s := (s >>> 5).xor s |>.mod 16777216
    let s := (s <<< 11).xor s |>.mod 16777216
    s



def solve_part_one (input : String) : OutputTypePartOne :=
    input
        |> parse_input
        |>.map (apply_n 2000 evolve)
        |>.sum

#eval solve_part_one example_1
#guard example_1_answer_part_one == solve_part_one example_1



def evolve_seq (n : Nat) (s : Nat) : Array Nat :=
    if n <= 0 then
        #[]
    else
        #[s] ++ evolve_seq (n-1) (evolve s)

#eval evolve_seq 10 123
#guard evolve_seq 10 123 == #[123, 15887950, 16495136, 527345, 704524, 1553684, 12683156, 11100544, 12249484, 7753432]
#eval evolve_seq 10 123 |>.size



def deltas (ss : Array Nat) : Array Int :=
    ss
        |>.map (. % 10)
        |>.windows 2
        |>.map (fun ab =>
            let a := ab[0]!.to_int
            let b := ab[1]!.to_int
            b - a
        )

#eval deltas $ evolve_seq 10 123
#guard deltas (evolve_seq 10 123) == #[-3, 6, -1, -1, 0, 2, -2, 0, -2]



def bananas_with_ds4 (ss : Array Nat) (ds : Array Int) (ds_4 : Int × Int × Int × Int) : Nat :=
    let (ds1, ds2, ds3, ds4) := ds_4
    let ds_4 := #[ds1, ds2, ds3, ds4]
    let index := ds.windows 4 |>.findIdx? (eq ds_4)
    index.map (ss[.]! % 10) |>.get!

#guard 6 == let ss := evolve_seq 10 123; let ds := deltas ss; bananas_with_ds4 ss ds (-1, -1, 0, 2)



def bananas_with_ds4' (sss : Array $ Array Nat) (dss : Array $ Array Int) (ds_4 : Int × Int × Int × Int) : Nat :=
    sss.zip dss
        |>.map (fun ss_ds =>
            let ss := ss_ds.1
            let ds := ss_ds.2
            bananas_with_ds4 ss ds ds_4
        )
        |>.sum


/- #eval let r := 3; let ds0 := List.int_range_ (-r) (r); ds0.tensor_product ds0 |>.tensor_product ds0 |>.tensor_product ds0 -/


def solve_part_two (input : String) : OutputTypePartTwo :=
    let sss := input
        |> parse_input
        |>.map (evolve_seq 2000)
    let dss := sss.map deltas
    let ds0 := List.int_range_ (-9) (9)
    ds0 |>.tensor_product ds0 |>.tensor_product ds0 |>.tensor_product ds0
        |>.map (fun abcd =>
            let (((a, b), c), d) := abcd
            dbg_trace "{a}\t{b}\t{c}\t{d}"
            bananas_with_ds4' sss dss (a, b, c, d)
        )
        |>.maximum?
        |>.get!

/- #eval solve_part_two example_2 -/
/- #guard example_2_answer_part_two == solve_part_two example_2 -/

