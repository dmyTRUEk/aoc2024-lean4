--- array

import aoc2024.mylib.list
import aoc2024.mylib.misc



/-- Get heads and tail of the array.
* `#[1,2,3,4].heads_tail = (#[1,2,3], 4)`
-/
-- TODO(feat,optimization): dont require `Inhabited`.
def Array.heads_tail [Inhabited T] (as : Array T) : (Array T × T) :=
    let heads := as.pop
    let tail  := as.back
    (heads, tail)

#guard (#[1,2,3], 4) == #[1,2,3,4].heads_tail



/-- Quick sort.
* `sort #[2,3,1] = #[1,2,3]`
* `sort #[8,3,1,8,0,3,2,1,2,7,9] = #[0,1,1,2,2,3,3,7,8,8,9]`
-/
partial def Array.sort [Inhabited T] [Ord T] : Array T -> Array T
    | #[] => #[]
    | as =>
        let (heads, tail) := as.heads_tail
        let (l, r) := heads.partition (fun x => (compare x tail).isLE)
        l.sort ++ #[tail] ++ r.sort

#guard #[1,2,3] == #[2,3,1].sort
#guard #[0,1,1,2,2,3,3,7,8,8,9] == #[8,3,1,8,0,3,2,1,2,7,9].sort



/-- Sum all elements of the array.
* `#[1, 2, 3].sum = 6`
* `(Array.range 11) = 55`
* `(Array.range 101).sum = 5050`
-/
partial def Array.sum [Add T] [OfNat T 0] [Inhabited T] : Array T -> T
    | #[] => 0
    | as =>
        let (heads, tail) := as.heads_tail
        tail + heads.sum

#guard 6 == #[1, 2, 3].sum
/- #guard 0.3 == sum #[0.1, 0.2] -/
#eval #[0.1, 0.2].sum
#guard 55 == (Array.range 11).sum
#guard 5050 == (Array.range 101).sum



-- /-- Flatten an array. -/
-- def Array.flatten : Array (Array T) -> Array T := Array.join
-- 
-- #guard #[1,2,3,4,5,6,7,8,9] == #[#[1,2,3], #[4], #[5,6], #[7,8], #[9]].flatten



/-- Split string into lines. -/
def String.split_lines' (s : String) : Array String :=
    s.splitOn "\n" |>.toArray

#guard #["abc", "def", "ghi"] == "abc\ndef\nghi".split_lines'



/-- Split string into chunks (empty line separated). -/
def String.split_chunks' (s : String) : Array String :=
    (s.splitOn "\n\n").toArray

-- TODO(test)



-- /-- Enumerate array elements.
-- * `#['a','b','c'].enumerate = #[(0,'a'), (1,'b'), (2,'c')]`
-- -/
-- def Array.enumerate : Array T -> Array (Nat × T) := Array.enum
-- 
-- #guard #[(0,'a'), (1,'b'), (2,'c')] == #['a','b','c'].enumerate



-- /-- Unique elements of the array.
-- * `#[1,1,2,1,2,3,1,2,4,3,2,1].unique = #[1,2,3,4]`
-- -/
-- def Array.unique [BEq T] : Endomorphism $ Array T :=
--     /- (.|>.foldl (λ acc el ↦ if acc.contains el then acc else acc.concat el) #[]) -/
--     Array.eraseDups
--     -- TODO(optimization): if array.length > 1000  ->  use `HashSet`?
-- 
-- #guard #[1,2,3,4] == #[1,1,2,1,2,3,1,2,4,3,2,1].unique



-- TODO(feat): Array.unique_by



-- /-- Is all elements in array are unique.
-- * `#[1,2,3,4].all_unique = true`
-- * `#[1,2,1,4].all_unique = false`
-- -/
-- def Array.all_unique [BEq T] (as : Array T) : Bool :=
--     as == as.unique
-- 
-- #guard #[1,2,3,4].all_unique
-- #guard !#[1,1,2,1,2,3,1,2,4,3,2,1].all_unique
-- /- #check_failure #[1,2,3,4] == #[1,1,2,1,2,3,1,2,4,3,2,1].all_unique -/
-- /- #check_failure 2 3 -/



/-- Check if `index` is in `as` bounds.
* `#['a','b','c'].contains_index 0 = true`
* `#['a','b','c'].contains_index 1 = true`
* `#['a','b','c'].contains_index 2 = true`
* `!#['a','b','c'].contains_index 3 = false`
* `!#['a','b','c'].contains_index 4 = false`
-/
def Array.contains_index (index : Nat) (as : Array T) : Bool :=
    index < as.size

#guard #['a','b','c'].contains_index 0
#guard #['a','b','c'].contains_index 1
#guard #['a','b','c'].contains_index 2
#guard !#['a','b','c'].contains_index 3
#guard !#['a','b','c'].contains_index 4



-- TODO(feat):
/- def join := Array.intersperse -/
/- def Array.dedup_cons := Array.eraseReps -/
/- def Array.imap := .|>.enum.map -/
/- def Array.ifilter := .|>.enum.filter -/



/-- Numbers from `start` to `start+n` exclusive, in increasing order.
* `range_from 0 4 = #[0,1,2,3]`
* `range_from 3 4 = #[3,4,5,6]`
-/
def Array.range_from (start count : Nat) : Array Nat :=
    /- Array.replicate count Unit.unit |>.enumFrom start |>.map Prod.fst -/
    Array.range count |>.map (. + start)

#guard Array.range 42 == Array.range_from 0 42
#guard #[0,1,2,3] == Array.range_from 0 4
#guard #[3,4,5,6] == Array.range_from 3 4
#guard #[] == Array.range_from 3 0



/-- Numbers from `min` to `max` inclusive, in increasing order. UB if `min > max`
* `range_from 0 2 = #[0,1,2]`
* `range_from 3 5 = #[3,4,5]`
-/
def Array.range_ (min max : Nat) : Array Nat :=
    Array.range (max + 1 - min) |>.map (. + min)

#guard #[0,1,2] == Array.range_ 0 2
#guard #[3,4,5] == Array.range_ 3 5
#guard #[3] == Array.range_ 3 3
#guard #[] == Array.range_ 3 2



/-- Subarray of a `as` starting from index `start` with (max) length `len`.
* `#['a','b','c','d','e','f'].subarray 1 3 = #['b','c','d']`
-/
def Array.subarray (as : Array T) (start len : Nat) : Array T :=
    as.extract start $ start+len

#guard #['b','c','d'] == #['a','b','c','d','e','f'].subarray 1 3



/-- Take `n` first elements of the `array`.
* `#[1,2,3,4,5].take 3 = #[1,2,3]`
-/
def Array.take (as : Array T) (n : Nat) : Array T :=
    as.subarray 0 n

#guard #[1,2,3] == #[1,2,3,4,5].take 3



/-- Drop `n` first elements of the `as`.
* `#[1,2,3].drop 1 = #[2,3]`
-/
def Array.drop (as : Array T) (n : Nat) : Array T :=
    as.subarray n as.size

#guard #[2,3] == #[1,2,3].drop 1



/-- Join array of strings with separator.
* `#["a", "b", "c"].join_ "-" = "a-b-c"`
-/
def Array.join_ (sep : String) (as: Array String) : String :=
    /- sep.intercalate as -- TODO -/
    as.toList.join_ sep

#guard "a-b-c" == #["a", "b", "c"].join_ "-"



/-- Join array of chars with separator.
* `#['a', 'b', 'c'].join_ "-" = "a-b-c"`
-/
def Array.join_chars' (sep : String) (as: Array Char) : String :=
    /- sep.intercalate $ as.map (fun c => s!"{c}") -/
    as.toList.join_chars sep

#guard "a-b-c" == #['a', 'b', 'c'].join_chars' "-"



/-- Split array `as` at index `n`.
* `#[1,2,3,4,5].split_at 2 = (#[1,2], #[3,4,5])`
-/
def Array.split_at (n : Nat) (as : Array T) : Array T × Array T :=
    (as.take n, as.drop n)

#guard (#[1,2], #[3,4,5]) == #[1,2,3,4,5].split_at 2



/-- Get array `as` in chunks of lenght `n`. UB if `n = 0`.
* `#[1,2,3,4,5,6].chunks 2 = #[#[1,2],#[3,4],#[5,6]]`
* `#[1,2,3,4,5,6].chunks 3 = #[#[1,2,3],#[4,5,6]]`
* `#[1,2,3,4,5,6,7].chunks 2 = #[#[1,2],#[3,4],#[5,6]]`
* `#[1,2,3,4,5,6,7].chunks 3 = #[#[1,2,3],#[4,5,6]]`
-/
partial def Array.chunks (n : Nat) (as : Array T) : Array $ Array T :=
    if n == 0 || as.size < n then #[] else
    let (h, t) := as.split_at n
    #[h].append $ t.chunks n

#guard #[#[1,2],#[3,4],#[5,6]] == #[1,2,3,4,5,6].chunks 2
#guard #[#[1,2,3],#[4,5,6]] == #[1,2,3,4,5,6].chunks 3
#guard #[#[1,2],#[3,4],#[5,6]] == #[1,2,3,4,5,6,7].chunks 2
#guard #[#[1,2,3],#[4,5,6]] == #[1,2,3,4,5,6,7].chunks 3



/-- View array `as` in sliding windows of lenght `n`.
* `#[1,2,3,4,5].windows 2 = #[#[1,2],#[2,3],#[3,4],#[4,5]]`
* `#[1,2,3,4,5].windows 3 = #[#[1,2,3],#[2,3,4],#[3,4,5]]`
-/
partial def Array.windows (n : Nat) (as : Array T) : Array $ Array T :=
    if n == 0 || as.size < n then #[] else
    #[as.take n].append (as.drop 1 |>.windows n)

#guard #[#[1,2],#[2,3],#[3,4],#[4,5]] == #[1,2,3,4,5].windows 2
#guard #[#[1,2,3],#[2,3,4],#[3,4,5]] == #[1,2,3,4,5].windows 3



/-- Juxt -- map value `a` by functions `fs` = apply functions `fs` to value `a`.
* `#[(. * 2), (. + 5), (. / 2)].juxt 10 = #[20, 15, 5]`
-/
def Array.juxt {A B : Type} (fs: Array $ A -> B) (a : A) : Array B :=
    fs.map (fun f => f a)

#guard #[20, 15, 5] == #[(. * 2), (. + 5), (. / 2)].juxt 10

