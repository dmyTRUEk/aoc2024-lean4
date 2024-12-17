--- list

import aoc2024.mylib.misc




/-- Quick sort.
* `sort [2,3,1] = [1,2,3]`
* `sort [8,3,1,8,0,3,2,1,2,7,9] = [0,1,1,2,2,3,3,7,8,8,9]`
-/
partial def List.sort [Ord T] : List T -> List T
    | [] => []
    | head :: tail =>
        let (l, r) := tail.partition (fun x => (compare x head).isLE)
        l.sort ++ [head] ++ r.sort

#guard [1,2,3] == [2,3,1].sort
#guard [0,1,1,2,2,3,3,7,8,8,9] == [8,3,1,8,0,3,2,1,2,7,9].sort



/-- Sum all elements of the list.
* `[1, 2, 3].sum = 6`
* `(List.range 11) = 55`
* `(List.range 101).sum = 5050`
-/
def List.sum [Add T] [OfNat T 0] : List T -> T
    | [] => 0
    | head :: tail => head + sum tail

#guard 6 == [1, 2, 3].sum
/- #guard 0.3 == sum [0.1, 0.2] -/
#eval [0.1, 0.2].sum
#guard 55 == (List.range 11).sum
#guard 5050 == (List.range 101).sum



/-- Flatten a list. -/
def List.flatten : List (List T) -> List T := List.join

#guard [1,2,3,4,5,6,7,8,9] == [[1,2,3], [4], [5,6], [7,8], [9]].flatten



/-- Split string into lines. -/
def String.split_lines : String -> List String :=
    (.|>.splitOn "\n")

#guard ["abc", "def", "ghi"] == "abc\ndef\nghi".split_lines



/-- Split string into chunks (empty line separated). -/
def String.split_chunks : String -> List String :=
    (.|>.splitOn "\n\n")

-- TODO(test)



/-- Enumerate list elements.
* `['a','b','c'].enumerate = [(0,'a'), (1,'b'), (2,'c')]`
-/
def List.enumerate : List T -> List (Nat × T) := List.enum

#guard [(0,'a'), (1,'b'), (2,'c')] == ['a','b','c'].enumerate



/-- Unique elements of the list.
* `[1,1,2,1,2,3,1,2,4,3,2,1].unique = [1,2,3,4]`
-/
def List.unique [BEq T] : Endomorphism $ List T :=
    /- (.|>.foldl (λ acc el ↦ if acc.contains el then acc else acc.concat el) []) -/
    List.eraseDups
    -- TODO(optimization): if list.length > 1000  ->  use `HashSet`?

#guard [1,2,3,4] == [1,1,2,1,2,3,1,2,4,3,2,1].unique



-- TODO(feat): List.unique_by



/-- Is all elements in list `as` are unique.
* `[1,2,3,4].all_unique = true`
* `[1,2,1,4].all_unique = false`
-/
def List.all_unique [BEq T] (as : List T) : Bool :=
    as == as.unique

#guard [1,2,3,4].all_unique
#guard ![1,1,2,1,2,3,1,2,4,3,2,1].all_unique
/- #check_failure [1,2,3,4] == [1,1,2,1,2,3,1,2,4,3,2,1].all_unique -/
/- #check_failure 2 3 -/



/-- Check if `index` is in a list `as` bounds.
* `['a','b','c'].contains_index 0 = true`
* `['a','b','c'].contains_index 1 = true`
* `['a','b','c'].contains_index 2 = true`
* `!['a','b','c'].contains_index 3 = false`
* `!['a','b','c'].contains_index 4 = false`
-/
def List.contains_index (index : Nat) (as : List T) : Bool :=
    index < as.length

#guard ['a','b','c'].contains_index 0
#guard ['a','b','c'].contains_index 1
#guard ['a','b','c'].contains_index 2
#guard !['a','b','c'].contains_index 3
#guard !['a','b','c'].contains_index 4



-- TODO(feat):
/- def join := List.intersperse -/
/- def List.dedup_cons := List.eraseReps -/
/- def List.imap := .|>.enum.map -/
/- def List.ifilter := .|>.enum.filter -/



/-- Numbers from `start` to `start+n` exclusive, in increasing order.
* `range_from 0 4 = [0,1,2,3]`
* `range_from 3 4 = [3,4,5,6]`
-/
def List.range_from (start count : Nat) : List Nat :=
    /- List.replicate count Unit.unit |>.enumFrom start |>.map Prod.fst -/
    List.range count |>.map (. + start)

#guard List.range 42 == List.range_from 0 42
#guard [0,1,2,3] == List.range_from 0 4
#guard [3,4,5,6] == List.range_from 3 4
#guard [] == List.range_from 3 0



/-- Numbers from `min` to `max` inclusive, in increasing order. UB if `min > max`
* `range_from 0 2 = [0,1,2]`
* `range_from 3 5 = [3,4,5]`
-/
def List.range_ (min max : Nat) : List Nat :=
    List.range (max + 1 - min) |>.map (. + min)

#guard [0,1,2] == List.range_ 0 2
#guard [3,4,5] == List.range_ 3 5
#guard [3] == List.range_ 3 3
#guard [] == List.range_ 3 2



/-- Sublist of a list `as` starting from index `start` with (max) length `len`.
* `['a','b','c','d','e','f'].sublist 1 3 = ['b','c','d']`
-/
def List.sublist (as : List T) (start len : Nat) : List T :=
    as.drop start |>.take len

#guard ['b','c','d'] == ['a','b','c','d','e','f'].sublist 1 3










/-- Join list of strings with separator.
* `["a", "b", "c"].join_ "-" = "a-b-c"`
-/
def List.join_ (sep : String) (as: List String) : String :=
    sep.intercalate as

#guard "a-b-c" == ["a", "b", "c"].join_ "-"


/-- Join list of chars with separator.
* `['a', 'b', 'c'].join_ "-" = "a-b-c"`
-/
def List.join_chars (sep : String) (ss: List Char) : String :=
    sep.intercalate $ ss.map (fun c => s!"{c}")

#guard "a-b-c" == ['a', 'b', 'c'].join_chars "-"



/-- Split list `as` at index `n`.
* `[1,2,3,4,5].split_at 2 = ([1,2], [3,4,5])`
-/
def List.split_at (n : Nat) (as : List T) : List T × List T :=
    (as.take n, as.drop n)

#guard ([1,2], [3,4,5]) == [1,2,3,4,5].split_at 2



/-- Get list `as` in chunks of lenght `n`. UB if `n = 0`.
* `[1,2,3,4,5,6].chunks 2 = [[1,2],[3,4],[5,6]]`
* `[1,2,3,4,5,6].chunks 3 = [[1,2,3],[4,5,6]]`
* `[1,2,3,4,5,6,7].chunks 2 = [[1,2],[3,4],[5,6]]`
* `[1,2,3,4,5,6,7].chunks 3 = [[1,2,3],[4,5,6]]`
-/
partial def List.chunks (n : Nat) (as : List T) : List $ List T :=
    if n == 0 || as.length < n then [] else
    let (h, t) := as.split_at n
    h :: t.chunks n

#guard [[1,2],[3,4],[5,6]] == [1,2,3,4,5,6].chunks 2
#guard [[1,2,3],[4,5,6]] == [1,2,3,4,5,6].chunks 3
#guard [[1,2],[3,4],[5,6]] == [1,2,3,4,5,6,7].chunks 2
#guard [[1,2,3],[4,5,6]] == [1,2,3,4,5,6,7].chunks 3



/-- View list `as` in sliding windows of lenght `n`.
* `[1,2,3,4,5].windows 2 = [[1,2],[2,3],[3,4],[4,5]]`
* `[1,2,3,4,5].windows 3 = [[1,2,3],[2,3,4],[3,4,5]]`
-/
partial def List.windows (n : Nat) (as : List T) : List $ List T :=
    if n == 0 || as.length < n then [] else
    as.take n :: (as.drop 1 |>.windows n)

#guard [[1,2],[2,3],[3,4],[4,5]] == [1,2,3,4,5].windows 2
#guard [[1,2,3],[2,3,4],[3,4,5]] == [1,2,3,4,5].windows 3



/-- Juxt -- map value `a` by functions `fs` = apply functions `fs` to value `a`.
* `[(. * 2), (. + 5), (. / 2)].juxt 10 = [20, 15, 5]`
-/
def List.juxt {A B : Type} (fs: List $ A -> B) (a : A) : List B :=
    fs.map (fun f => f a)

#guard [20, 15, 5] == [(. * 2), (. + 5), (. / 2)].juxt 10

