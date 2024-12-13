-- mylib

-- TODO
/- def todo := sorry -/

/- def partition_n (l : List T) : List (List T) := -/
/-     sorry -/



/-- Endomorphism - function whose domain = codomain (image). -/
def Endomorphism T := T -> T

-- TODO(test)



/-- Absolute value of the `Int`.
* `( 42 : Int).abs = 42`
* `(-42 : Int).abs = 42`
-/
def Int.abs (n : Int) : Int :=
    if n >= 0 then n else -n

#guard ( 42 : Int).abs == 42
#guard (-42 : Int).abs == 42



/-- Transpose `list2d` as matrix transpose. UB if `list2d` is not rectangular.
* `[[1,2],[3,4],[5,6]].transpose = [[1,3,5],[2,4,6]]`
* `[[1,3,5],[2,4,6]].transpose = [[1,2],[3,4],[5,6]]`
-/
def List.transpose (list2d : List $ List T) [Inhabited T] : List $ List T :=
    assert! true -- TODO: all have same length
    let ly := list2d.length
    let lx := list2d[0]!.length
    let ry := List.range ly
    let rx := List.range lx
    rx.map (fun x =>
        ry.map (fun y =>
            list2d[y]![x]!
        )
    )

/- #guard transpose [] == [] -/
/- #guard transpose [[]] == [[]] -/
/- /- #guard transpose [[],[]] == [[]] -/ -/
/- /- #guard transpose [[],[],[]] == [[]] -/ -/
/- #eval transpose [[1,2],[3,4],[5,6]] -/
/- #eval transpose [[1,3,5],[2,4,6]] -/
#guard [[1,3,5],[2,4,6]] == [[1,2],[3,4],[5,6]].transpose
#guard [[1,2],[3,4],[5,6]] == [[1,3,5],[2,4,6]].transpose



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



/-- Equals function. -/
def eq [BEq T] (x y : T) : Bool := x == y
/-- Not Equals function. -/
def ne [BEq T] (x y : T) : Bool := x != y
/-- Less Than function. -/
def lt [Ord T] (x y : T) : Bool := (compare x y).isLT
/-- Greater Than function. -/
def gt [Ord T] (x y : T) : Bool := (compare x y).isGT
/-- Less or Equal function. -/
def le [Ord T] (x y : T) : Bool := (compare x y).isLE
/-- Greater or Equal function. -/
def ge [Ord T] (x y : T) : Bool := (compare x y).isGE



/-- Apply `n` times function `f` to `arg`.
* `apply_n 0 (. * 2) 1 == 1`
* `apply_n 1 (. * 2) 1 == 2`
* `apply_n 2 (. * 2) 1 == 4`
* `apply_n 8 (. * 2) 1 == 256`
-/
def apply_n (n : Nat) (f : T -> T) (arg : T) : T :=
    if n <= 0 then arg else apply_n (n-1) f (f arg)

#guard apply_n 0 (. * 2) 1 == 1
#guard apply_n 1 (. * 2) 1 == 2
#guard apply_n 2 (. * 2) 1 == 4
#guard apply_n 8 (. * 2) 1 == 256



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



/-- String to "matrix" (list of lists) of characters. -/
def String.to_list2d (input : String) : List $ List Char :=
    input.split_lines.map String.toList

#guard [['a', 'b'], ['c', 'd']] == "ab\ncd".to_list2d



/-- Enumerate list elements.
* `['a','b','c'].enumerate == [(0,'a'), (1,'b'), (2,'c')]`
-/
def List.enumerate : List T -> List (Nat × T) := List.enum

#guard [(0,'a'), (1,'b'), (2,'c')] == ['a','b','c'].enumerate



/-- Unique elements of the list.
* `[1,1,2,1,2,3,1,2,4,3,2,1].unique == [1,2,3,4]`
-/
def List.unique [BEq T] : Endomorphism $ List T :=
    /- (.|>.foldl (λ acc el ↦ if acc.contains el then acc else acc.concat el) []) -/
    List.eraseDups
    -- TODO(optimization): if list.length > 1000  ->  use `HashSet`?

#guard [1,2,3,4] == [1,1,2,1,2,3,1,2,4,3,2,1].unique



-- TODO(feat): List.unique_by



/-- Is all elements in list are unique.
* `[1,2,3,4].all_unique = true`
* `[1,2,1,4].all_unique = false`
-/
def List.all_unique [BEq T] (list : List T) : Bool :=
    list == list.unique

#guard [1,2,3,4].all_unique
#guard ![1,1,2,1,2,3,1,2,4,3,2,1].all_unique
/- #check_failure [1,2,3,4] == [1,1,2,1,2,3,1,2,4,3,2,1].all_unique -/
/- #check_failure 2 3 -/



/-- Map list of lists recursively.
* `[[1,2],[3,4]].map_rec (. * 10) == [[10,20],[30,40]]`
-/
def List.map_rec (f : A -> B) (list : List $ List A) : List $ List B :=
    list.map (.|>.map f)

#guard [[10,20],[30,40]] == [[1,2],[3,4]].map_rec (. * 10)



/-- Constant function.
* `(const_fun true) 0     == true`
* `(const_fun true) 10    == true`
* `(const_fun true) "abc" == true`
* `(const_fun true) "def" == true`
* `(const_fun true) true  == true`
* `(const_fun true) false == true`

* `(const_fun 42) 0     == 42`
* `(const_fun 42) 10    == 42`
* `(const_fun 42) "abc" == 42`
* `(const_fun 42) "def" == 42`
* `(const_fun 42) true  == 42`
* `(const_fun 42) false == 42`

* `(const_fun "abc") 0     == "abc"`
* `(const_fun "abc") 10    == "abc"`
* `(const_fun "abc") "abc" == "abc"`
* `(const_fun "abc") "def" == "abc"`
* `(const_fun "abc") true  == "abc"`
* `(const_fun "abc") false == "abc"`
-/
def const_fun : B -> (A -> B) := (fun _ => .)

#guard true == (const_fun true) 0
#guard true == (const_fun true) 10
#guard true == (const_fun true) "abc"
#guard true == (const_fun true) "def"
#guard true == (const_fun true) true
#guard true == (const_fun true) false
#guard 42 == (const_fun 42) 0
#guard 42 == (const_fun 42) 10
#guard 42 == (const_fun 42) "abc"
#guard 42 == (const_fun 42) "def"
#guard 42 == (const_fun 42) true
#guard 42 == (const_fun 42) false
#guard "abc" == (const_fun "abc") 0
#guard "abc" == (const_fun "abc") 10
#guard "abc" == (const_fun "abc") "abc"
#guard "abc" == (const_fun "abc") "def"
#guard "abc" == (const_fun "abc") true
#guard "abc" == (const_fun "abc") false



-- TODO(feat): "anti"-`const_fun` -- `const_fun` always return first argument. "anti"-`const_fun` should always return second argument.



/-- Check if `index` is in `list` bounds.
* `['a','b','c'].contains_index 0 = true`
* `['a','b','c'].contains_index 1 = true`
* `['a','b','c'].contains_index 2 = true`
* `!['a','b','c'].contains_index 3 = false`
* `!['a','b','c'].contains_index 4 = false`
-/
def List.contains_index (index : Nat) (list : List T) : Bool :=
    index < list.length

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



/-- Index of element for which `pred` evaluates to true.
* `[['a','b'],['c','d']].index_of_first_in_2d? (eq 'a') == some (0,0)`
* `[['a','b'],['c','d']].index_of_first_in_2d? (eq 'b') == some (0,1)`
* `[['a','b'],['c','d']].index_of_first_in_2d? (eq 'c') == some (1,0)`
* `[['a','b'],['c','d']].index_of_first_in_2d? (eq 'd') == some (1,1)`
* `[['a','b'],['c','d']].index_of_first_in_2d? (eq 'x') == none`
-/
def List.index_of_first_in_2d? [Inhabited T] (pred : T -> Bool) (list : List $ List T) : Option (Nat × Nat) :=
    if list.length = 0 then none else fff 0 0
    -- TODO(optim): try "enum_2d" |> flatten |> "find_index"
where
    fff := fun y x => -- TODO(refactor): rename `fff` -> ?
        /- dbg_trace "y={y} x={x}" -/
        if list.contains_index y && list[y]!.contains_index x && pred list[y]![x]! then
            some (y, x)
        else
            if x + 1 < list[y]!.length then
                fff y (x+1)
            else if y + 1 < list.length then
                fff (y+1) 0
            else
                none

#guard some (0,0) == [['a','b'],['c','d']].index_of_first_in_2d? (eq 'a')
#guard some (0,1) == [['a','b'],['c','d']].index_of_first_in_2d? (eq 'b')
#guard some (1,0) == [['a','b'],['c','d']].index_of_first_in_2d? (eq 'c')
#guard some (1,1) == [['a','b'],['c','d']].index_of_first_in_2d? (eq 'd')
#guard none       == [['a','b'],['c','d']].index_of_first_in_2d? (eq 'x')
#guard some (2,1) == [['a','b'],[],['d','A']].index_of_first_in_2d? (eq 'A')



/-- Numbers from `start` to `start+n` exclusive, in increasing order.
* `range_from 0 4 == [0,1,2,3]`
* `range_from 3 4 == [3,4,5,6]`
-/
def range_from (start count : Nat) : List Nat :=
    /- List.replicate count Unit.unit |>.enumFrom start |>.map Prod.fst -/
    List.range count |>.map (. + start)

#guard List.range 42 == range_from 0 42
#guard [0,1,2,3] == range_from 0 4
#guard [3,4,5,6] == range_from 3 4
#guard [] == range_from 3 0



/-- Numbers from `min` to `max` inclusive, in increasing order. UB if `min` > `max`
* `range_from 0 2 == [0,1,2]`
* `range_from 3 5 == [3,4,5]`
-/
def range_ (min max : Nat) : List Nat :=
    List.range (max + 1 - min) |>.map (. + min)

#guard [0,1,2] == range_ 0 2
#guard [3,4,5] == range_ 3 5
#guard [3] == range_ 3 3
#guard [] == range_ 3 2

