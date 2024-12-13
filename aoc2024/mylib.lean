-- mylib

-- TODO
/- def todo := sorry -/

/- def partition_n (l : List T) : List (List T) := -/
/-     sorry -/



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



/-- Map list of lists recursively.
* `[[1,2],[3,4]].map_rec (. * 10) == [[10,20],[30,40]]`
-/
def List.map_rec (f : A -> B) (list : List $ List A) : List $ List B :=
    list.map (.|>.map f)

#guard [[10,20],[30,40]] == [[1,2],[3,4]].map_rec (. * 10)

