-- mylib

-- TODO
/- def todo := sorry -/

/- def partition_n (l : List T) : List (List T) := -/
/-     sorry -/



def Int.abs (x : Int) : Int :=
    if x >= 0 then x else -x

#guard ( 42 : Int).abs == 42
#guard (-42 : Int).abs == 42



def transpose (list2d : List (List T)) [Inhabited T] : List (List T) :=
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
#guard transpose [[1,2],[3,4],[5,6]] == [[1,3,5],[2,4,6]]
#guard transpose [[1,3,5],[2,4,6]] == [[1,2],[3,4],[5,6]]



partial def sort [Ord T] : List T -> List T
    | [] => []
    | head :: tail =>
        let (l, r) := tail.partition (fun x => (compare x head).isLE)
        sort l ++ [head] ++ sort r

#guard [1,2,3] == sort [3,2,1]
#guard [0,1,1,2,2,3,3,7,8,8,9] == sort [8,3,1,8,0,3,2,1,2,7,9]



def sum [Add T] [OfNat T 0] : List T -> T
    | [] => 0
    | head :: tail => head + sum tail

#guard 6 == sum [1, 2, 3]
/- #guard 0.3 == sum [0.1, 0.2] -/
#eval sum [0.1, 0.2]
#guard 5050 == (sum $ List.range 101)
#guard 5050 == (sum (List.range 101))



def eq [BEq T] (x y : T) : Bool := x == y
def ne [BEq T] (x y : T) : Bool := x != y
def lt [Ord T] (x y : T) : Bool := (compare x y).isLT
def gt [Ord T] (x y : T) : Bool := (compare x y).isGT
def le [Ord T] (x y : T) : Bool := (compare x y).isLE
def ge [Ord T] (x y : T) : Bool := (compare x y).isGE



def apply_n (n : Nat) (f : T -> T) (arg : T) : T :=
    if n <= 0 then arg else apply_n (n-1) f (f arg)

#guard apply_n 0 (. * 2) 1 == 1
#guard apply_n 1 (. * 2) 1 == 2
#guard apply_n 2 (. * 2) 1 == 4
#guard apply_n 8 (. * 2) 1 == 256



def List.flatten : List (List T) -> List T := List.join

#guard [1,2,3,4,5,6,7,8,9] == [[1,2,3], [4], [5,6], [7,8], [9]].flatten

