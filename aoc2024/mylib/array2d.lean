--- array2d

import aoc2024.mylib.array
import aoc2024.mylib.misc
import aoc2024.mylib.string
import aoc2024.mylib.vec2n



/-- Transpose `ass` as matrix transpose. UB if `ass` is not rectangular.
* `[[1,2],[3,4],[5,6]].transpose = [[1,3,5],[2,4,6]]`
* `[[1,3,5],[2,4,6]].transpose = [[1,2],[3,4],[5,6]]`
-/
def Array.transpose [Inhabited T] (ass : Array $ Array T) : Array $ Array T :=
    /- assert! true -- TODO: all have same length -/
    let ly := ass.size
    let lx := ass[0]!.size
    let ry := Array.range ly
    let rx := Array.range lx
    rx.map (fun x =>
        ry.map (fun y =>
            ass[y]![x]!
        )
    )

/- #guard transpose #[] == #[] -/
/- #guard transpose #[[]] == #[#[]] -/
/- /- #guard transpose #[#[],#[]] == #[#[]] -/ -/
/- /- #guard transpose #[#[],#[],#[]] == #[#[]] -/ -/
/- #eval transpose [[1,2],[3,4],[5,6]] -/
/- #eval transpose [[1,3,5],[2,4,6]] -/
#guard #[#[1,3,5],#[2,4,6]] == #[#[1,2],#[3,4],#[5,6]].transpose
#guard #[#[1,2],#[3,4],#[5,6]] == #[#[1,3,5],#[2,4,6]].transpose



/-- String to "matrix" (array of arrays) of characters.
* `"ab\ncd".to_array2d = #[#['a', 'b'], #['c', 'd']]`
-/
def String.to_array2d (s : String) : Array $ Array Char :=
    s.split_lines'.map (.|>.toList.toArray)

#guard #[#['a', 'b'], #['c', 'd']] == "ab\ncd".to_array2d



/-- Map array of arrays `ass` recursively by function `f`.
* `#[#[1,2],#[3,4]].map_rec (. * 10) = #[#[10,20],#[30,40]]`
-/
def Array.map_rec (f : A -> B) (ass : Array $ Array A) : Array $ Array B :=
    ass.map (.|>.map f)

#guard #[#[10,20],#[30,40]] == #[#[1,2],#[3,4]].map_rec (. * 10)



/-- Index of element for which `pred` evaluates to true.
* `#[#['a','b'],#['c','d']].index2d_of_first? (eq 'a') = some (0,0)`
* `#[#['a','b'],#['c','d']].index2d_of_first? (eq 'b') = some (0,1)`
* `#[#['a','b'],#['c','d']].index2d_of_first? (eq 'c') = some (1,0)`
* `#[#['a','b'],#['c','d']].index2d_of_first? (eq 'd') = some (1,1)`
* `#[#['a','b'],#['c','d']].index2d_of_first? (eq 'x') = none`
-/
def Array.index2d_of_first? [Inhabited T] (pred : T -> Bool) (ass : Array $ Array T) : Option $ Nat × Nat :=
    if ass.size = 0 then none else fff 0 0
    -- TODO(optim): try "enum_2d" |> flatten |> "find_index"
where
    fff := fun y x => -- TODO(refactor): rename `fff` -> ?
        /- dbg_trace "y={y} x={x}" -/
        if ass.contains_index y && ass[y]!.contains_index x && pred ass[y]![x]! then
            some (y, x)
        else
            if x + 1 < ass[y]!.size then
                fff y (x+1)
            else if y + 1 < ass.size then
                fff (y+1) 0
            else
                none

#guard some (0,0) == #[#['a','b'],#['c','d']].index2d_of_first? (eq 'a')
#guard some (0,1) == #[#['a','b'],#['c','d']].index2d_of_first? (eq 'b')
#guard some (1,0) == #[#['a','b'],#['c','d']].index2d_of_first? (eq 'c')
#guard some (1,1) == #[#['a','b'],#['c','d']].index2d_of_first? (eq 'd')
#guard none       == #[#['a','b'],#['c','d']].index2d_of_first? (eq 'x')
#guard some (2,1) == #[#['a','b'],#[],#['d','A']].index2d_of_first? (eq 'A')


/-- Index of element for which `pred` evaluates to true.
* `#[#['a','b'],#['c','d']].index2d_of_first! (eq 'a') = (0,0)`
* `#[#['a','b'],#['c','d']].index2d_of_first! (eq 'b') = (0,1)`
* `#[#['a','b'],#['c','d']].index2d_of_first! (eq 'c') = (1,0)`
* `#[#['a','b'],#['c','d']].index2d_of_first! (eq 'd') = (1,1)`
* `#[#['a','b'],#['c','d']].index2d_of_first! (eq 'x') = (0,0)`
-/
def Array.index2d_of_first! [Inhabited T] (pred : T -> Bool) (ass : Array $ Array T) : Nat × Nat :=
    ass.index2d_of_first? pred |>.get!

#guard (0,0) == #[#['a','b'],#['c','d']].index2d_of_first! (eq 'a')
#guard (0,1) == #[#['a','b'],#['c','d']].index2d_of_first! (eq 'b')
#guard (1,0) == #[#['a','b'],#['c','d']].index2d_of_first! (eq 'c')
#guard (1,1) == #[#['a','b'],#['c','d']].index2d_of_first! (eq 'd')
#guard (0,0) == #[#['a','b'],#['c','d']].index2d_of_first! (eq 'x')
#guard (2,1) == #[#['a','b'],#[],#['d','A']].index2d_of_first! (eq 'A')



/-- Sizes of a 2d array `ass`. UB if `ass` is not rectangular.
* `#[#['a','b'],#['c','d'],#['e','f']].sizes = (3, 2)`
-/
def Array.sizes (ass : Array $ Array T) : Nat × Nat :=
    -- TODO: assert is rectangular
    (ass.size, ass[0]!.size)

#guard (3, 2) == #[#['a','b'],#['c','d'],#['e','f']].sizes



/-- Set element at indices `y` `x` to `new_value`. UB if indices out of bounds.
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=0, y:=0 : Vec2n} 'x' = #[#['x','b'],#['c','d'],#['e','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=1, y:=0 : Vec2n} 'x' = #[#['a','x'],#['c','d'],#['e','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=0, y:=1 : Vec2n} 'x' = #[#['a','b'],#['x','d'],#['e','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=1, y:=1 : Vec2n} 'x' = #[#['a','b'],#['c','x'],#['e','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=0, y:=2 : Vec2n} 'x' = #[#['a','b'],#['c','d'],#['x','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=1, y:=2 : Vec2n} 'x' = #[#['a','b'],#['c','d'],#['e','x']]`
-/
def Array.set2d_v (ass : Array $ Array T) (v : Vec2n) (new_value : T) : Array $ Array T :=
    ass.set! v.y (ass[v.y]!.set! v.x new_value)

#guard #[#['x','b'],#['c','d'],#['e','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=0, y:=0 : Vec2n} 'x'
#guard #[#['a','x'],#['c','d'],#['e','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=1, y:=0 : Vec2n} 'x'
#guard #[#['a','b'],#['x','d'],#['e','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=0, y:=1 : Vec2n} 'x'
#guard #[#['a','b'],#['c','x'],#['e','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=1, y:=1 : Vec2n} 'x'
#guard #[#['a','b'],#['c','d'],#['x','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=0, y:=2 : Vec2n} 'x'
#guard #[#['a','b'],#['c','d'],#['e','x']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_v {x:=1, y:=2 : Vec2n} 'x'


/-- Set element at indices `y` `x` to `new_value`. UB if indices out of bounds.
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_yx 0 0 'N' = #[#['N','b'],#['c','d'],#['e','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_yx 0 1 'N' = #[#['a','N'],#['c','d'],#['e','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_yx 1 0 'N' = #[#['a','b'],#['N','d'],#['e','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_yx 1 1 'N' = #[#['a','b'],#['c','N'],#['e','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_yx 2 0 'N' = #[#['a','b'],#['c','d'],#['N','f']]`
* `#[#['a','b'],#['c','d'],#['e','f']].set2d_yx 2 1 'N' = #[#['a','b'],#['c','d'],#['e','N']]`
-/
def Array.set2d_yx (ass : Array $ Array T) (yx : Nat × Nat) (new_value : T) : Array $ Array T :=
    ass.set2d_v (Vec2n.from_prod_yx yx) new_value

#guard #[#['x','b'],#['c','d'],#['e','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_yx (0, 0) 'x'
#guard #[#['a','x'],#['c','d'],#['e','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_yx (0, 1) 'x'
#guard #[#['a','b'],#['x','d'],#['e','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_yx (1, 0) 'x'
#guard #[#['a','b'],#['c','x'],#['e','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_yx (1, 1) 'x'
#guard #[#['a','b'],#['c','d'],#['x','f']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_yx (2, 0) 'x'
#guard #[#['a','b'],#['c','d'],#['e','x']] == #[#['a','b'],#['c','d'],#['e','f']].set2d_yx (2, 1) 'x'



/- TODO -/
/- def Array.get2d_yx? (ass : Array $ Array T) (yx : Nat × Nat) : Option T := -/
/-     let (y, x) := yx -/
/-     ass[y]?.map (fun r => r[x]?) |>.flatten -/


/- TODO -/
/- def Array.get2d_yx! [Inhabited T] (ass : Array $ Array T) (y x : Nat) : T := -/
/-     ass[y]![x]! -/


/-- Get element with indices given by `Vec2n` `v`. UB if out of bounds.
* `#[#['a','b'],#['c','d']].get2d_v! { x:=0, y:=0 : Vec2n } ='a'`
* `#[#['a','b'],#['c','d']].get2d_v! { x:=1, y:=0 : Vec2n } ='b'`
* `#[#['a','b'],#['c','d']].get2d_v! { x:=0, y:=1 : Vec2n } ='c'`
* `#[#['a','b'],#['c','d']].get2d_v! { x:=1, y:=1 : Vec2n } ='d'`
-/
def Array.get2d_v! [Inhabited T] (ass : Array $ Array T) (v : Vec2n) : T :=
    let { x, y } := v
    ass[y]![x]!

#guard 'a' == #[#['a','b'],#['c','d']].get2d_v! { x:=0, y:=0 : Vec2n }
#guard 'b' == #[#['a','b'],#['c','d']].get2d_v! { x:=1, y:=0 : Vec2n }
#guard 'c' == #[#['a','b'],#['c','d']].get2d_v! { x:=0, y:=1 : Vec2n }
#guard 'd' == #[#['a','b'],#['c','d']].get2d_v! { x:=1, y:=1 : Vec2n }



/-- View `ass` in sliding windows of width×height = `wh`.
* `#[ #[1,2,3], #[4,5,6], #[7,8,9] ].windows2d (2, 2) = #[#[#[1,2],#[4,5]], #[#[2,3],#[5,6]], #[#[4,5],#[7,8]], #[#[5,6],#[8,9]]]`
-/
def Array.windows2d (wh : Nat × Nat) (ass : Array $ Array T) : Array $ Array $ Array T :=
    let (w, h) := wh
    if w == 0 || h == 0 then #[] else
    ass.windows h
        |>.map (fun rows =>
            let rows_windows := rows.map $ Array.windows w
            rows_windows.transpose
        )
        |>.flatten

#guard #[#[#[1,2],#[4,5]], #[#[2,3],#[5,6]], #[#[4,5],#[7,8]], #[#[5,6],#[8,9]]] == #[ #[1,2,3], #[4,5,6], #[7,8,9] ].windows2d (2, 2)



/-- Map of characters to string.
* `#[#['a','b','c'],#['d','e','f'],#['g','h','i']].as_map_to_string = "a b c\nd e f\ng h i"`
-/
def Array.as_map_to_string (ass : Array $ Array Char) : String :=
    ass
        |>.map (fun l => l.join_chars' " ")
        |>.join_ "\n"

#guard "a b c\nd e f\ng h i" == #[#['a','b','c'],#['d','e','f'],#['g','h','i']].as_map_to_string



/- def Array.modify2d_yx (ass : Array $ Array T) (yx : Nat × Nat) (f : T -> T) : Array $ Array T := -/
/-     let (y, x) := yx -/
/-     sorry -/

