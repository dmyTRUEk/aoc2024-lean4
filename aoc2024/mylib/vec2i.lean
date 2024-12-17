--- vec2i

import aoc2024.mylib.int


-- TODO(refactor): unify Vec2n & Vec2i

/-- 2d vector of `Int`s. -/
structure Vec2i where
    x : Int
    y : Int
deriving Repr, BEq, Hashable, Ord



/-- Zero vector. -/
def Vec2i.zero : Vec2i := { x:=0, y:=0 : Vec2i }

#guard { x:=0, y:=0 : Vec2i } == Vec2i.zero



instance : Inhabited Vec2i where
    default := Vec2i.zero

#guard { x:=0, y:=0 : Vec2i } == (default : Vec2i)
#guard { x:=0, y:=0 : Vec2i } == default



instance : Add Vec2i where
    add a b := {
        x := a.x + b.x,
        y := a.y + b.y,
        : Vec2i
    }

#guard { x:=6, y:=8 : Vec2i } == { x:=2, y:=3 : Vec2i } + { x:=4, y:=5 : Vec2i }



/-- `Vec2i` from `Int × Int` as (x, y).
* `Vec2i.from_prod_xy (4, 5) = { x:=4, y:=5 : Vec2i }`
-/
def Vec2i.from_prod_xy (xy : Int × Int) : Vec2i := { x:=xy.1, y:=xy.2 }

#guard { x:=4, y:=5 : Vec2i } == Vec2i.from_prod_xy (4, 5)


/-- `Vec2i` from `Int × Int` as (y, x).
* `Vec2i.from_prod_yx (4, 5) = { x:=4, y:=5 : Vec2i }`
-/
def Vec2i.from_prod_yx (yx : Int × Int) : Vec2i := { x:=yx.2, y:=yx.1 }

#guard { x:=4, y:=5 : Vec2i } == Vec2i.from_prod_yx (5, 4)



/-- Parse from string.
* `Vec2i.from_string? "42,-145" = some { x:=42, y:=-145 : Vec2i }`
* `Vec2i.from_string? "X=42, Y=-145" "X=" ", Y=" = some { x:=42, y:=-145 : Vec2i }`
* `Vec2i.from_string? "abc" = none`
-/
def Vec2i.from_string? (s : String) (prefix_ : String := "") (sep : String := ",") : Option Vec2i :=
    if let [some x, some y] := s.drop prefix_.length |>.splitOn sep |>.map String.toInt? then
        some { x, y : Vec2i }
    else
        none

#guard some { x:=42, y:=-145 : Vec2i } == Vec2i.from_string? "42,-145"
#guard some { x:=42, y:=-145 : Vec2i } == Vec2i.from_string? "X=42, Y=-145" "X=" ", Y="
#guard (none : Option Vec2i) == Vec2i.from_string? "abc"


/-- Parse from string.
* `Vec2i.from_string! "42,-145" = { x:=42, y:=-145 : Vec2i }`
* `Vec2i.from_string! "X=42, Y=-145" "X=" ", Y=" = { x:=42, y:=-145 : Vec2i }`
* `Vec2i.from_string! "abc" = { x:=0, y:=0 : Vec2i }`
-/
def Vec2i.from_string! (s : String) (prefix_ : String := "") (sep : String := ",") : Vec2i :=
    Vec2i.from_string? s prefix_ sep |>.getD default

#guard { x:=42, y:=-145 : Vec2i } == Vec2i.from_string! "42,-145"
#guard { x:=42, y:=-145 : Vec2i } == Vec2i.from_string! "X=42, Y=-145" "X=" ", Y="
#guard { x:=0, y:=0 : Vec2i } == Vec2i.from_string! "abc"



/-- `Vec2i` to `Int × Int` as (y, x).
* `{ x:=4, y:=5 : Vec2i }.to_prod_yx = (5, 4)`
-/
def Vec2i.to_prod_yx (v : Vec2i) : Int × Int :=
    (v.y, v.x)

#guard (5, 4) == { x:=4, y:=5 : Vec2i }.to_prod_yx


/-- `Vec2i` to `Int × Int` as (x, y).
* `{ x:=4, y:=5 : Vec2i }.to_prod_xy = (4, 5)`
-/
def Vec2i.to_prod_xy (v : Vec2i) : Int × Int :=
    (v.x, v.y)

#guard (4, 5) == { x:=4, y:=5 : Vec2i }.to_prod_xy



/-- For debug only! -/
def Vec2i.to_string (v : Vec2i) : String :=
    s!"[{v.x} {v.y}]"



/-- Add `n` to both `x` & `y`.
* `{ x:=-4, y:=5 : Vec2i }.add_to_xy 10 = { x:=6, y:=15 }`
* `Vec2i.zero.add_to_xy 42 ={ x:=42, y:=42 }`
-/
def Vec2i.add_to_xy (v : Vec2i) (n : Int) : Vec2i :=
    { x := v.x + n, y := v.y + n }

#guard { x:=42, y:=42 : Vec2i } == Vec2i.zero.add_to_xy 42
#guard { x:=6, y:=15 : Vec2i } == { x:=-4, y:=5 : Vec2i }.add_to_xy 10



/-- L1 distance from `self` to `other`.
* `{ x:=-3, y:=4 : Vec2i }.l1_distance_to { x:=7, y:=-8 : Vec2i } = 22`
-/
def Vec2i.l1_distance_to (self other : Vec2i) : Nat :=
    self.x.distance_to other.x + self.y.distance_to other.y

#guard 22 == { x:=-3, y:=4 : Vec2i }.l1_distance_to { x:=7, y:=-8 : Vec2i }

