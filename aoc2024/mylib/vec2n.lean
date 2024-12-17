--- vec2n

-- TODO(refactor): unify Vec2n & Vec2i

/-- 2d vector of `Nat`s. -/
structure Vec2n where
    x : Nat
    y : Nat
deriving Repr, BEq, Hashable, Ord


/-- Zero vector. -/
def Vec2n.zero : Vec2n := { x:=0, y:=0 : Vec2n }

#guard { x:=0, y:=0 : Vec2n } == Vec2n.zero


instance : Inhabited Vec2n where
    default := Vec2n.zero

#guard { x:=0, y:=0 : Vec2n } == (default : Vec2n)
#guard { x:=0, y:=0 : Vec2n } == default


instance : Add Vec2n where
    add a b := {
        x := a.x + b.x,
        y := a.y + b.y,
        : Vec2n
    }

#guard { x:=6, y:=8 : Vec2n } == { x:=2, y:=3 : Vec2n } + { x:=4, y:=5 : Vec2n }


/-- `Vec2n` from `Nat × Nat` as (x, y).
* `Vec2n.from_prod_xy (4, 5) = { x:=4, y:=5 : Vec2n }`
-/
def Vec2n.from_prod_xy (xy : Nat × Nat) : Vec2n := { x:=xy.1, y:=xy.2 }

#guard { x:=4, y:=5 : Vec2n } == Vec2n.from_prod_xy (4, 5)


/-- `Vec2n` from `Nat × Nat` as (y, x).
* `Vec2n.from_prod_yx (5, 4) = { x:=4, y:=5 : Vec2n }`
-/
def Vec2n.from_prod_yx (yx : Nat × Nat) : Vec2n := { x:=yx.2, y:=yx.1 }

#guard { x:=4, y:=5 : Vec2n } == Vec2n.from_prod_yx (5, 4)


/-- Parse from string.
* `Vec2n.from_string? "42,145" = some { x:=42, y:=145 : Vec2n }`
* `Vec2n.from_string? "X=42, Y=145" "X=" ", Y=" = some { x:=42, y:=145 : Vec2n }`
* `Vec2n.from_string? "abc" = none`
-/
def Vec2n.from_string? (s : String) (prefix_ : String := "") (sep : String := ",") : Option Vec2n :=
    if let [some x, some y] := s.drop prefix_.length |>.splitOn sep |>.map String.toNat? then
        some { x, y : Vec2n }
    else
        none

#guard some { x:=42, y:=145 : Vec2n } == Vec2n.from_string? "42,145"
#guard some { x:=42, y:=145 : Vec2n } == Vec2n.from_string? "X=42, Y=145" "X=" ", Y="
#guard (none : Option Vec2n) == Vec2n.from_string? "abc"


/-- Parse from string.
* `Vec2n.from_string! "42,145" = { x:=42, y:=145 : Vec2n }`
* `Vec2n.from_string! "X=42, Y=145" "X=" ", Y=" = { x:=42, y:=145 : Vec2n }`
* `Vec2n.from_string! "abc" = { x:=0, y:=0 : Vec2n }`
-/
def Vec2n.from_string! (s : String) (prefix_ : String := "") (sep : String := ",") : Vec2n :=
    Vec2n.from_string? s prefix_ sep |>.getD default

#guard { x:=42, y:=145 : Vec2n } == Vec2n.from_string! "42,145"
#guard { x:=42, y:=145 : Vec2n } == Vec2n.from_string! "X=42, Y=145" "X=" ", Y="
#guard { x:=0, y:=0 : Vec2n } == Vec2n.from_string! "abc"


/-- `Vec2n` to `Nat × Nat` as (y, x).
* `{ x:=4, y:=5 : Vec2n }.to_prod_yx = (5, 4)`
-/
def Vec2n.to_prod_yx (v : Vec2n) : Nat × Nat :=
    (v.y, v.x)

#guard (5, 4) == { x:=4, y:=5 : Vec2n }.to_prod_yx


/-- `Vec2n` to `Nat × Nat` as (x, y).
* `{ x:=4, y:=5 : Vec2n }.to_prod_xy = (4, 5)`
-/
def Vec2n.to_prod_xy (v : Vec2n) : Nat × Nat :=
    (v.x, v.y)

#guard (4, 5) == { x:=4, y:=5 : Vec2n }.to_prod_xy


/-- For debug only! -/
def Vec2n.toString (v : Vec2n) : String :=
    s!"[{v.x} {v.y}]"


/-- Add `n` to both `x` & `y`.
*  `{ x:=4, y:=5 : Vec2n }.add_to_xy 10 = { x:=14, y:=15 }`
*  `Vec2n.zero.add_to_xy 42 ={ x:=42, y:=42 }`
-/
def Vec2n.add_to_xy (v : Vec2n) (n : Nat) : Vec2n :=
    { x := v.x + n, y := v.y + n }

#guard { x:=42, y:=42 : Vec2n } == Vec2n.zero.add_to_xy 42
#guard { x:=14, y:=15 : Vec2n } == { x:=4, y:=5 : Vec2n }.add_to_xy 10

