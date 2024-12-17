--- int



/-- Absolute value of the `Int`.
* `( 42 : Int).abs = 42`
* `(-42 : Int).abs = 42`
-/
def Int.abs (n : Int) : Int :=
    if n >= 0 then n else -n

#guard ( 42 : Int).abs == 42
#guard (-42 : Int).abs == 42



/-- Distance from `self` to `other`.
* `(-3).distance_to 5 = 8`
-/
def Int.distance_to (self other : Int) : Nat :=
    (self - other).natAbs

#guard 8 == (-3).distance_to 5

