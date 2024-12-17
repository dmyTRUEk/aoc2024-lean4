--- nat



/-- Distance from `self` to `other`.
* `(3).distance_to 5 = 2`
-/
def Nat.distance_to (self other : Nat) : Nat :=
    if self > other then
        self - other
    else
        other - self

#guard 2 == (3).distance_to 5

