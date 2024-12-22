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



/-- Convert natural (`Nat`) number `n` to integer (`Int`).
* `(2 : Nat).to_int = (2 : Int)`
-/
def Nat.to_int (n : Nat) : Int := n

#guard (2 : Int) == (2 : Nat).to_int

