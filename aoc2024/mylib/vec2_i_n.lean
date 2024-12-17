--- vec2i & vec2n

import aoc2024.mylib.misc
import aoc2024.mylib.vec2i
import aoc2024.mylib.vec2n

/- def Vec2n.to_vec2i (v : Vec2n) : Vec2i := -/
/-     { x := v.x, y := v.y : Vec2i } -/
/- #guard -/



/-- Add `other : Vec2i` to `self : Vec2n`.
* `{ x:=4, y:=7 : Vec2n } +! { x:=-1, y:=-2 : Vec2i } = { x:=3, y:=5 : Vec2n }`
* `{ x:=1, y:=2 : Vec2n } +! { x:=-4, y:=-7 : Vec2i } = { x:=0, y:=0 : Vec2n }`
-/
instance : HAddExcl Vec2n Vec2i Vec2n where
    h_add_excl := fun self other => {
        x := self.x + other.x |>.toNat,
        y := self.y + other.y |>.toNat,
    }

#guard { x:=3, y:=5 : Vec2n } == { x:=4, y:=7 : Vec2n } +! { x:=-1, y:=-2 : Vec2i }
#guard { x:=0, y:=0 : Vec2n } == { x:=1, y:=2 : Vec2n } +! { x:=-4, y:=-7 : Vec2i }

