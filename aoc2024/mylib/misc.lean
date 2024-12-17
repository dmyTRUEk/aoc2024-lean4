--- misc

-- TODO(refactor): put them in alphabetic order?

-- TODO
/- def todo := sorry -/

/- def partition_n (l : List T) : List (List T) := -/
/-     sorry -/



class HAddExcl (α : Type u) (β : Type v) (γ : outParam (Type w)) where
    h_add_excl : α → β → γ

infixl:65 "+!" => HAddExcl.h_add_excl



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
* `apply_n 0 (. * 2) 1 = 1`
* `apply_n 1 (. * 2) 1 = 2`
* `apply_n 2 (. * 2) 1 = 4`
* `apply_n 8 (. * 2) 1 = 256`
-/
def apply_n (n : Nat) (f : T -> T) (arg : T) : T :=
    if n <= 0 then arg else apply_n (n-1) f (f arg)

#guard apply_n 0 (. * 2) 1 == 1
#guard apply_n 1 (. * 2) 1 == 2
#guard apply_n 2 (. * 2) 1 == 4
#guard apply_n 8 (. * 2) 1 == 256





/-- Constant function.
* `(const_fun true) 0     = true`
* `(const_fun true) 10    = true`
* `(const_fun true) "abc" = true`
* `(const_fun true) "def" = true`
* `(const_fun true) true  = true`
* `(const_fun true) false = true`

* `(const_fun 42) 0     = 42`
* `(const_fun 42) 10    = 42`
* `(const_fun 42) "abc" = 42`
* `(const_fun 42) "def" = 42`
* `(const_fun 42) true  = 42`
* `(const_fun 42) false = 42`

* `(const_fun "abc") 0     = "abc"`
* `(const_fun "abc") 10    = "abc"`
* `(const_fun "abc") "abc" = "abc"`
* `(const_fun "abc") "def" = "abc"`
* `(const_fun "abc") true  = "abc"`
* `(const_fun "abc") false = "abc"`
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



/-- Flatten `Option Option T` into `Option T`.
* `(some $ some 42).flatten = some 42`
* `(some $ some 42 : Option $ Option Nat).flatten = some 42`
* `(some none      : Option $ Option Nat).flatten = none`
* `(none           : Option $ Option Nat).flatten = none`
-/
def Option.flatten : Option (Option T) -> Option T
    | some $ some v => some v
    | _ => none

#guard some 42 == (some $ some 42 : Option $ Option Nat).flatten
#guard none    == (some none      : Option $ Option Nat).flatten
#guard none    == (none           : Option $ Option Nat).flatten



/-- Reverse float division. Usefull for `.map`.
* `Float.div_ 10. 20. = 2.`
* `10..div_ 20. = 2.`
* `([10., 20., 30.] |>.map $ Float.div_ 10.) = [1., 2., 3.]`
-/
def Float.div_ (denominator numerator : Float) : Float :=
    numerator / denominator

#guard 2. == Float.div_ 10. 20.
#guard 2. == 10..div_ 20.
#guard [1., 2., 3.] == ([10., 20., 30.] |>.map $ Float.div_ 10.)



/-- Swap elements in product type.
* `(1, 'a').swap = ('a', 1)`
-/
def Prod.swap (prod : A × B) : B × A := (prod.snd, prod.fst)

#guard ('a', 1) == (1, 'a').swap

