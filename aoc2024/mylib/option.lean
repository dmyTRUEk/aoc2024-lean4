--- option



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



/-- True is `option` is some and `pred` evaluates to true.
* `(none : Option Nat).is_some_and (fun n => n > 10) = false`
* `(some 5 : Option Nat).is_some_and (fun n => n > 10) = false`
* `(some 15 : Option Nat).is_some_and (fun n => n > 10) = true`
-/
def Option.is_some_and (option : Option T) (pred : T -> Bool) : Bool :=
    match option with
    | none => false
    | some value => pred value

#guard false == (none : Option Nat).is_some_and (fun n => n > 10)
#guard false == (some 5 : Option Nat).is_some_and (fun n => n > 10)
#guard true  == (some 15 : Option Nat).is_some_and (fun n => n > 10)

