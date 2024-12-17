--- string



/-- Replicate string `s` `n` times.
* `"abc".replicate 3 = "abcabcabc"`
* `String.replicate 3 "abc" = "abcabcabc"`
-/
def String.replicate (n : Nat) (s : String) : String :=
    List.replicate n s |> join

#guard "abcabcabc" == "abc".replicate 3
#guard "abcabcabc" == String.replicate 3 "abc"



/-- Join strings with separator.
* `"-".join_ ["a", "b", "c"] = "a-b-c"`
-/
def String.join_ (sep : String) (ss : List String) : String :=
    sep.intercalate ss

#guard "a-b-c" == "-".join_ ["a", "b", "c"]


