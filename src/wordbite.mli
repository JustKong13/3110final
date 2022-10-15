type game
(** The abstract type of values representing the games. *)

val available_strs : string -> string list -> string list
(** Generates a list of [string list] with 10 random strings, where strings are
    either 1 or 2 chars, and approx. 30% of the letters are vowels*)
