type t
(** The abstract type of values representing the games. *)

val available_strs : t -> string list
(** Generates a random list of [string list] with 10 strings, where strings are
    either 1 or 2 chars, and approx. 30% of the letters are vowels*)
