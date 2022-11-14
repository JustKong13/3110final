type game
(** The abstract type of values representing the games. *)

val strings : string -> string list -> string list
(** Generates a list of [string list] with 10 random strings, where strings are
    either 1 or 2 chars, and approx. 30% of the letters are vowels. Excludes two
    letter combinations that are not found, or extremely rare, in the English.
    EXTRA TIP: The alphabet with the frequency of vowels doubled, and the empty
    list, should be passed in as arguments*)
