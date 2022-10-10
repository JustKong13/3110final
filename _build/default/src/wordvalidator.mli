(** Word validator module

    This module has only one callable method, [check_word] which is used to 
    check if the provided word of type [string] exists within an dictionary of
    English words *)

val check_word : string -> bool
(** [check_word word] returns a boolean if the word is in the dictionary of 
    English words*)