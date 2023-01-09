open Question

(**Fuzzy allows some similar answers to be accepted as correct responses*)

val is_similar_one_word : string -> string -> bool
(**[is_similar_one_word l i] returns if a word in list l is similar to i*)

val is_similar_mult_words : string -> string -> bool
(**[is_similar_mult_words l i] returns if a word in list l is similar to i *)
