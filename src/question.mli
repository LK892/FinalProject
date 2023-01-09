(**Question transform our JSON data into a type question and filtern inputted answers*)

type t
(** Representation of questions, answers, and points.*)

val from_json : Yojson.Basic.t -> t
(** [from_json i] is a question and respective answers that [i] showcases.
Requires that [i] is a representation of a valid JSON question and respective
answers**)

val get_questions : t -> string list
(** [get_question q] is a list of questions from type [q].*)

val get_answers : t -> string -> string list list
(** [get_answers q question] is a list of the lists of correct answers from [question]. *)

(*val getter : t -> string -> string -> int*)

val get_point : t -> string -> string -> int
(** [get_point q question answer] is a the amount of points that [answer] is worth for [question].*)

val filtered : t -> string list list -> string -> string -> string list list
(** [filtered q answ_list quest input] returns the list of answers for a 
      particular question with any previously given answers removed*)