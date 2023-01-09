open Question
open Graphics

(**Game parses through our questions and inputted answer and updates the points*)

val number_of_question : int -> unit
(**[number_of_question user_question] sets user_question as the number of
   questions that will be played*)

val parse_answers1 :
  Question.t -> string list list -> string -> string -> bool * string
(**[parse_answers1 q answ_list quest input] returns true if [input] is in
  [answ_list]*)

val questions_helper : Question.t -> string list -> unit
(** [questions_helper q questions] takes in a [q] and a list of questions
   [questions] and goes through each question in the list, if the question is
   unanswered it prompts the user to enter an answer to the question and
   evaluates it. Once the user runs out of tries or wins all of the points, it
   moves onto the next question till there are no more quesitons. At the end,
   it prints no more questions. *)

val questions_helper_2 : Question.t -> string list -> string * string -> unit
(**[questions_helper_2 q questions] represents the functionality of 
    questions_helper but with two players*)

val questions_helper_final : Question.t -> string list -> unit
(**[questions_helper_final q questions] represents functionality of 
    questions_helper but with one try per question*)

val is_space : string -> bool
(**[is_space answ] is true if [answ] contains a space. Will show if correct
   answer is multiword.*)

val fuzzy_similar : string -> string -> bool
(**[fuzzy_similar input answ] returns if input is similar to answ.*)

val random_question : Question.t -> string
(**[random_question q] returns a random question from the list of questions. *)

val calculate_current_score : Question.t -> string -> string -> int -> int
(**[calculate_current_score q quest input initial_score] is the number of points
   the player currently has. *)
