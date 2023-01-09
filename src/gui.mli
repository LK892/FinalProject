open Graphics
open Question

(**GUI contains the functionality to open a GUI window and play the game*)

val set_background : color -> unit
(**[set_background color] sets background color of gui*)

val make_button : 'a -> unit
(**[make_button x str] creates the starting buttons on the home page*)

val enter_questions : unit -> string
(**[enter_question ()] ensures the player types in a valid positve integer
   as the number of questions they would like to play and returns the number of
   questions that is being played.*)

val game_mode_1 : unit -> unit
(**[game_mode_1] gui screen for gamemode 1*)

val print_question : int -> string -> unit
(**[print_question] print individual questions in gamemode1*)

val wait_for_next_screen : unit -> unit
(**[wait_for_next_screen] between displaying correct answers after round to next
    question; press enter to go to next question*)

val display_all_answers : t -> color -> color -> color -> color -> color -> unit
(**[display_all_answers] display the correct answers and total score after each question*)

val enter_answers : color -> string -> color -> color -> string
(**[enter_answers] Evaluates entered characters by player*)

val no_more_ques_screen : color -> unit
(** [no_more_ques_screen] ending screen after round of gamemode1*)

val no_more_ques_screen2 : int -> string -> string -> int -> unit
(** [no_more_ques_screen] ending screen after round of gamemode2*)

val game_mode_2 : unit -> unit
(**[game_mode_2] gui screen for gamemode 2*)

val display_top_answer : t -> color -> unit
(** [display_top_answer] displays top correct answer in final round*)

val final_round : unit -> unit
(**[final_round] gui screen for final round*)

val enter_player_name : string -> string
(** [enter_player_name] takes in names of players from gui*)

val encouraging_signs : color -> color -> color -> color -> string -> unit
(**[encouraging_signs x y w h str ] creates signs for decoration*)

val discouraging_signs : string list
(**List of strings to passive-agressively encourage players *)

val encouraging_signs_list : string list
(**List of strings to encourage players positively*)

val display_player_name : string -> string -> unit
(**display's player name*)

val pass_next_player : string -> unit
(**creates display screen to pass to the next player*)