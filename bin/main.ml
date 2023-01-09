(*Main file for playing the game*)

open Familyfeud
open Question
open Game
open Graphics
open Gui

let data_dir_prefix = "data" ^ Filename.dir_sep

(**[enter_names ()] gets input for players names for two player mode*)
let enter_names () =
  let player1 = Gui.enter_player_name "1" in
  let player2 = Gui.enter_player_name "2" in
  (player1, player2)

(** [play_game] opens the file and passes it as a parameter for [questions_helper]
   to get the questions in the file and start the answer process for either the
   one player or two player process*)
let play_game f mode =
  let j = Yojson.Basic.from_file f in
  let q = Question.from_json j in
  let input = Gui.enter_questions () in
  let () = Game.number_of_question (int_of_string input) in
  if mode = 1 then Game.questions_helper q (Question.get_questions q)
  else if mode = 2 then
    Game.questions_helper_2 q (Question.get_questions q) (enter_names ())
  else Game.questions_helper_final q (Question.get_questions q)

(**[question_file_name] is the name of the json file questions are drawn from*)
let question_file_name = data_dir_prefix ^ "gamemode1" ^ ".json"

(**[print_mode_message] is the message to print to prompt user to enter gamemode *)
let print_mode_message =
  "In the termianl, you can see when a question round has ended. You will play \
   the game in the GUI pop-up window. Enjoy and have fun!"

(**[evaluate_mode value] evaluates mode to choose what to start on*)
let rec evaluate_mode value =
  let key_pressed = value.key in
  match key_pressed with
  | '1' -> "gamemode1"
  | '2' -> "gamemode2"
  | '3' -> "final"
  | _ -> "invalid"

(** [try_mode_again] runs when user input mode is not found and prints out
    an Error. *)
let rec try_mode_again m file =
  if m = "gamemode1" then play_game file 1
  else if m = "gamemode2" then play_game file 2
  else if m = "final" then play_game file 3
  else
    let file = question_file_name in
    let mode = evaluate_mode (wait_next_event [ Key_pressed ]) in
    try_mode_again mode file

(** [new_window ()] opens new gui window with the home page*)
let new_window () =
  let () = open_graph " 800x800" in
  set_window_title "Family Feud";
  Gui.set_background blue;
  Gui.make_button "";
  moveto 350 700;
  set_color white;
  draw_string "Family Feud"

(** [main ()] prompts for the game to play, then starts the game with the input
    mode the user chooses. *)
let main () =
  let () = new_window () in
  ANSITerminal.print_string [ ANSITerminal.red ] "\n\nWelcome to Family Feud.\n";
  print_endline print_mode_message;
  print_string "> ";
  let file = question_file_name in
  try_mode_again (evaluate_mode (wait_next_event [ Key_pressed ])) file

(** Execute the game engine. *)
let () = main ()