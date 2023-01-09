(*open Familyfeud*)
open Question
open Fuzzy
open Graphics
open Gui

let initial_score = 0
let total_score = ref 0
let total_score2 = ref 0
let num_question = ref 0
let random_init = Random.self_init ()

(**[is_random_order] represents if the questions are randomized*)
let is_random_order = false

let random_question q =
  random_init;
  let len = List.length (Question.get_questions q) - 1 in
  let ri = Random.int len in
  List.nth (Question.get_questions q) ri

let is_space answ = String.contains answ ' '

let fuzzy_similar input answ =
  if is_space answ then Fuzzy.is_similar_mult_words answ input
  else Fuzzy.is_similar_one_word answ input

let rec match_answers indiv_answ input =
  match indiv_answ with
  | [] -> (false, input)
  | h :: t -> if fuzzy_similar input h then (true, h) else match_answers t input

let rec parse_answers1 q answ_list quest input =
  match answ_list with
  | [] -> (false, input)
  | h :: t ->
      let b, fuzzy_input = match_answers h input in
      if b = true then (true, fuzzy_input) else parse_answers1 q t quest input

let calculate_current_score q quest input initial_score =
  Question.get_point q quest input + initial_score

let random_sign lst x y w h =
  random_init;
  let len = List.length lst - 1 in
  let randomsign = Random.int len in
  let choosen_sign = List.nth lst randomsign in
  set_color black;
  set_font "-*-helvetica-medium-r-normal--25-*-*-*-*-*-*-*";
  draw_rect x y w h;
  moveto (x + 10) (y + (h / 3));
  draw_string choosen_sign;
  set_font "-*-helvetica-medium-r-normal--10-*-*-*-*-*-*-*"

(*[a_match_true] is a tuple of list of string lists that represents the
   answers left for the question and an int that represents the current score
   of the player. This is called when the input answer is a valid answer.*)
let a_match_true q answ_list quest input initial_score =
  let current_score = calculate_current_score q quest input initial_score in
  let str =
    "Correct! You get "
    ^ string_of_int (Question.get_point q quest input)
    ^ " points! Your current score is "
    ^ string_of_int current_score
    ^ " Please enter your next answer "
  in
  moveto 120 500;
  draw_string str;
  random_sign Gui.encouraging_signs_list 200 200 400 100;
  (Question.filtered q answ_list quest input, current_score, true)

(*[a_match_false] is a tuple of a list of answers left [answ_list] for the
  question and an int [intial_score] that is the number of points the player
  has. It prints out incorrect answer and the number of tries left, since it
  is called with an incorrect answer input. *)
let a_match_false q answ_list quest num (initial_score : int) =
  let str = "Incorrect Answer. " ^ string_of_int num ^ " tries left. " in
  moveto 120 500;
  draw_string str;
  random_sign Gui.discouraging_signs 200 200 400 100;
  (answ_list, initial_score, false)

(*[parser q answ_list quest num initial_score] is a tuple of a list of string
  list and an int. It parses the inputed answer and determines if it is a valid
  answer choice and then performs the appropriate action. It runs till all answer
  chocies are guess or no more tries are left.*)
let parser q answ_list quest num initial_score (mode : color)
    (overall_mode : color) : string list list * int * bool =
  let input = Gui.enter_answers 10 "" mode overall_mode in
  let a_match, fuzzy_input = parse_answers1 q answ_list quest input in
  if a_match = true then
    a_match_true q answ_list quest fuzzy_input initial_score
  else a_match_false q answ_list quest num initial_score

let updateScore quest initial_score player q mode overall_mode =
  if player = 1 then total_score := !total_score + initial_score
  else total_score2 := !total_score2 + initial_score;
  if mode = 1 || mode = 2 then
    Gui.display_all_answers q !total_score !total_score2 player mode
      overall_mode
  else Gui.display_top_answer q !total_score;
  Gui.wait_for_next_screen ();
  print_endline "End of Question"

(*[parses_num q answ_list quest num initial_score] iterates through a question
  till there are no more tries left. It goes through the first test and sees if
  we can go to the next one*)
let rec parses_num q answ_list quest num initial_score player (mode : color)
    (overall_mode : color) : unit =
  let new_answ = parser q answ_list quest num initial_score mode overall_mode in
  if num = 0 then
    match new_answ with
    | x, y, z -> updateScore quest y player q mode overall_mode
  else
    match new_answ with
    | [ []; []; []; [] ], y, z -> updateScore quest y player q mode overall_mode
    | h, t, false -> parses_num q h quest (num - 1) t player mode overall_mode
    | x, y, true -> parses_num q x quest num y player mode overall_mode

let number_of_question user_question = num_question := user_question

let rec questions_string_gen q questions =
  if !num_question = 0 then (
    Gui.no_more_ques_screen !total_score;
    Graphics.wait_next_event [ Button_down; Key_pressed ];
    "No more questions and your total score is " ^ string_of_int !total_score)
  else
    match questions with
    | [] ->
        "No more questions and your total score is "
        ^ string_of_int !total_score
    | h :: t ->
        let rand_quest = if is_random_order then random_question q else h in
        let () = Gui.print_question 1 rand_quest in
        (*let () = print_endline (rand_quest ^ " Type your answer") in*)
        num_question := !num_question - 1;
        let () =
          parses_num q (Question.get_answers q rand_quest) rand_quest 3 0 1 1 1
        in
        questions_string_gen q t

let rec questions_string_gen2 q questions (name1, name2) =
  if !num_question = 0 then (
    Gui.no_more_ques_screen2 !total_score name1 name2 !total_score2;
    Graphics.wait_next_event [ Button_down; Key_pressed ];
    "No more questions. " ^ name1 ^ " total score is "
    ^ string_of_int !total_score ^ " and " ^ name2 ^ " total score is "
    ^ string_of_int !total_score2)
  else
    match questions with
    | [] ->
        "No more questions and your total score is "
        ^ string_of_int !total_score
    | h :: t ->
        let rand_quest = if is_random_order then random_question q else h in
        let () = Gui.display_player_name rand_quest name1 in
        let () =
          parses_num q (Question.get_answers q rand_quest) rand_quest 3 0 1 1 2
        in
        let () = Gui.pass_next_player name2 in
        Graphics.wait_next_event [ Button_down; Key_pressed ];
        let () = Gui.display_player_name rand_quest name2 in
        let () =
          parses_num q (Question.get_answers q rand_quest) rand_quest 3 0 2 2 2
        in
        num_question := !num_question - 1;
        let () = Gui.pass_next_player name1 in
        Graphics.wait_next_event [ Button_down; Key_pressed ];
        questions_string_gen2 q t (name1, name2)

let rec questions_helper q questions =
  let () = Gui.game_mode_1 () in
  print_endline (questions_string_gen q questions)

let data_dir_prefix = "data" ^ Filename.dir_sep

let rec questions_helper_2 q questions (name1, name2) =
  let () = Gui.game_mode_2 () in
  print_endline (questions_string_gen2 q questions (name1, name2))

let rec questions_helper_final q questions =
  let () = Gui.final_round () in
  if !num_question = 0 then (
    Gui.no_more_ques_screen !total_score;
    Graphics.wait_next_event [ Button_down; Key_pressed ];
    print_endline
      ("No more questions and your total score is " ^ string_of_int !total_score))
  else
    match questions with
    | [] ->
        print_endline
          ("No more questions and your total score is "
         ^ string_of_int !total_score)
    | h :: t ->
        let rand_quest = if is_random_order then random_question q else h in
        let () = Gui.print_question 3 rand_quest in
        let () = print_endline (rand_quest ^ ". Type your answer") in
        num_question := !num_question - 1;
        let () =
          parses_num q (Question.get_answers q rand_quest) rand_quest 0 0 1 3 3
        in
        questions_helper_final q t
