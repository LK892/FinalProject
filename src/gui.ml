open Graphics
open Question

(*to store the question that we're at right now*)
let ques = ref ""

let discouraging_signs =
  [
    "You are not good at this!";
    "Do Better!";
    "Think Harder";
    "Why so many wrong?";
    "Read more.";
    "Study harder.";
    "Try some more"
    (*
    "Please try.";
    "Family feud is supposed to be easy";
    "I thought you knew this.";
    "Practice.";
    "What is this.";
    "Try harder.";
    "YOU'll Get it Next time!";
    "The force is not with you";
    "We thought you could do this.";
    "We don't approve of this answer.";
    "How could someone be this bad.";
    "Why can't you get this right";*);
  ]

let encouraging_signs_list =
  [
    "Good job!";
    "YAY!!";
    "YOU GOT THIS!!";
    "YOU ARE DOING AMAZING!!";
    "WOW";
    "SUPERSTARS!";
    "WINNERS RIGHT HERE!"
    (*
    "WE BELIEVE IN YOU!!";
    "THIS IS SO GOOD!!";
    "THE BEST IN THE WORLD";
    "CHAMPIONS RIGHT HERE";
    "THE FUTURE SUPERPOWERS!!";
    "WHAT SMART PEOPLE!!";
    "BEAUTIFUL JOB";
    "WE ARE PROUD OF YOU";
    "SLAY";
    "THE FORCE IS WITH YOU!";*);
  ]

let balloon x y r color =
  let q = color in
  set_color black;
  draw_poly_line [| (x, y - r); (x, y - r - 50) |];
  set_color q;
  fill_circle x y r;
  set_color black;
  fill_poly
    [| (x - (r / 2), y - r + 5); (x - (r / 2), y - r - 15); (x, y - r - 5) |];
  fill_poly
    [| (x + (r / 2), y - r + 5); (x + (r / 2), y - r - 15); (x, y - r - 5) |]

(*[set_background color] sets background color of gui*)
let set_background color =
  let c = color in
  set_color c;
  fill_rect 0 0 (size_x ()) (size_y ())

(*[make_button_helper x str] helps create starting buttons on home page*)
let make_button_helper x str =
  set_color white;
  draw_rect x 495 190 90;
  moveto (x + 13) 535;
  draw_string str

let encouraging_signs x y w h str =
  set_color red;
  draw_rect x y w h;
  moveto (x + 9) (y + 10);
  draw_string str

let family_feud_button x y =
  set_color green;
  draw_circle x y 60;
  moveto (x - 35) y;
  draw_string "FAMILY FEUD!!"

let diamond x y size =
  fill_poly [| (x, y - size); (x + size, y); (x, y + size); (x - size, y) |]

(*[make_button x str] creates starting buttons on home page*)
let make_button str =
  set_color yellow;
  diamond 300 700 15;
  diamond 460 700 15;
  make_button_helper 65 "Press 1 for one-player mode";
  make_button_helper 295 "Press 2 for two-player mode";
  make_button_helper 525 "Press 3 for final round";
  moveto 350 700;
  set_color white;
  draw_string "Family Feud";
  moveto 130 350;
  draw_string
    "Rules: Once you pick a game mode, enter a valid number of questions (1-9) \
     to play the game.";
  moveto 100 320;
  draw_string
    "In Game Mode 1, answer the questions as they pop up on the screen. If you \
     answer incorrectly, you get";
  moveto 100 300;
  draw_string
    " a strike. If you get three strikes before guessing all the correct \
     answers then you move on to the";
  moveto 150 280;
  draw_string
    " next question. Once you finish all your questions, you will see your \
     total score!";
  moveto 100 250;
  draw_string
    "In Game Mode 2, pick one friend to answer the first question first and \
     once they run out of strikes";
  moveto 100 230;
  draw_string
    " the second person answers the questions. Once you both answer all the \
     questions, your total";
  moveto 300 210;
  draw_string " scores determine the winner!";
  moveto 100 180;
  draw_string
    "In the Final Round, you only get one try per question. You are a winner \
     if you get a total of";
  moveto 85 160;
  draw_string
    " (20 * number_of_questions) points! For example, if you play 5 questions, \
     you need 100 points to win!";
  encouraging_signs 340 100 90 30 "You Got This!";
  encouraging_signs 327 400 120 30 "We believe in you!"

(*helper function to go through each question from enter_questions*)
let rec enter_question_num () =
  encouraging_signs 300 300 230 30 "Make sure you pick the right number!";
  let input = (wait_next_event [ Key_pressed ]).key in
  if
    int_of_string_opt (Char.escaped input) < Some 0
    || int_of_string_opt (Char.escaped input) = None
  then enter_question_num ()
  else Char.escaped input

(*[enter_question_num ()] ensures the player types in a valid positve integer
   as the number of questions they would like to play.*)
let enter_questions () =
  clear_graph ();
  set_background blue;
  moveto 215 700;
  set_text_size 500;
  set_color white;
  draw_string
    "Enter the number of questions that you would like to play (from 0-9)!";
  let input = enter_question_num () in
  draw_string input;
  input

let game_mode_1 () =
  clear_graph ();
  set_background blue;
  set_color white;
  moveto 350 700;
  set_font "-*-helvetica-medium-r-normal--15-*-*-*-*-*-*-*";
  draw_string "One Player Mode";
  draw_rect 100 150 600 500;
  moveto 100 580;
  lineto 700 580;
  moveto 130 100;
  draw_string "Enter an answer: "

let game_mode_2 () =
  clear_graph ();
  set_background blue;
  set_color white;
  moveto 330 700;
  set_font "-*-helvetica-medium-r-normal--12-*-*-*-*-*-*-*";
  draw_string "Two Player Mode";
  draw_rect 100 150 600 500;
  moveto 100 580;
  lineto 700 580;
  moveto 130 100;
  draw_string "Enter an answer: "

let final_round () =
  clear_graph ();
  set_background blue;
  set_color white;
  moveto 330 700;
  draw_string "Final Round";
  draw_rect 100 150 600 500;
  moveto 100 580;
  lineto 700 580;
  moveto 150 100;
  set_font "-*-helvetica-medium-r-normal--15-*-*-*-*-*-*-*";
  draw_string "Enter an answer: "

(*print individual questions in gamemode1*)
let print_question mode q =
  if mode = 1 then game_mode_1 ()
  else if mode = 2 then game_mode_2 ()
  else final_round ();
  moveto 150 600;
  set_text_size 500;
  set_color white;
  set_font "-*-helvetica-medium-r-normal--12-*-*-*-*-*-*-*";
  draw_string ("Question: " ^ q);
  ques := q

(*between displaying correct answers after round to next question; press enter to go to next question*)
let rec wait_for_next_screen () =
  match Char.code (wait_next_event [ Key_pressed ]).key with
  | 13 -> ()
  | _ -> wait_for_next_screen ()

(*helper function to display correct answers*)
let rec display_correct_answers answ x y =
  match answ with
  | [] -> draw_string ""
  | [ h ] ->
      moveto x (y - 60);
      draw_string ("Correct answer: " ^ h);
      moveto 100 (y - 80);
      lineto 700 (y - 80)
  | h :: t ->
      moveto x (y - 60);
      draw_string ("Correct answer: " ^ h);
      moveto 100 (y - 80);
      lineto 700 (y - 80);
      display_correct_answers t x (y - 60)

(*display the correct answers and total score after each question*)
let display_all_answers q score_1 score_2 player mode overall_mode =
  if overall_mode = 1 then print_question 1 !ques
  else if overall_mode = 2 then print_question 2 !ques;
  let ans_list = Question.get_answers q !ques in
  let correct_ans_list = List.map (fun x -> List.hd x) ans_list in
  moveto 150 500;
  set_font "-*-helvetica-medium-r-normal--12-*-*-*-*-*-*-*";
  draw_string
    "End of question. Here are the correct answers! Press enter to move on to \
     next question.";
  set_color green;
  display_correct_answers correct_ans_list 110 500;
  family_feud_button 700 720;
  set_color white;
  if player = 1 then (
    moveto 300 200;
    draw_string ("Your current score is: " ^ string_of_int score_1))
  else (
    moveto 300 200;
    draw_string ("Their current score is: " ^ string_of_int score_1);
    moveto 300 100;
    draw_string ("Your current score is: " ^ string_of_int score_2))

(*what to return if backspace typed when typing answer*)
let back_space s move =
  let () = set_color green in
  match String.length s with
  | 0 -> ("", false)
  | _ -> (String.sub s 0 (String.length s - 1), true)

(*typing answer*)
let rec enter_answers move s mode (overall_mode : color) =
  set_color white;
  set_font "-*-courier-medium-r-normal--12-*-*-*-*-*-*-*";
  moveto (250 + move) 100;
  let key = (wait_next_event [ Key_pressed ]).key in
  match Char.code key with
  | 13 ->
      let () = clear_graph () in
      let () =
        print_question
          (if overall_mode = 1 then 1 else if overall_mode = 2 then 2 else 3)
          !ques
      in
      s
  | 8 ->
      let str, length_bool = back_space s move in
      if length_bool then (
        let () = set_color blue in
        let () = draw_rect (250 + move - 10) 100 13 17 in
        fill_rect (250 + move - 10) 100 13 17;
        enter_answers (move - 10) str mode overall_mode)
      else enter_answers move "" mode overall_mode
  | _ ->
      let s = s ^ String.make 1 key in
      let () = draw_string (Char.escaped key) in
      enter_answers (move + 10) s mode overall_mode

(*draws heart*)
let draw_heart () =
  set_color red;
  let center_x = 300 in
  let center_y = 100 in
  let radius = 30 in
  let left_x = center_x + radius in
  let right_x = center_x + radius + 45 in
  let top_y = center_y + radius in
  let bottom_y = center_y + radius in
  draw_circle left_x top_y radius;
  draw_circle right_x bottom_y radius;
  fill_circle left_x top_y radius;
  fill_circle right_x bottom_y radius;
  fill_poly [| (307, 110); (350, 60); (397, 110) |]

(*draws peace sign*)
let draw_peace () =
  set_color red;
  set_line_width 10;
  let center_x = 200 in
  let center_y = 110 in
  let radius = 50 in
  draw_circle center_x center_y radius;
  moveto 200 160;
  lineto 200 60;
  moveto 200 110;
  lineto 170 80;
  moveto 200 110;
  lineto 230 80

let no_more_ques_screen total_score =
  clear_graph ();
  set_background blue;
  set_color white;
  moveto 200 400;
  set_font "-*-helvetica-medium-r-normal--20-*-*-*-*-*-*-*";
  draw_string
    ("No more questions and your total score is " ^ string_of_int total_score);
  moveto 235 500;
  set_font "-*-helvetica-medium-r-normal--30-*-*-*-*-*-*-*";
  draw_string "Thank you for playing!";
  encouraging_signs 200 200 345 100 "What an AMAZING JOB!!";
  draw_heart ();
  draw_peace ();
  moveto 450 60;
  set_line_width 10;
  set_font "-*-helvetica-medium-r-normal--90-*-*-*-*-*-*-*";
  draw_string "3110";
  balloon 600 550 30 red;
  balloon 170 550 30 red

(*gui screen for gamemode 2*)
let no_more_ques_screen2 total_score name1 name2 total_score2 =
  clear_graph ();
  set_background blue;
  set_color white;
  moveto 200 400;
  set_font "-*-helvetica-medium-r-normal--20-*-*-*-*-*-*-*";
  draw_string "No more questions! Final Scores are:";
  moveto 210 370;
  draw_string (name1 ^ ":" ^ string_of_int total_score);
  moveto 210 340;
  draw_string (name2 ^ ":" ^ string_of_int total_score2);
  moveto 235 500;
  set_font "-*-helvetica-medium-r-normal--30-*-*-*-*-*-*-*";
  draw_string "Thank you for playing!";
  encouraging_signs 200 200 345 100 "What an AMAZING JOB!!";
  draw_heart ();
  draw_peace ();
  moveto 450 60;
  set_line_width 10;
  set_font "-*-helvetica-medium-r-normal--90-*-*-*-*-*-*-*";
  draw_string "3110";
  balloon 600 550 30 red;
  balloon 170 550 30 red

(** Prints questions for the final round*)
let print_question_final q =
  final_round ();
  moveto 150 600;
  set_text_size 500;
  set_color white;
  draw_string ("Question: " ^ q);
  ques := q

(**note will change this a little so it prints different things based on your 
    answer- just want basic functionality rn*)
let display_top_answer q score =
  print_question_final !ques;
  let all_answs = Question.get_answers q !ques in
  let top_answ = List.hd (List.map (fun x -> List.hd x) all_answs) in
  moveto 150 500;
  set_font "-*-helvetica-medium-r-normal--13-*-*-*-*-*-*-*";
  draw_string "End of question! Press enter to go to the next question.";
  moveto 150 300;
  set_color green;
  draw_string ("Top Answer: " ^ top_answ);
  set_color white;
  moveto 300 200;
  draw_string ("Current Score: " ^ string_of_int score)

let enter_player_name (num : string) =
  clear_graph ();
  set_background blue;
  set_color white;
  moveto 290 700;
  draw_string ("Enter the name of player " ^ num);
  enter_answers 10 "" 2 2

let display_player_name rand_quest name =
  print_question 2 rand_quest;
  set_color white;
  moveto 330 670;
  set_font "-*-helvetica-medium-r-normal--20-*-*-*-*-*-*-*";
  draw_string (name ^ "'s turn. ")

let pass_next_player name =
  clear_graph ();
  set_background blue;
  set_color white;
  moveto 330 700;
  draw_string "Two Player Mode";
  moveto 300 450;
  set_font "-*-helvetica-medium-r-normal--40-*-*-*-*-*-*-*";
  draw_string ("Pass to " ^ name)
