(*Transforms JSON file with data into type t*)
open Yojson.Basic.Util

type answer = { indv_answ : string list; points : int }
type questions_list = { question : string; answers : answer list }
type t = { questions : questions_list list }

let make_answer_type json =
  {
    indv_answ = json |> member "answer" |> to_list |> List.map to_string;
    points = json |> member "points" |> to_int;
  }

let make_questions_list_type json =
  {
    question = json |> member "question" |> to_string;
    answers = json |> member "answers" |> to_list |> List.map make_answer_type;
  }

let from_json json =
  {
    questions =
      json |> member "questions" |> to_list |> List.map make_questions_list_type;
  }

let get_questions q = List.map (fun x -> x.question) q.questions

let rec get_answers_helper_two answer =
  match answer with
  | [] -> []
  | h :: t -> h.indv_answ :: get_answers_helper_two t

let rec get_answers_helper q_list quest =
  match q_list with
  | [] -> []
  | h :: t ->
      if h.question = quest then get_answers_helper_two h.answers
      else get_answers_helper t quest

let get_answers q quest = get_answers_helper q.questions quest
let ans_match answer ans = if ans = answer then true else false

let rec get_points_helper2 answers answer =
  match answers with
  | [] -> 0
  | h :: t ->
      if List.exists (ans_match answer) h.indv_answ then h.points
      else get_points_helper2 t answer

let rec get_points_helper quest quest_list ans =
  match quest_list with
  | [] -> 0
  | h :: t ->
      if h.question = quest then get_points_helper2 h.answers ans
      else get_points_helper quest t ans

let get_point q quest answer = get_points_helper quest q.questions answer

let rec individual_filtered q answ_list input =
  let filter_list = List.filter (fun x -> input <> x) answ_list in
  if answ_list <> filter_list then [] else answ_list

let rec filtered q answ_list quest input =
  match answ_list with
  | [] -> []
  | h :: t -> individual_filtered q h input :: filtered q t quest input
