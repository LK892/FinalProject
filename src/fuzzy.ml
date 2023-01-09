open Question

(**[is_space answ] is true if [answ] contains a space. Will show if correct
   answer is multiword.*)
let is_space answ = String.contains answ ' '

(**[substr_one_word answ input] returns false if all chars in [answ] aren't in
   [input], otherwise true. *)
let substr_one_word answ input =
  let len_answ = String.length answ - 1 in
  let arr = Array.make (String.length answ) false in
  for i = 0 to len_answ do
    if String.contains input (String.get answ i) = false then arr.(i) <- false
    else arr.(i) <- true
  done;
  Array.exists (fun x -> x = false) arr = false

let rec is_similar_one_word answ input =
  if
    is_space answ = false
    && substr_one_word
         (String.trim (String.lowercase_ascii answ))
         (String.trim (String.lowercase_ascii input))
       = true
  then true
  else false

(**[make_word_list answ] separates a phrase of multiple words that has length 
   than 3 into a separate element within a list*)
let make_word_list answ =
  List.filter (fun x -> String.length x > 3) (String.split_on_char ' ' answ)

(**[subst_two_words answ_split input] tests is any of the words within the 
  multiword answer (with a length greater than three) are similar to input*)
let rec subst_two_words answ_split input =
  match answ_split with
  | [] -> false
  | h :: t -> if substr_one_word h input then true else subst_two_words t input

let rec is_similar_mult_words answ input =
  if
    subst_two_words
      (make_word_list (String.trim (String.lowercase_ascii answ)))
      (String.trim (String.lowercase_ascii input))
    = true
  then true
  else false
