open OUnit2
open Familyfeud
open Question
open Game
open Fuzzy

(** 
    Testing Plan: 

    Our game is ran on a GUI, so we tested the parts of the 
    system that couldn't be tested with OUnit manually while testing as many 
    aspects of the system possible with OUnit tests. Our OUnit tests include 
    testing all of the operations to parse our json file within the question.ml
    file. For every test case for each operation, we ensured to test with at 
    least three questions from our json file, the first, the last, and another 
    from the middle to ensure correctness. We also tested any edge cases for 
    these operations. We also wrote OUnit tests for our fuzzy algorithm, which 
    detects similar inputs, the score calculator, and the functionality within
    our code that parses answers. Ensuring these lower level details were 
    correct was key in being able to test our GUI concurrently, as we could rule
    out code as the source of problems. We wrote OUnit tests using mostly black 
    box testing but using some glass box testing as well to try and ensure our 
    correctness.

    While some aspects of our test suite, as an example the fuzzy algorithm, 
    could have been theoretically tested in the GUI, we choose to write OUnit 
    tests to have a way to consitently check to ensure certain aspects of our 
    system continued to work properly as we implemented new features. 

    For testing the GUI, we all individually tried out many scenarios to ensure
    expected behavior, and continued to monitor this behavior as we made 
    progress in our implementation. In total, the combination of OUnit tests
    along with manually testing the GUI helped us ensure correctness of our
    code.

*)
let data_dir_prefix = "data" ^ Filename.dir_sep

let questions = Yojson.Basic.from_file (data_dir_prefix ^ "questions.json")
let gamemode1 = Yojson.Basic.from_file (data_dir_prefix ^ "gamemode1.json")

let get_answers_test (name : string) (input : string list list)
    (expected_output : string list list) =
  name >:: fun _ -> assert_equal expected_output input

let get_answers_tests =
  [
    get_answers_test "GameMode1 Question on Line 7341 Answer Test"
      (get_answers (from_json gamemode1)
         "Name Something Humans Wear, But Cats Look Silly In")
      [ [ "Hats" ]; [ "Shoes" ]; [ "Glasses" ]; [ "Sweater" ] ];
    get_answers_test "Gamemode1 Answer Q1 Test"
      (get_answers (from_json gamemode1) "Name Something That Can Be Spoiled")
      [ [ "Milk" ]; [ "Food" ]; [ "Kids" ]; [ "A Surprise" ] ];
    get_answers_test "Gamemode1 Answer Middle Question Test"
      (get_answers (from_json gamemode1)
         "Name Something You Include On A Letter, But Not On An E-Mail.")
      [
        [ "Stamp" ]; [ "Signature" ]; [ "Address" ]; [ "Salutation/ Greeting" ];
      ];
    get_answers_test "Gamemode1 Answer End Question Test"
      (get_answers (from_json gamemode1)
         "Name Someplace You’d Have To Be On Really Good Terms To Invite Your \
          Ex.")
      [
        [ "Your Wedding" ];
        [ "Birthday Party" ];
        [ "Family Reunion" ];
        [ "Your House" ];
      ];
    get_answers_test "GameMode1 Question 20"
      (get_answers (from_json gamemode1)
         "Name Something A Teenage Boy Might Ask His Dad For.")
      [ [ "Car" ]; [ "Money" ]; [ "Advice" ]; [ "Bike" ] ];
    get_answers_test "GameMode1 JSON Question 10"
      (get_answers (from_json gamemode1)
         "What Might A Professional Athlete Do Just After A Sports Game To \
          Avoid Getting Sore Muscles?")
      [
        [ "Stretch" ];
        [ "Get A Massage" ];
        [ "Soak In Hot Tub" ];
        [ "Apply Ice" ];
      ];
    get_answers_test "GameMode1 JSON Question 30"
      (get_answers (from_json gamemode1)
         "Name Something From The Laundry That’s Impossible To Fold Neatly.")
      [ [ "Fitted Sheets" ]; [ "Socks" ]; [ "Underwear" ]; [ "Blouse" ] ];
    get_answers_test "GameMode1 JSON Line 11546 Question Answer Test"
      (get_answers (from_json gamemode1)
         "Name An Occupation In which You’d Get To Use A Two Way Radio")
      [
        [ "Police Officer" ];
        [ "Trucker" ];
        [ "Construction" ];
        [ "Security Guard" ];
      ];
    get_answers_test "GameMode 1 JSON Line 17143 Answer Test"
      (get_answers (from_json gamemode1)
         "Name A Reason Why You Might Offer Your Bus Seat To Someone Else.")
      [ [ "Elderly" ]; [ "Pregnant" ]; [ "Disability" ]; [ "Hands Full" ] ];
    get_answers_test "GameMode1 JSON Line 21812 Answer Test"
      (get_answers (from_json gamemode1)
         "Name Something It’s Probably Much Harder To Do In Outer Space Than \
          It Is On Earth.")
      [ [ "Breathe" ]; [ "Walk" ]; [ "Go To The Bathroom" ]; [ "Eat" ] ];
    get_answers_test "GameMode1 Line 27699 Answers "
      (get_answers (from_json gamemode1)
         "Tell Me The Occupation Of Someone Who Might Wear A Headlamp.")
      [
        [ "Miner" ];
        [ "Doctor/Dentist" ];
        [ "police/Detective" ];
        [ "Construction Worker" ];
      ];
    get_answers_test "GameMode1 Line 25379 Answers "
      (get_answers (from_json gamemode1)
         "Without Seeing Them Steal, Name Something That You’d See A Person Do \
          To Make You Suspicious That They’re Shoplifting.")
      [ [ "Looking Around" ]; [ "Big Coat" ]; [ "Hands In Pocket" ]; [ "Run" ] ];
    get_answers_test "GameMode1 Line 23262 Answers "
      (get_answers (from_json gamemode1)
         "We Asked 100 Husbands: Name Something Your Wife Spends More Money On \
          Than You Do.")
      [ [ "Clothes" ]; [ "Hair Products" ]; [ "Make Up" ]; [ "Jewelry" ] ];
    get_answers_test "GameMode1 Line 21522 Answers "
      (get_answers (from_json gamemode1)
         "Name Something From School That A Kid Might Try To Hide From Their \
          Parents.")
      [
        [ "Report Card/Grades" ];
        [ "Note From Teacher" ];
        [ "Homework" ];
        [ "Detention/Discipline" ];
      ];
    get_answers_test "GameMode1 Line 19086 Answers"
      (get_answers (from_json gamemode1)
         "If You’re Going To Do Your Own Taxes, Name Something You Probably \
          Need")
      [ [ "Computer/Calculator" ]; [ "Forms" ]; [ "Records" ]; [ "Pencil" ] ];
    get_answers_test "GameMode1 Line 18622 Answers"
      (get_answers (from_json gamemode1)
         "Name A Restaurant Job That A Stay-At-Home Parent Does For Free All \
          The Time")
      [ [ "Cook" ]; [ "Dishwasher" ]; [ "Server" ]; [ "Busser" ] ];
    get_answers_test "GameMode1 Line 16476 Answers "
      (get_answers (from_json gamemode1)
         "What Might Your Teenager Do While Borrowing Your Car That Would \
          Break Your Trust?")
      [
        [ "Accident" ];
        [ "Speeding Ticket" ];
        [ "Illegal Activity" ];
        [ "Break Curfew" ];
      ];
    get_answers_test "GameMode1 Line 13750 Answers"
      (get_answers (from_json gamemode1)
         "Name Something That You Write Out By Hand, Instead Of Type.")
      [ [ "Letter/Card" ]; [ "Check" ]; [ "Signature" ]; [ "Grocery List" ] ];
    get_answers_test "GameMode1 Line 11459 Answers"
      (get_answers (from_json gamemode1)
         "Name Something A Person Running From The Law Might Not Put On Their \
          Social Networking Site")
      [ [ "Real Name" ]; [ "Address" ]; [ "Picture" ]; [ "Phone Number" ] ];
    get_answers_test "GameMode1 Line 10270"
      (get_answers (from_json gamemode1)
         "Name Something That’s On Your Dinner Table Every Night That The Dog \
          Won’t Beg For.")
      [
        [ "Vegetables/ Salad" ];
        [ "Salt" ];
        [ "Silverware/ Plates" ];
        [ "Napkin/ Tablecloth" ];
      ];
    get_answers_test "GameMode1 Line "
      (get_answers (from_json gamemode1)
         "Name An Article Of Clothing That Many Men Think Women Look \
          Attractive In")
      [ [ "Dress" ]; [ "Mini Skirt" ]; [ "Swimsuit" ]; [ "Underclothes" ] ];
    get_answers_test "GameMode1 line 178"
      (get_answers (from_json gamemode1)
         "Name A Drink You Get At A Cafe That Most People Can’t Make At Home")
      [ [ "Latte" ]; [ "Cappuccino" ]; [ "Mocha" ]; [ "Americano" ] ];
    get_answers_test "Gamemode1 265"
      (get_answers (from_json gamemode1)
         "What Might A Professional Athlete Do Just After A Sports Game To \
          Avoid Getting Sore Muscles?")
      [
        [ "Stretch" ];
        [ "Get A Massage" ];
        [ "Soak In Hot Tub" ];
        [ "Apply Ice" ];
      ];
  ]

let get_point_test (name : string) (input : int) (expected_output : int) =
  name >:: fun _ -> assert_equal expected_output input ~printer:string_of_int

let get_point_tests =
  [
    get_point_test "Q1 GameMode1 Second Answer" 17
      (get_point (from_json gamemode1) "Name Something That Can Be Spoiled"
         "Food");
    get_point_test "Q2 GameMode1 Third Answer"
      (get_point (from_json gamemode1) "Name Something That Can Be Spoiled"
         "Kids")
      13;
    get_point_test "Q2 GameMode1 Fourth Answer"
      (get_point (from_json gamemode1) "Name Something That Can Be Spoiled"
         "A Surprise")
      5;
    get_point_test "Q1 GameMode1 First Answer"
      (get_point (from_json gamemode1) "Name Something That Can Be Spoiled"
         "Milk")
      56;
    get_point_test "Middle Q Line 16824 GameMode1 Last Answer"
      (get_point (from_json gamemode1)
         "If You Boil You Veggies Too Long, Name Something They Lose." "Color")
      6;
    get_point_test "Middle Q Line 16824 GameMode1 First Answer"
      (get_point (from_json gamemode1)
         "If You Boil You Veggies Too Long, Name Something They Lose."
         "Vitamins")
      45;
    get_point_test "Middle Q Line 16824 GameMode1 Second Answer"
      (get_point (from_json gamemode1)
         "If You Boil You Veggies Too Long, Name Something They Lose." "Flavor")
      29;
    get_point_test "Middle Q Line 16824 GameMode1 Third Answer"
      (get_point (from_json gamemode1)
         "If You Boil You Veggies Too Long, Name Something They Lose."
         "Firmness")
      15;
    get_point_test "Second to Last Question GameMode1 Second Answer"
      (get_point (from_json gamemode1)
         "What Do A Barber And Butcher Have In Common?" "Own A Shop")
      30;
    get_point_test "Second to Last Question GameMode1 First Answer"
      (get_point (from_json gamemode1)
         "What Do A Barber And Butcher Have In Common?" "Cutting")
      46;
    get_point_test "Second to Last Question GameMode1 Third Answer"
      (get_point (from_json gamemode1)
         "What Do A Barber And Butcher Have In Common?" "Often Male")
      13;
    get_point_test "Second to Last Question GameMode1 Last Answer"
      (get_point (from_json gamemode1)
         "What Do A Barber And Butcher Have In Common?" "White Coat")
      9;
    get_point_test "Middle Question Line 15983 GameMode1 First Answer"
      (get_point (from_json gamemode1)
         "Name a specific chore that you cannot do without making a lot of \
          noise."
         "Vacuumming")
      61;
    get_point_test "Middle Question Line 15983 GameMode1 Second Answer"
      (get_point (from_json gamemode1)
         "Name a specific chore that you cannot do without making a lot of \
          noise."
         "Dishes")
      16;
    get_point_test "Middle Question Line 15983 GameMode1 Third Answer"
      (get_point (from_json gamemode1)
         "Name a specific chore that you cannot do without making a lot of \
          noise."
         "Laundry")
      10;
    get_point_test "Middle Question Line 15983 GameMode1 Fourt Answer"
      (get_point (from_json gamemode1)
         "Name a specific chore that you cannot do without making a lot of \
          noise."
         "Mowing lawn")
      9;
    get_point_test "Last Question GameMode1 Last Answer"
      (get_point (from_json gamemode1)
         "Name Someplace You’d Have To Be On Really Good Terms To Invite Your \
          Ex."
         "Your House")
      10;
    get_point_test "Last Question GameMode1 First Answer"
      (get_point (from_json gamemode1)
         "Name Someplace You’d Have To Be On Really Good Terms To Invite Your \
          Ex."
         "Your Wedding")
      40;
    get_point_test "Last Question GameMode1 Second Answer"
      (get_point (from_json gamemode1)
         "Name Someplace You’d Have To Be On Really Good Terms To Invite Your \
          Ex."
         "Birthday Party")
      23;
    get_point_test "Last Question GameMode1 Third Answer"
      (get_point (from_json gamemode1)
         "Name Someplace You’d Have To Be On Really Good Terms To Invite Your \
          Ex."
         "Family Reunion")
      21;
    get_point_test "Last Question GameMode1 Last Answer"
      (get_point (from_json gamemode1)
         "Name Someplace You’d Have To Be On Really Good Terms To Invite Your \
          Ex."
         "Your House")
      10;
  ]

let rec list_print lst = match lst with [] -> "" | h :: t -> h ^ list_print t

let rec string_lst_print lst =
  match lst with [] -> "" | h :: t -> list_print h ^ " " ^ string_lst_print t

let filter_tests (name : string) (input1 : Question.t)
    (input2 : string list list) (input3 : string) (input4 : string)
    (expected_output : string list list) =
  name >:: fun _ ->
  assert_equal expected_output (Question.filtered input1 input2 input3 input4)
    ~printer:(fun x -> string_lst_print x)

let filter_test =
  [
    filter_tests "Question 1 answer 1" (from_json questions)
      [
        [ "celebrity"; "celebs" ];
        [ "politician"; "president" ];
        [ "customer service"; "client service" ];
        [ "advice columnist"; "reporters" ];
      ]
      "Name a profession where you may receive lots of mail from stangers"
      "celebs"
      [
        [];
        [ "politician"; "president" ];
        [ "customer service"; "client service" ];
        [ "advice columnist"; "reporters" ];
      ];
    filter_tests "Question 1 answer 2" (from_json questions)
      [
        [];
        [ "politician"; "president" ];
        [ "customer service"; "client service" ];
        [ "advice columnist"; "reporters" ];
      ]
      "Name a profession where you may receive lots of mail from stangers"
      "reporters"
      [
        [];
        [ "politician"; "president" ];
        [ "customer service"; "client service" ];
        [];
      ];
    filter_tests "Question 1 answer 3" (from_json questions)
      [
        [];
        [ "politician"; "president" ];
        [ "customer service"; "client service" ];
        [];
      ]
      "Name a profession where you may receive lots of mail from stangers"
      "president"
      [ []; []; [ "customer service"; "client service" ]; [] ];
    filter_tests "Question 1 answer 4" (from_json questions)
      [ []; []; [ "customer service"; "client service" ]; [] ]
      "Name a profession where you may receive lots of mail from stangers"
      "client service" [ []; []; []; [] ];
    filter_tests "GameMode1 Q1 first answer" (from_json gamemode1)
      [ [ "Milk" ]; [ "Food" ]; [ "Kids" ]; [ "A Surprise" ] ]
      "Name Something That Can Be Spoiled" "Milk"
      [ []; [ "Food" ]; [ "Kids" ]; [ "A Surprise" ] ];
    filter_tests "GameMode1 Q1 second answer" (from_json gamemode1)
      [ []; [ "Food" ]; [ "Kids" ]; [ "A Surprise" ] ]
      "Name Something That Can Be Spoiled" "Food"
      [ []; []; [ "Kids" ]; [ "A Surprise" ] ];
    filter_tests "GameMode1 Q1 third answer" (from_json gamemode1)
      [ []; []; [ "Kids" ]; [ "A Surprise" ] ]
      "Name Something That Can Be Spoiled" "Kids"
      [ []; []; []; [ "A Surprise" ] ];
    filter_tests "GameMode1 Q1 last answer" (from_json gamemode1)
      [ []; []; []; [ "A Surprise" ] ]
      "Name Something That Can Be Spoiled" "A Surprise" [ []; []; []; [] ];
    filter_tests "GameMode1 Last Question last answer" (from_json questions)
      [
        [ "Your Wedding" ];
        [ "Birthday Party" ];
        [ "Family Reunion" ];
        [ "Your House" ];
      ]
      "Name Someplace You’d Have To Be On Really Good Terms To Invite Your Ex."
      "Your House"
      [ [ "Your Wedding" ]; [ "Birthday Party" ]; [ "Family Reunion" ]; [] ];
    filter_tests "GameMode1 Last Question Third answer" (from_json questions)
      [ [ "Your Wedding" ]; [ "Birthday Party" ]; [ "Family Reunion" ]; [] ]
      "Name Someplace You’d Have To Be On Really Good Terms To Invite Your Ex."
      "Family Reunion"
      [ [ "Your Wedding" ]; [ "Birthday Party" ]; []; [] ];
    filter_tests "GameMode1 Last Question First answer" (from_json questions)
      [ [ "Your Wedding" ]; [ "Birthday Party" ]; []; [] ]
      "Name Someplace You’d Have To Be On Really Good Terms To Invite Your Ex."
      "Your Wedding"
      [ []; [ "Birthday Party" ]; []; [] ];
    filter_tests "GameMode1 Last Question Second answer" (from_json questions)
      [ []; [ "Birthday Party" ]; []; [] ]
      "Name Someplace You’d Have To Be On Really Good Terms To Invite Your Ex."
      "Birthday Party" [ []; []; []; [] ];
    filter_tests "GameMode1 Question Line 22943 Second answer"
      (from_json questions)
      [ [ "Pizza" ]; [ "Hamburgers" ]; [ "Sanswiches" ]; [ "Hot Dogs" ] ]
      "What Kind Of Food Should Not Be Served At A Wedding?" "Hamburgers"
      [ [ "Pizza" ]; []; [ "Sanswiches" ]; [ "Hot Dogs" ] ];
    filter_tests "GameMode1 Question Line 22943 Last answer"
      (from_json questions)
      [ [ "Pizza" ]; []; [ "Sanswiches" ]; [ "Hot Dogs" ] ]
      "What Kind Of Food Should Not Be Served At A Wedding?" "Hot Dogs"
      [ [ "Pizza" ]; []; [ "Sanswiches" ]; [] ];
    filter_tests "GameMode1 Question Line 22943 First answer"
      (from_json questions)
      [ [ "Pizza" ]; []; [ "Sanswiches" ]; [] ]
      "What Kind Of Food Should Not Be Served At A Wedding?" "Pizza"
      [ []; []; [ "Sanswiches" ]; [] ];
    filter_tests "GameMode1 Question Line 22943 Wrong answer"
      (from_json questions)
      [ []; []; [ "Sanswiches" ]; [] ]
      "What Kind Of Food Should Not Be Served At A Wedding?" "Things"
      [ []; []; [ "Sanswiches" ]; [] ];
    filter_tests "GameMode1 Question Line 22943 Third answer"
      (from_json questions)
      [ []; []; [ "Sanswiches" ]; [] ]
      "What Kind Of Food Should Not Be Served At A Wedding?" "Sanswiches"
      [ []; []; []; [] ];
    filter_tests "GameMode1 Question Line 13257 First answer"
      (from_json questions)
      [ [ "Pierce" ]; [ "Tatoo" ]; [ "Tan" ]; [ "Shave Their Head" ] ]
      "Name Something People Do To Their Body That Other People Think Is Crazy."
      "Pierce"
      [ []; [ "Tatoo" ]; [ "Tan" ]; [ "Shave Their Head" ] ];
    filter_tests "GameMode1 Question Line 13257 Third answer"
      (from_json questions)
      [ []; [ "Tatoo" ]; [ "Tan" ]; [ "Shave Their Head" ] ]
      "What Kind Of Food Should Not Be Served At A Wedding?" "Tan"
      [ []; [ "Tatoo" ]; []; [ "Shave Their Head" ] ];
    filter_tests "GameMode1 Question Line 13257 Last answer"
      (from_json questions)
      [ []; [ "Tatoo" ]; []; [ "Shave Their Head" ] ]
      "What Kind Of Food Should Not Be Served At A Wedding?" "Shave Their Head"
      [ []; [ "Tatoo" ]; []; [] ];
    filter_tests "GameMode1 Question Line 13257 Second answer"
      (from_json questions)
      [ []; [ "Tatoo" ]; []; [] ]
      "What Kind Of Food Should Not Be Served At A Wedding?" "Tatoo"
      [ []; []; []; [] ];
    filter_tests "GameMode1 Question Line 410 Last answer" (from_json questions)
      [ [ "Monster" ]; [ "Weapon" ]; [ "Ghost" ]; [ "Bad Guy" ] ]
      "In A Scary Movie, Name Something Specific That Causes A Character To \
       Scream"
      "Bad Guy"
      [ [ "Monster" ]; [ "Weapon" ]; [ "Ghost" ]; [] ];
    filter_tests "GameMode1 Question Line 410 First answer"
      (from_json questions)
      [ [ "Monster" ]; [ "Weapon" ]; [ "Ghost" ]; [] ]
      "In A Scary Movie, Name Something Specific That Causes A Character To \
       Scream"
      "Monster"
      [ []; [ "Weapon" ]; [ "Ghost" ]; [] ];
    filter_tests "GameMode1 Question Line 410 Second answer"
      (from_json questions)
      [ []; [ "Weapon" ]; [ "Ghost" ]; [] ]
      "In A Scary Movie, Name Something Specific That Causes A Character To \
       Scream"
      "Weapon"
      [ []; []; [ "Ghost" ]; [] ];
    filter_tests "GameMode1 Question Line 410 Third answer"
      (from_json questions)
      [ []; []; [ "Ghost" ]; [] ]
      "In A Scary Movie, Name Something Specific That Causes A Character To \
       Scream"
      "Ghost" [ []; []; []; [] ];
    filter_tests "GameMode1 Question Line 25843 second answer"
      (from_json questions)
      [ [ "Party" ]; [ "Drink" ]; [ "Go Naked" ]; [ "Sleep All Day" ] ]
      "Name Something People Do On Spring Break, But Not On An Ordinary \
       Vacation."
      "Drink"
      [ [ "Party" ]; []; [ "Go Naked" ]; [ "Sleep All Day" ] ];
    filter_tests "GameMode1 Question Line 25843 Third answer"
      (from_json questions)
      [ [ "Party" ]; []; [ "Go Naked" ]; [ "Sleep All Day" ] ]
      "Name Something People Do On Spring Break, But Not On An Ordinary \
       Vacation."
      "Go Naked"
      [ [ "Party" ]; []; []; [ "Sleep All Day" ] ];
    filter_tests "GameMode1 Question Line 25843 second answer wrong"
      (from_json questions)
      [ [ "Party" ]; []; []; [ "Sleep All Day" ] ]
      "Name Something People Do On Spring Break, But Not On An Ordinary \
       Vacation."
      "Drink"
      [ [ "Party" ]; []; []; [ "Sleep All Day" ] ];
    filter_tests "GameMode1 Question Line 25843 Last answer"
      (from_json questions)
      [ [ "Party" ]; []; []; [ "Sleep All Day" ] ]
      "Name Something People Do On Spring Break, But Not On An Ordinary \
       Vacation."
      "Sleep All Day"
      [ [ "Party" ]; []; []; [] ];
  ]

let fuzzy_test (name : string) (input1 : string) (input2 : string)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (is_similar_one_word input1 input2)
    ~printer:string_of_bool

let fuzzy_test2 (name : string) (input1 : string) (input2 : string)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output
    (is_similar_mult_words input1 input2)
    ~printer:string_of_bool

let fuzzy_tests =
  [
    fuzzy_test "substring" "Kids" "my kids" true;
    fuzzy_test "case" "Milk" "milk" true;
    fuzzy_test "false case" "Energy" "enegy" false;
    fuzzy_test "true case" "Energy" "LOTS of ENERGY" true;
    fuzzy_test "true walk case" "Walk" "Going for a walk" true;
    fuzzy_test "true walk case with walks" "Walk" "Going for a walks" true;
    fuzzy_test "false walk case" "Walk" "Going for walking" true;
    fuzzy_test "false walk case" "Walk" "run" false;
    fuzzy_test2 "true multi word" "Like The Taste" "like the taste " true;
    fuzzy_test2 "true multi word fuzzy" "Like The Taste" "Like a Taste" true;
    fuzzy_test2 "true multi word not exact" "Your House" "House" true;
    fuzzy_test2 "true multi word" "Family Reunion " "family reunion" true;
    fuzzy_test2 "trim two words" "Like The Taste" "like the taste " true;
    fuzzy_test2 "incorrect multi word" "Like The Taste" "The" false;
    fuzzy_test2 "correct fuzzy subword" "Like The Taste" "tastes amazing" true;
    fuzzy_test2 "totally incorrect" "Like The Taste" "huh" false;
  ]

let parse_answers_test (name : string) (input1 : Question.t)
    (input2 : string list list) (input3 : string) (input4 : string)
    (expected_output : bool * string) =
  name >:: fun _ ->
  assert_equal expected_output (parse_answers1 input1 input2 input3 input4)

let parse_answers_tests =
  [
    parse_answers_test "GameMode Q1 correct, not fuzzy" (from_json gamemode1)
      [ [ "Milk" ]; [ "Food" ]; [ "Kids" ]; [ "A Surprise" ] ]
      "Name Something That Can Be Spoiled" "Milk" (true, "Milk");
    parse_answers_test "GameMode Q1 correct, fuzzy" (from_json gamemode1)
      [ [ "Milk" ]; [ "Food" ]; [ "Kids" ]; [ "A Surprise" ] ]
      "Name Something That Can Be Spoiled" "milks" (true, "Milk");
    parse_answers_test " GameMode Q1 incorrect" (from_json gamemode1)
      [ [ "Milk" ]; [ "Food" ]; [ "Kids" ]; [ "A Surprise" ] ]
      "Name Something That Can Be Spoiled" "eggs" (false, "eggs");
    parse_answers_test "Last Question correct two words" (from_json gamemode1)
      [
        [ "Your Wedding" ];
        [ "Birthday Party" ];
        [ "Family Reunion" ];
        [ "Your House" ];
      ]
      "Name Someplace You’d Have To Be On Really Good Terms To Invite Your Ex."
      "Birthday Party" (true, "Birthday Party");
    parse_answers_test "Last Question fuzzy two words" (from_json gamemode1)
      [
        [ "Your Wedding" ];
        [ "Birthday Party" ];
        [ "Family Reunion" ];
        [ "Your House" ];
      ]
      "Name Someplace You’d Have To Be On Really Good Terms To Invite Your Ex."
      "  birthday Party" (true, "Birthday Party");
    parse_answers_test "Last Q correct two words fuzzy" (from_json gamemode1)
      [
        [ "Your Wedding" ];
        [ "Birthday Party" ];
        [ "Family Reunion" ];
        [ "Your House" ];
      ]
      "Name Someplace You’d Have To Be On Really Good Terms To Invite Your Ex."
      "party " (true, "Birthday Party");
    parse_answers_test "incorrect two words" (from_json gamemode1)
      [ [ "Sleep" ]; [ "Cook" ]; [ "Eat" ]; [ "Use Bathroom" ] ]
      "Name Something You Usually Do Indoors, But Might Do Outdoors When \
       Camping."
      "use" (false, "use");
    parse_answers_test "incorrect partly filtered list" (from_json gamemode1)
      [ [ "Monday" ]; [ "Thursday" ]; [ "Wednesday" ]; [] ]
      "Tell Me The Day Of The Week When You Start Thinking About The Weekend."
      "Tuesday" (false, "Tuesday");
    parse_answers_test "empty list" (from_json gamemode1) [ [] ]
      "Tell Me The Day Of The Week When You Start Thinking About The Weekend."
      "Tuesday" (false, "Tuesday");
    parse_answers_test "Line 15171 Question correct answer"
      (from_json gamemode1)
      [
        [ "Helmet" ];
        [ "Leather Pants" ];
        [ "Leather Jacket" ];
        [ "Sunglasses" ];
      ]
      "Name Something A Person Might Buy To Go With Their Motorcycle." "Helmet"
      (true, "Helmet");
    parse_answers_test "Line 15171 Question incorrect answer"
      (from_json gamemode1)
      [
        [ "Helmet" ];
        [ "Leather Pants" ];
        [ "Leather Jacket" ];
        [ "Sunglasses" ];
      ]
      "Name Something A Person Might Buy To Go With Their Motorcycle." "Wheels"
      (false, "Wheels");
    parse_answers_test "Line 15171 Question filtered correct answer"
      (from_json gamemode1)
      [ [ "Helmet" ]; [ "Leather Pants" ]; [ "Leather Jacket" ]; [] ]
      "Name Something A Person Might Buy To Go With Their Motorcycle." "Helmet"
      (true, "Helmet");
    parse_answers_test "Line 15171 Question filtered incorrect answer"
      (from_json gamemode1)
      [ [ "Helmet" ]; [ "Leather Pants" ]; [ "Leather Jacket" ]; [] ]
      "Name Something A Person Might Buy To Go With Their Motorcycle." "Shorts"
      (false, "Shorts");
    parse_answers_test "Line 15171 Question fuzzy correct answer second half"
      (from_json gamemode1)
      [
        [ "Helmet" ];
        [ "Leather Pants" ];
        [ "Leather Jacket" ];
        [ "Sunglasses" ];
      ]
      "Name Something A Person Might Buy To Go With Their Motorcycle." "pants"
      (true, "Leather Pants");
    parse_answers_test "Line 15171 Question fuzzy correct answer first half"
      (from_json gamemode1)
      [
        [ "Helmet" ];
        [ "Leather Pants" ];
        [ "Leather Jacket" ];
        [ "Sunglasses" ];
      ]
      "Name Something A Person Might Buy To Go With Their Motorcycle." "leather"
      (true, "Leather Pants");
    parse_answers_test "Line 15171 Question fuzzy incorrect answer"
      (from_json gamemode1)
      [
        [ "Helmet" ];
        [ "Leather Pants" ];
        [ "Leather Jacket" ];
        [ "Sunglasses" ];
      ]
      "Name Something A Person Might Buy To Go With Their Motorcycle." "cargo"
      (false, "cargo");
    parse_answers_test "Line 10415 Question answer almost empty list"
      (from_json gamemode1)
      [ [ "Sugar" ]; []; []; [] ]
      "Name An Unhealthy Ingredient You Often See In Kids’ Cereals." "Sugar"
      (true, "Sugar");
    parse_answers_test "Line 10415 Question answer wrong almost empty list"
      (from_json gamemode1)
      [ []; [ "Chocolate" ]; []; [] ]
      "Name An Unhealthy Ingredient You Often See In Kids’ Cereals." "Sugar"
      (false, "Sugar");
    parse_answers_test "Line 10415 Question 2 word wrong answer"
      (from_json gamemode1)
      [ [ "Sugar" ]; [ "Chocolate" ]; [ "Corn Syrup" ]; [ "Salt" ] ]
      "Name An Unhealthy Ingredient You Often See In Kids’ Cereals."
      "Green Apple" (false, "Green Apple");
    parse_answers_test "Line 10415 Question answer 2 word right fuzzy"
      (from_json gamemode1)
      [ [ "Sugar" ]; [ "Chocolate" ]; [ "Corn Syrup" ]; [ "Salt" ] ]
      "Name An Unhealthy Ingredient You Often See In Kids’ Cereals."
      "Heavy Syrup" (true, "Corn Syrup");
    parse_answers_test "Line 10415 Question answer 1 word right fuzzy"
      (from_json gamemode1)
      [ [ "Sugar" ]; [ "Chocolate" ]; [ "Corn Syrup" ]; [ "Salt" ] ]
      "Name An Unhealthy Ingredient You Often See In Kids’ Cereals."
      "chocolates" (true, "Chocolate");
    parse_answers_test "Line 5572 GameMode1 Question correct answer "
      (from_json gamemode1)
      [ [ "Sick" ]; [ "Car Trouble" ]; [ "Traffic" ]; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "Traffic" (true, "Traffic");
    parse_answers_test "Line 5572 GameMode1 Question correct typo answer "
      (from_json gamemode1)
      [ [ "Sick" ]; [ "Car Trouble" ]; [ "Traffic" ]; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "Traffiwc" (true, "Traffic");
    parse_answers_test "Line 5572 GameMode1 Question correct typo 2 word answer"
      (from_json gamemode1)
      [ [ "Sick" ]; [ "Car Trouble" ]; [ "Traffic" ]; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "catr ttrouble" (true, "Car Trouble");
    parse_answers_test
      "Line 5572 GameMode1 Question correct answer partial empty list"
      (from_json gamemode1)
      [ []; [ "Car Trouble" ]; []; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "oversleppt" (true, "Overslept");
    parse_answers_test "Line 5572 GameMode1 Question correct answer backwards"
      (from_json gamemode1)
      [ [ "Sick" ]; [ "Car Trouble" ]; []; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "kcis" (true, "Sick");
    parse_answers_test
      "Line 5572 GameMode1 Question 1 word correct answer fuzzy backwards"
      (from_json gamemode1)
      [ [ "Sick" ]; [ "Car Trouble" ]; []; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "elbourt" (true, "Car Trouble");
    parse_answers_test
      "Line 5572 GameMode1 Question 1 word correct answer fuzzy backwards"
      (from_json gamemode1)
      [ [ "Sick" ]; [ "Car Trouble" ]; []; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "elbourt rac" (true, "Car Trouble");
    parse_answers_test
      "Line 5572 GameMode1 Question 1 word typo answer fuzzy backwards"
      (from_json gamemode1)
      [ [ "Sick" ]; [ "Car Trouble" ]; []; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "tkics" (true, "Sick");
    parse_answers_test
      "Line 5572 GameMode1 Question 1 word typo answer fuzzy backwards"
      (from_json gamemode1)
      [ [ "Sick" ]; [ "Car Trouble" ]; []; [ "Overslept" ] ]
      "Tell Me An Excuse For Not Going To Work That No One Really Believes"
      "tkics" (true, "Sick");
    parse_answers_test "Line 6471 GameMode1 3 word correct answer"
      (from_json gamemode1)
      [
        [ "Barking Dog" ];
        [ "No One’s Home" ];
        [ "No Soliciting Sign" ];
        [ "Run Down House" ];
      ]
      "Why Might A Door-To-Door Salesperson Skip A Particular House?"
      "Run Down House" (true, "Run Down House");
    parse_answers_test "Line 6471 GameMode1 3 word typo correct answer"
      (from_json gamemode1)
      [
        [ "Barking Dog" ];
        [ "No One’s Home" ];
        [ "No Soliciting Sign" ];
        [ "Run Down House" ];
      ]
      "Why Might A Door-To-Door Salesperson Skip A Particular House?"
      "No Soliciting Signt"
      (true, "No Soliciting Sign");
    parse_answers_test "Line 6471 GameMode1 3 word incorrect answer empty list"
      (from_json gamemode1)
      [ []; []; []; [ "Run Down House" ] ]
      "Why Might A Door-To-Door Salesperson Skip A Particular House?"
      "No Soliciting Sign"
      (false, "No Soliciting Sign");
    parse_answers_test "Line 26394 Questions wrong" (from_json gamemode1)
      [ [ "Partner" ]; [ "Family" ]; [ "Boss" ]; [ "Teacher" ] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "Child" (false, "Child");
    parse_answers_test "Line 26394 Questions wrong with partial empty list"
      (from_json gamemode1)
      [ [ "Partner" ]; [ "Family" ]; []; [] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "Adult" (false, "Adult");
    parse_answers_test "Line 26394 Questions wrong with completely empty list"
      (from_json gamemode1) [ []; []; []; [] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "Boss" (false, "Boss");
    parse_answers_test "Line 26394 Questions right with almost empty list"
      (from_json gamemode1)
      [ [ "Partner" ]; []; []; [] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "partner" (true, "Partner");
    parse_answers_test "Line 26394 Questions fuzzy right with word in middle"
      (from_json gamemode1)
      [ [ "Partner" ]; [ "Family" ]; [ "Boss" ]; [ "Teacher" ] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "wowfamilywow" (true, "Family");
    parse_answers_test "Line 26394 Questions fuzzy right with word in middle"
      (from_json gamemode1)
      [ [ "Partner" ]; [ "Family" ]; [ "Boss" ]; [ "Teacher" ] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "wfamilyq" (true, "Family");
    parse_answers_test "Line 26394 Questions plural right" (from_json gamemode1)
      [ [ "Partner" ]; [ "Family" ]; [ "Boss" ]; [ "Teacher" ] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "bosses" (true, "Boss");
    parse_answers_test "Line 26394 Questions wrong" (from_json gamemode1)
      [ [ "Partner" ]; [ "Family" ]; [ "Boss" ]; [ "Teacher" ] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "rulers" (false, "rulers");
    parse_answers_test "Line 26394 Questions right answer" (from_json gamemode1)
      [ [ "Partner" ]; [ "Family" ]; [ "Boss" ]; [ "Teacher" ] ]
      "Name A Person In Your Life That You’d Be Mortified To Call By The Wrong \
       Name."
      "Teacher" (true, "Teacher");
    parse_answers_test "Line 26568 Questions wrong with typo"
      (from_json gamemode1)
      [ [ "Rodeo" ]; [ "Saloon" ]; [ "Farm" ]; [ "Texas" ] ]
      "If You Wanted To Marry A Cowboy Where Would You Start Hanging Out?"
      "parm" (false, "parm");
    parse_answers_test "Line 26568 Questions wrong completely"
      (from_json gamemode1)
      [ [ "Rodeo" ]; [ "Saloon" ]; [ "Farm" ]; [ "Texas" ] ]
      "If You Wanted To Marry A Cowboy Where Would You Start Hanging Out?"
      "not sure" (false, "not sure");
    parse_answers_test "Line 26568 Questions right" (from_json gamemode1)
      [ [ "Rodeo" ]; [ "Saloon" ]; [ "Farm" ]; [ "Texas" ] ]
      "If You Wanted To Marry A Cowboy Where Would You Start Hanging Out?"
      "texas" (true, "Texas");
    parse_answers_test "Line 26568 Questions second answer"
      (from_json gamemode1)
      [ [ "Rodeo" ]; [ "Saloon" ]; [ "Farm" ]; [ "Texas" ] ]
      "If You Wanted To Marry A Cowboy Where Would You Start Hanging Out?"
      "Saloon" (true, "Saloon");
    parse_answers_test "Line 26568 Questions third answer extra letter"
      (from_json gamemode1)
      [ [ "Rodeo" ]; [ "Saloon" ]; [ "Farm" ]; [ "Texas" ] ]
      "If You Wanted To Marry A Cowboy Where Would You Start Hanging Out?"
      "farmt" (true, "Farm");
    parse_answers_test "Line 27351 Questions First" (from_json gamemode1)
      [ [ "Dog" ]; [ "Each Other" ]; [ "TV" ]; [ "Worry" ] ]
      "Besides Their Kids, Name Something That Might Keep New Parents Awake At \
       Night."
      "Dog" (true, "Dog");
    parse_answers_test "Line 27351 Questions Second" (from_json gamemode1)
      [ [ "Dog" ]; [ "Each Other" ]; [ "TV" ]; [ "Worry" ] ]
      "Besides Their Kids, Name Something That Might Keep New Parents Awake At \
       Night."
      "each other" (true, "Each Other");
    parse_answers_test "Line 27351 Questions Third" (from_json gamemode1)
      [ [ "Dog" ]; []; [ "TV" ]; [ "Worry" ] ]
      "Besides Their Kids, Name Something That Might Keep New Parents Awake At \
       Night."
      "TV" (true, "TV");
    parse_answers_test "Line 27351 Questions last" (from_json gamemode1)
      [ []; []; []; [ "Worry" ] ]
      "Besides Their Kids, Name Something That Might Keep New Parents Awake At \
       Night."
      "Worry worm" (true, "Worry");
    parse_answers_test "Line 27351 Questions no answers" (from_json gamemode1)
      [ []; []; []; [] ]
      "Besides Their Kids, Name Something That Might Keep New Parents Awake At \
       Night."
      "Books" (false, "Books");
    parse_answers_test "Line 27351 Questions no answers right answer"
      (from_json gamemode1) [ []; []; []; [] ]
      "Besides Their Kids, Name Something That Might Keep New Parents Awake At \
       Night."
      "Worry" (false, "Worry");
  ]

let curr_score_test (name : string) (input1 : Question.t) (input2 : string)
    (input3 : string) (input4 : int) (expected_output : int) =
  name >:: fun _ ->
  assert_equal expected_output
    (calculate_current_score input1 input2 input3 input4)
    ~printer:string_of_int

let curr_score_tests =
  [
    curr_score_test "initial of 0 correct input" (from_json gamemode1)
      "In A Scary Movie, Name Something Specific That Causes A Character To \
       Scream"
      "Monster" 0 38;
    curr_score_test "incorrect answer" (from_json gamemode1)
      "In A Scary Movie, Name Something Specific That Causes A Character To \
       Scream"
      "Zombie" 38 38;
    curr_score_test "another correct" (from_json gamemode1)
      "In A Scary Movie, Name Something Specific That Causes A Character To \
       Scream"
      "Ghost" 38 59;
    curr_score_test "another question" (from_json gamemode1)
      "Name A Sport That People Bet On" "Boxing" 5 11;
    curr_score_test "Line 6471 Question first answer" (from_json gamemode1)
      "Why Might A Door-To-Door Salesperson Skip A Particular House?"
      "Barking Dog" 100 146;
    curr_score_test "Line 6471 Question second answer" (from_json gamemode1)
      "Why Might A Door-To-Door Salesperson Skip A Particular House?"
      "No One’s Home" 146 172;
    curr_score_test "Line 6471 Question second answer" (from_json gamemode1)
      "Why Might A Door-To-Door Salesperson Skip A Particular House?"
      "No Soliciting Sign" 172 189;
    curr_score_test "Line 6471 Question second answer" (from_json gamemode1)
      "Why Might A Door-To-Door Salesperson Skip A Particular House?"
      "Run Down House" 189 196;
    curr_score_test "Line 14011 Question First answer" (from_json gamemode1)
      "Name A Type Of Food That Could Most Easily Be Turned Into A Heart-Shape \
       For Valentine’s Day."
      "Cake" 0 34;
    curr_score_test "Line 14011 Question wrong answer" (from_json gamemode1)
      "Name A Type Of Food That Could Most Easily Be Turned Into A Heart-Shape \
       For Valentine’s Day."
      "Doughnut" 34 34;
    curr_score_test "Line 14011 Question second answer" (from_json gamemode1)
      "Name A Type Of Food That Could Most Easily Be Turned Into A Heart-Shape \
       For Valentine’s Day."
      "Cookies" 34 63;
    curr_score_test "Line 14011 Question wrong second answer"
      (from_json gamemode1)
      "Name A Type Of Food That Could Most Easily Be Turned Into A Heart-Shape \
       For Valentine’s Day."
      "apples" 63 63;
    curr_score_test "Line 14011 Question third answer" (from_json gamemode1)
      "Name A Type Of Food That Could Most Easily Be Turned Into A Heart-Shape \
       For Valentine’s Day."
      "Sandwich/Bread" 63 83;
    curr_score_test "Line 14011 Question wrong third answer"
      (from_json gamemode1)
      "Name A Type Of Food That Could Most Easily Be Turned Into A Heart-Shape \
       For Valentine’s Day."
      "juice" 83 83;
    curr_score_test "Line 14011 Question fourth answer" (from_json gamemode1)
      "Name A Type Of Food That Could Most Easily Be Turned Into A Heart-Shape \
       For Valentine’s Day."
      "Pancakes" 83 97;
    curr_score_test "Line 27177 Question wrong answer " (from_json gamemode1)
      "Name Something A Kid Might Get To Do When Sleeping Over At A Friend’s \
       House That He Doesn’t Do At Home"
      "Play" 0 0;
    curr_score_test "Line 27177 Question right answer " (from_json gamemode1)
      "Name Something A Kid Might Get To Do When Sleeping Over At A Friend’s \
       House That He Doesn’t Do At Home"
      "Stay Up" 120 205;
    curr_score_test "Line 27177 Question wrong answer " (from_json gamemode1)
      "Name Something A Kid Might Get To Do When Sleeping Over At A Friend’s \
       House That He Doesn’t Do At Home"
      "Have fun" 123 123;
  ]

let is_random_question (prev_question : string) =
  random_question (from_json gamemode1) <> prev_question

let random_question_test (name : string) (input : string)
    (expected_output : bool) =
  name >:: fun _ ->
  assert_equal expected_output (is_random_question input)
    ~printer:string_of_bool

let random_question_tests =
  [
    (let rq1 = random_question (from_json gamemode1) in
     random_question_test "first question test" rq1 true);
    random_question_test "initial question in file"
      "Name Something That Can Be Spoiled" true;
    random_question_test "last question in file"
      "Name Someplace You’d Have To Be On Really Good Terms To Invite Your Ex."
      true;
  ]

let tests =
  "final project test suite"
  >::: List.flatten
         [
           get_point_tests;
           filter_test;
           fuzzy_tests;
           curr_score_tests;
           random_question_tests;
           parse_answers_tests;
           get_answers_tests;
         ]

let _ = run_test_tt_main tests
