(* Homework2 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_0 = all_except_option ("string", ["string"]) = SOME []
val test1_1 = all_except_option ("string", ["strin"]) = NONE
val test1_2 = all_except_option ("string", ["string", "test", "t2"]) = SOME ["t2", "test"] 
val test1_3 = all_except_option ("strin", ["a", "strin", "b"]) = SOME ["b", "a"]

val test2_0 = get_substitutions1 ([["foo"],["there"]], "foo") = []
val test2_1 = get_substitutions1 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Jeffrey","Geoff"]
val test2_2 = get_substitutions1 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") =  ["Fredrick","F","Freddie"]

val test3_0 = get_substitutions2 ([["foo"],["there"]], "foo") = []
val test3_1 = get_substitutions2 ([["Fred","Fredrick"],["Jeff","Jeffrey"],["Geoff","Jeff","Jeffrey"]],"Jeff") = ["Jeffrey","Geoff","Jeffrey"]
val test3_2 = get_substitutions2 ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]],"Fred") =  ["F","Freddie","Fredrick"]


val test4_0 = similar_names ([["Fred","Fredrick"],["Elizabeth","Betty"],["Freddie","Fred","F"]], {first="Fred", middle="W", last="Smith"}) =
	    [{first="Fredrick", last="Smith", middle="W"},
	     {first="Freddie", last="Smith", middle="W"}, {first="F", last="Smith", middle="W"}, {first="Fred", last="Smith", middle="W"}]

val test5_0 = card_color (Clubs, Num 2) = Black
val test5_1 = card_color (Spades, Num 10) = Black
val test5_2 = card_color (Diamonds, Num 2) = Red

val test6_0 = card_value (Clubs, Num 2) = 2
val test6_1 = card_value (Clubs, Ace) = 11
val test6_2 = card_value (Clubs, King) = 10

val test7_0 = remove_card ([(Hearts, Ace)], (Hearts, Ace), IllegalMove) = []
val test7_1 = remove_card ([(Hearts, Ace), (Clubs, Ace)], (Hearts, Ace), IllegalMove) = [(Clubs, Ace)]
fun helper (cs, c, e) =
    remove_card(cs,c,e)
    handle IllegalMove => []
val test7_2 = helper ([(Hearts, Ace)], (Hearts, King), IllegalMove) = []

val test8_0 = all_same_color [(Hearts, Ace), (Hearts, Ace)] = true
val test8_1 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Diamonds, Num 3)] = true
val test8_2 = all_same_color [(Hearts, Ace), (Hearts, Ace), (Clubs, King)] = false
val test8_3 = all_same_color [] = true
val test8_4 = all_same_color [(Hearts, Ace)] = true

val test9_0 = sum_cards [(Clubs, Num 2),(Clubs, Num 2)] = 4
val test9_1 = sum_cards [(Clubs, Num 4),(Clubs, Ace)] = 15
val test9_2 = sum_cards [(Clubs, King),(Clubs, Jack)] = 20

val test10_0 = score ([(Hearts, Num 2),(Clubs, Num 4)],10) = 4
val test10_1 = score ([(Hearts, Num 2),(Diamonds, Num 4)],10) = 2
val test10_2 = score ([(Hearts, Num 2),(Clubs, Num 9)],10) = 3
val test10_3 = score ([(Hearts, Num 2),(Diamonds, Num 9)],10) = 1

val test11 = officiate ([(Hearts, Num 2),(Clubs, Num 4)],[Draw], 15) = 6

val test12 = officiate ([(Clubs,Ace),(Spades,Ace),(Clubs,Ace),(Spades,Ace)],
                        [Draw,Draw,Draw,Draw,Draw],
                        42)
             = 3

val test13 = ((officiate([(Clubs,Jack),(Spades,Num(8))],
                         [Draw,Discard(Hearts,Jack)],
                         42);
               false) 
              handle IllegalMove => true)
