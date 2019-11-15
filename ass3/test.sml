(* Homework3 Simple Test*)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)

val test1_0 = only_capitals ["A","B","C"] = ["A","B","C"]
val test1_1 = only_capitals ["Asad","bJHJ","Cat"] = ["Asad","Cat"]
val test1_2 = only_capitals ["a","b","c"] = []

val test2_0 = longest_string1 ["A","bc","C"] = "bc"
val test2_1 = longest_string1 ["A","bc","cC"] = "bc"
val test2_2 = longest_string1 ["A","bc","Ccc"] = "Ccc"
val test2_3 = longest_string1 [] = ""

val test3_0 = longest_string2 ["A","bc","C"] = "bc"
val test3_1 = longest_string2 ["A","bc","Cc"] = "Cc"
val test3_2 = longest_string1 ["A","bc","Ccc"] = "Ccc"
val test3_3 = longest_string1 [] = ""

val test4_0 = longest_string3 [] = ""
val test4_1 = longest_string3 ["A","bc","C"] = "bc"
val test4_2 = longest_string3 ["A","B","C"] = "A"
val test4_3 = longest_string4 ["A","B","C"] = "C"
val test4_4 = longest_string4 ["A","B","Ccc"] = "Ccc"

val test5_0 = longest_capitalized ["A","bc","C"] = "A"
val test5_1 = longest_capitalized ["A","Bc","C"] = "Bc"
val test5_2 = longest_capitalized ["A","Bc","Cc"] = "Bc"

val test6_0 = rev_string "abc" = "cba"
val test6_1 = rev_string "cba" = "abc"
val test6_2 = rev_string "abc" = "cba"

val test7_0 = first_answer (fn x => if x > 3 then SOME x else NONE) [1,2,3,4,5] = 4
val test7_1 = first_answer (fn x => if x < 3 then SOME x else NONE) [1,2,3,4,5] = 1
(*val test7_2 = first_answer (fn x => if x > 5 then SOME x else NONE) [1,2,3,4,5] = 2 *)

val test8_0 = all_answers (fn x => if x = 1 then SOME [x] else NONE) [2,3,4,5,6,7] = NONE
val test8_1 = all_answers (fn x => if x > 1 then SOME [x] else NONE) [2,3,4,5,6,7] = SOME [2,3,4,5,6,7]

val test9a_0 = count_wildcards Wildcard = 1
val test9a_1 = count_wildcards UnitP = 0
val test9a_2 = count_wildcards (TupleP ([Wildcard, UnitP, UnitP, Wildcard])) = 2
val test9a_3 = count_wildcards (ConstructorP ("str", TupleP [UnitP, Wildcard, Wildcard])) = 2

val test9b_0 = count_wild_and_variable_lengths (Variable("a")) = 1
val test9b_1 = count_wild_and_variable_lengths (TupleP ([Wildcard, Variable("ab"), Variable("a")])) = 4
val test9b_2 = count_wild_and_variable_lengths (UnitP) = 0
val test9b_3 = count_wild_and_variable_lengths (Variable("abc")) = 3

val test9c_0 = count_some_var ("x", Variable("x")) = 1
val test9c_1 = count_some_var ("x", Variable("r")) = 0
val test9c_2 = count_some_var ("x", TupleP ([Variable("x"), Variable("x"), Variable("s"), Variable("x")])) = 3
val test9c_3 = count_some_var ("x", Variable("x")) = 1

val test10_0 = check_pat (TupleP ([Variable("a"), Variable("b"), Variable("c"), Variable("d"), ConstructorP ("e", UnitP)])) = true
val test10_1 = check_pat (Variable("x")) = true
val test10_2 = check_pat (TupleP ([Variable("s"), Variable("d"), ConstructorP ("s", UnitP)])) = false
val test10_3 = check_pat (TupleP ([Variable("s"), Variable("s")])) = false

val test11 = match (Const(1), UnitP) = NONE

(*val test12 = first_match Unit [UnitP] = SOME []
*)
