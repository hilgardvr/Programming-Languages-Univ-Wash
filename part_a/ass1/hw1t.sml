(* Homework1 Simple Test *)
(* These are basic test cases. Passing these tests does not guarantee that your code will pass the actual homework grader *)
(* To run the test, add a new line to the top of this file: use "homeworkname.sml"; *)
(* All the tests should evaluate to true. For example, the REPL should say: val test1 = true : bool *)


val test1_0 = is_older ((1,2,3),(2,3,4)) = true
val test1_1 = is_older ((2,3,3),(2,3,4)) = true
val test1_2 = is_older ((2,3,3),(2,3,1)) = false 

val test2_0 = number_in_month ([(2012,2,28),(2013,12,1)],2) = 1
val test2_1 = number_in_month ([(2012,2,28),(2013,12,1),(1999,2,1)],2) = 2
val test2_2 = number_in_month ([(2012,2,28),(2013,12,1)],3) = 0

val test3_0 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = 3
val test3_1 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2]) = 1
val test3_2 = number_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[7]) = 0

val test4_0 = dates_in_month ([(2012,2,28),(2013,12,1)],2) = [(2012,2,28)]
val test4_1 = dates_in_month ([(2012,2,28),(2013,12,1),(2014,2,1)],2) = [(2012,2,28),(2014,2,1)]
val test4_2 = dates_in_month ([(2012,2,28),(2013,12,1)],1) = []

val test5_0 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_1 = dates_in_months ([(2919,10,1),(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[10,2,3,4]) = [(2919,10,1),(2012,2,28),(2011,3,31),(2011,4,28)]
val test5_2 = dates_in_months ([(2012,2,28),(2013,12,1),(2011,3,31),(2011,4,28)],[2,3,4]) = [(2012,2,28),(2011,3,31),(2011,4,28)]

val test6_0 = get_nth (["hi", "there", "how", "are", "you"], 2) = "there"
val test6_1 = get_nth (["hi", "there", "how", "are", "you"], 1) = "hi"
val test6_2 = get_nth (["hi", "there", "how", "are", "you"], 5) = "you"

val test7_0 = date_to_string (2013, 6, 1) = "June 1, 2013"

val test8_0 = number_before_reaching_sum (10, [1,2,3,4,5]) = 3
val test8_1 = number_before_reaching_sum (0, [1,2,3,4,5]) = 0
val test8_2 = number_before_reaching_sum (15, [1,2,3,4,5,6]) = 4

val test9_0 = what_month 70 = 3
val test9_1 = what_month 10 = 1
val test9_2 = what_month 365 = 12

val test10_0 = month_range (31, 34) = [1,2,2,2]
val test10_1 = month_range (29, 34) = [1,1,1,2,2,2]
val test10_2 = month_range (334, 334) = [11]

val test11_0 = oldest([(2012,2,28),(2011,3,31),(2011,4,28)]) = SOME (2011,3,31)
val test11_1 = oldest([]) = NONE
val test11_2 = oldest([(2012,2,28),(2011,3,31),(2010,4,28)]) = SOME (2010,4,28)
