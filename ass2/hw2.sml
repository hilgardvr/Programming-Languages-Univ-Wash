(* Dan Grossman, Coursera PL, HW2 Provided Code *)

(* if you use this function to compare two strings (returns true if the same
   string), then you avoid several of the functions in problem 1 having
   polymorphic types that may be confusing *)
fun same_string(s1 : string, s2 : string) =
    s1 = s2

(* put your solutions for problem 1 here *)

fun all_except_option(str : string, lst : string list) =
    let
        fun lst_builder(str, lst, ret, found) =
            case lst of
            [] => if found
                    then SOME ret
                    else NONE
            | h::t => if same_string (h, str)
                    then lst_builder (str, t, ret, true)
                    else lst_builder (str, t, h :: ret, found) 
    in
        lst_builder(str, lst, [], false)
    end

fun get_substitutions1 (lstlst, str) =
    case lstlst of
    [] => []
    | h::t => case all_except_option(str, h) of
        NONE => get_substitutions1 (t, str)
        | SOME lst => lst @ get_substitutions1 (t, str)

fun get_substitutions2 (lstlst, str) =
    let
        fun lst_builder(lstlst, str, acc) =
            case lstlst of
            [] => acc
            | h::t =>
                case all_except_option(str, h) of
                    NONE => lst_builder(t, str, acc)
                    | SOME lst => lst_builder(t, str, lst @ acc)
    in
        lst_builder(lstlst, str, [])
    end

fun similar_names (lstlst, {first=f, middle=m, last=l}) =
    let
        fun helper (lst,  acc) =
            case lst of
                [] => acc
                | h::t => helper (t, {first = h, middle = m, last = l} :: acc)
    in
        helper (get_substitutions2 (lstlst, f), [{first = f, middle = m, last =l}])
    end
(*val get_substitutions2 = get_substitutions1*)

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)
