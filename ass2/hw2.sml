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

(* you may assume that Num is always used with values 2, 3, ..., 10
   though it will not really come up *)
datatype suit = Clubs | Diamonds | Hearts | Spades
datatype rank = Jack | Queen | King | Ace | Num of int 
type card = suit * rank

datatype color = Red | Black
datatype move = Discard of card | Draw 

exception IllegalMove

(* put your solutions for problem 2 here *)

fun card_color (c) =
    case c of
    (Clubs, _) => Black
    | (Spades, _) => Black
    | _ => Red

fun card_value (c) =
    case c of
    (_, Ace) => 11
    | (_, Num num) => num
    | _ => 10

fun remove_card (cs, c, e) =
    let
        fun helper (cs, found, acc) =
            case cs of
            [] => if found
                then acc
                else raise e
            | h::t => if h = c
                then helper (t, true, acc)
                else helper (t, found, h::acc)
    in
        helper (cs, false, [])
    end


fun all_same_color (cs) =
    case cs of
    [] => true
    | h::[] => true
    | h::n::t => if card_color(h) = card_color(n)
                    then all_same_color(n::t)
                    else false 

fun sum_cards (cs) =
    let
        fun helper (cs, acc) =
            case cs of
            [] => acc
            | h::t => helper(t, card_value(h) + acc)
    in
        helper (cs, 0)
    end

fun score (cs, goal) =
    let
        val sum = sum_cards(cs)
        val prelim = if sum > goal
            then 3 * (sum - goal)
            else goal - sum
    in
    if all_same_color (cs)
        then prelim div 2
        else prelim
    end

fun officiate (cs, ml, goal) =
    let
        fun helper (cs, ml, pcs) =
            case ml of
            [] => score (pcs, goal)
            | (Discard c)::t => 
                if sum_cards(pcs) > goal
                then score (pcs, goal)
                else  helper (cs, t, remove_card(pcs, c, IllegalMove))
            | (Draw)::t =>
                if sum_cards(pcs) > goal
                then score (pcs, goal)
                else 
                    case cs of
                        [] => raise IllegalMove
                        | hc::tc => helper (tc, t, hc::pcs)
    in
        helper (cs, ml, [])
    end
                            
