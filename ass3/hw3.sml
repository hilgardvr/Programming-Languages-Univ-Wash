(* Coursera Programming Languages, Homework 3, Provided Code *)

exception NoAnswer

datatype pattern = Wildcard
		 | Variable of string
		 | UnitP
		 | ConstP of int
		 | TupleP of pattern list
		 | ConstructorP of string * pattern

datatype valu = Const of int
	      | Unit
	      | Tuple of valu list
	      | Constructor of string * valu

fun g f1 f2 p =
    let 
	val r = g f1 f2 
    in
	case p of
	    Wildcard          => f1 ()
	  | Variable x        => f2 x
	  | TupleP ps         => List.foldl (fn (p,i) => (r p) + i) 0 ps
	  | ConstructorP(_,p) => r p
	  | _                 => 0
    end

(**** for the challenge problem only ****)

datatype typ = Anything
	     | UnitT
	     | IntT
	     | TupleT of typ list
	     | Datatype of string

(**** you can put all your code here ****)

fun only_capitals (strlst) =
    List.filter (fn str => Char.isUpper(String.sub (str, 0))) strlst

fun longest_string1 (strlst) =
    foldl (fn (str1, str2) => if String.size(str1) > String.size(str2) then str1 else str2) "" strlst

fun longest_string2 (strlst) =
    foldl (fn (str1, str2) => if String.size(str1) >= String.size(str2) then str1 else str2) "" strlst

fun longest_string_helper f strlst =
    let
        fun helper strlst result =
            case strlst of
                [] => result
            | h::t => if f (String.size(h), String.size(result))
                then helper t h
                else helper t result
    in
        helper strlst ""
    end

val longest_string3 = longest_string_helper (fn (len1, len2) => len1 > len2)

val longest_string4 = longest_string_helper (fn (len1, len2) => len1 >= len2)

fun longest_capitalized strlst = 
    (longest_string3 o only_capitals) strlst

fun rev_string str =
    (implode o rev o explode) str
    
fun first_answer f lst =
    case lst of
        [] => raise NoAnswer
        | h::t => case f h of
            NONE => first_answer f t
            | SOME v => v

fun all_answers f lst =
    let
        fun helper lst acc =
            case lst of
                [] => SOME acc
                | h::t => case f h of
                    NONE => NONE
                    | SOME l => helper t (acc@l)
    in
        helper lst []
    end

fun count_wildcards p =
    g (fn () => 1) (fn x => 0) p 

fun count_wild_and_variable_lengths p =
    g (fn () => 1) (fn x => String.size(x)) p

fun count_some_var (str, p) =
   g (fn () => 0) (fn s => if s = str then 1 else 0) p 

fun check_pat p =
    let
        fun create_list p =
            case p of
            Variable s => [s]
            | TupleP ps => 
                List.foldl (fn (pat, acc) => acc@(create_list pat)) [] ps
            | ConstructorP (str, pat) =>
                [str]@(create_list pat)
            | _ => []
    in
        create_list p
    end
