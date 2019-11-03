fun is_older ( d1 : (int * int * int), d2 : (int * int * int)) =
  if #1 d1 < #1 d2
  then true
  else
    if #1 d1 = #1 d2
    then
      if #2 d1 < #2 d2
      then true
      else
        if #2 d1 = #2 d2
        then
          if #3 d1 < #3 d2
          then true
          else false
        else false
    else false

fun number_in_month ( xs : (int * int * int) list, month : int) =
  let
    fun acc (xs : (int * int * int) list, sum : int) =
      if null xs
      then sum
      else
        if #2 (hd xs) = month
        then acc (tl xs, sum + 1)
        else acc (tl xs, sum)
  in
    acc (xs, 0)
  end

fun number_in_months ( dates : (int * int * int) list, months : int list) =
  if null months
  then 0
  else number_in_month (dates, hd months) + number_in_months (dates, tl months)


fun dates_in_month ( dates : (int * int * int) list, month: int) =
  if null dates
  then []
  else
    if #2 (hd dates) = month
    then hd dates :: dates_in_month (tl dates, month)
    else dates_in_month (tl dates, month)

fun dates_in_months (dates : (int * int * int) list, months: int list) =
  if null months
  then []
  else dates_in_month (dates, hd months) @ dates_in_months (dates, tl months)       

fun get_nth ( xs : string list, num : int) =
  if num < 2
  then hd xs
  else get_nth ((tl xs), num - 1)

fun date_to_string (date : (int * int * int)) =
  let
    val months = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"]
  in
    get_nth (months, #2 date) ^ " " ^ Int.toString (#3 date) ^ ", " ^ Int.toString(#1 date)
  end

fun number_before_reaching_sum (sum : int, intList : int list) =
  let
    fun acc (intList : int list, count : int, total : int) =
	if (total + (hd intList)) >= sum
	then count
	else acc (tl intList, count + 1, total + hd intList)
  in
    acc (intList, 0, 0)
  end

fun what_month (day : int) =
  let
    val days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31] (*59, 90, 120, 151, 181, 212, 243, 273, 304, 334, 365]*)
  in
    1 + number_before_reaching_sum (day, days)
  end

fun month_range (from : int, to : int) =
  if from > to
  then []
  else what_month(from) :: month_range(from + 1, to)

fun oldest (dates : (int * int * int) list) =
  let
    fun acc (dates : (int * int * int) list, old : (int * int * int)) = 
      if null dates
      then old
      else
        if is_older (hd dates, old)
	then acc (tl dates, hd dates)
	else acc (tl dates, old)
  in
    if null dates
    then NONE
    else SOME (acc (tl dates, hd dates))
  end
