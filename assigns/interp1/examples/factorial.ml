
let _ =
  let rec fact (n : int) : int =
    if n = 0
    then 1
    else n * fact (n - 1)
  in assert (fact 6 = 6 * 5 * 4 * 3 * 2)
