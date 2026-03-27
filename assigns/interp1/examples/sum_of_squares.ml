let program =
  let sum_of_squares =
    fun (x : int) ->
    fun (y : int) ->
    x * x + y * y
  in
  let n = sum_of_squares 3 (-5) in
  let _test = assert (n = 34) in
  ()
