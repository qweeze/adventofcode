use "utils.sml";


fun part1 () =
  let 
    val xs = List.map (valOf o Int.fromString) (readlines "input/day1")
    fun countIncreases xs =
      case xs of
         x1::x2::xs' => (if x2 > x1 then 1 else 0) + countIncreases (x2::xs')
       | _ => 0
  in
    countIncreases xs
  end


fun part2 () =
  let 
    val xs = List.map (valOf o Int.fromString) (readlines "input/day1")
    val n = 3

    fun countIncreases xs =
      if List.length xs <= n
      then 0
      else (if List.nth (xs, n) > hd xs then 1 else 0) + countIncreases (tl xs)
  in
    countIncreases xs
  end
