use "utils.sml";

fun part1 () =
  let
    val line = hd (readlines "input/day7")
    val xs = List.map (Option.valOf o Int.fromString) (String.tokens (fn c => c = #",") line)
    val xs = ListMergeSort.sort (op >) xs
    val pos = List.nth (xs, (List.length xs) div 2)
  in
    List.foldr (fn (x, acc) => Int.abs(x - pos) + acc) 0 xs
  end


fun part2 () =
  let
    val line = hd (readlines "input/day7")
    val xs = List.map (Option.valOf o Int.fromString) (String.tokens (fn c => c = #",") line)
    val (sum, length) = (List.foldr (op +) 0 xs, List.length xs)
    val pos = Real.round(Real.fromInt(sum) / Real.fromInt(length))

    fun total pos =
      let
        val diffs = List.map (fn x => Int.abs(x - pos)) xs
      in
        List.foldr (fn (x, acc) => (x * (x + 1)) div 2 + acc) 0 diffs
      end

    val (r1, r2) = (total pos, total (pos - 1))
  in
    if r1 < r2 then r1 else r2
  end
