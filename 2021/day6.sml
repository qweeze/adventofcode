use "utils.sml";


fun part1 () =
  let
    val line = hd (readlines "input/day6")
    val xs = List.map (Option.valOf o Int.fromString) (String.tokens (fn c => c = #",") line)

    fun count days =
      if days <= 0 then 1
      else count (days - 7) + count (days - 9)
  in
    List.foldr (fn (x, acc) => acc + count (80 - x)) 0 xs
  end


fun part2 () =
  let
    val line = hd (readlines "input/day6")
    val xs = List.map (Option.valOf o Int.fromString) (String.tokens (fn c => c = #",") line)

    fun count days =
      let
        fun count' (idx, fs) =
          if idx = 0 then hd fs
          else count' (idx - 1, List.nth (fs, 8) + List.nth (fs, 6):: fs)

        val fs: IntInf.int list = [1,2,2,2,2,2,2,2,3]
      in
        if days < 8 then List.nth (fs, days)
        else count' (days - 8, List.rev fs)
      end
  in
    List.foldr (fn (x, acc) => acc + count (256 - x)) 0 xs
  end
