use "utils.sml";


fun part1 () =
  let 
    val lines = readlines "input/day3"
    val nBits = (List.length o tl o String.explode) (hd lines)
    val zeros = List.tabulate(nBits, (fn n => 0))
    val countOne = ListPair.map (fn (bit, count) => count + (if bit = #"1" then 1 else 0))

    val counts = List.foldl countOne zeros (List.map String.explode lines)
    val half = List.length lines div 2
    val gamma = List.map (fn x => if x > half then 1 else 0) counts
    val epsilon = List.map (fn x => if x <= half then 1 else 0) counts

    fun toDecimal bits =
      let 
        fun pow2 0 = 1
          | pow2 n = 2 * pow2 (n - 1)
      in
        case bits of
            [] => 0
          | x::bits' => (pow2 (List.length bits - 1)) * x + toDecimal bits'
      end
  in
    toDecimal gamma * toDecimal epsilon
  end


fun part2 () =
  let 
    val lines = readlines "input/day3"
    val chrToInt = fn chr => if chr = #"1" then 1 else 0
    val xs = List.map (List.map chrToInt o String.explode) lines

    fun count el xs =
      List.length (List.filter (fn x => x = el) xs)

    fun mostCommonBit xs =
      if (count 1 xs) >= (count 0 xs) then 1 else 0

    fun leastCommonBit xs =
      if mostCommonBit xs = 1 then 0 else 1

    fun extractValue (criteria, xs, n) =
      let
        val filterBy = criteria (List.map (fn bits => List.nth (bits, n)) xs)
      in
        case List.filter (fn bits => List.nth (bits, n) = filterBy) xs of
             x::[] => x
           | xs => extractValue (criteria, xs, n + 1)
      end

    fun toDecimal bits =
      let 
        fun pow2 0 = 1
          | pow2 n = 2 * pow2 (n - 1)
      in
        case bits of
            [] => 0
          | x::bits' => (pow2 (List.length bits - 1)) * x + toDecimal bits'
      end

    val oxygenGenerator = (toDecimal o extractValue) (mostCommonBit, xs, 0)
    val CO2Scrubber = (toDecimal o extractValue) (leastCommonBit, xs, 0)
  in
    oxygenGenerator * CO2Scrubber
  end
