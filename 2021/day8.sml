use "utils.sml";


fun parseLine line =
  let
    val splitBy = fn chr => String.tokens (fn c => c = chr)
    val [patterns, digits] = splitBy #"|" line
  in
    (splitBy #" " patterns, splitBy #" " digits)
  end


fun part1 () =
  let
    val lines = readlines "input/day8"

    fun countUnique (_, digits) =
      let
        val isUnique = fn length => List.exists (fn el => el = length) [2, 3, 4, 7]
      in
        List.length (List.filter isUnique (List.map String.size digits))
      end
  in
    List.foldr (fn (line, acc) => acc + (countUnique o parseLine) line) 0 lines
  end


fun part2 () =
  let
    val lines = readlines "input/day8"
    val allChars = [#"a", #"b", #"c", #"d", #"e", #"f", #"g"]

    fun getCounts patterns =
      let
        val chars = List.foldr (fn (p, acc) => String.explode p @ acc) [] patterns
      in
        List.map (fn chr => (chr, List.length (List.filter (fn c => c = chr) chars))) allChars
      end

    fun charByCount (counts, n) =
      let
        val SOME (chr, _) = List.find (fn (chr, count) => count = n) counts
      in
        chr
      end

    fun patternBySize (patterns, size) =
      Option.valOf (List.find (fn c => String.size c = size) patterns)

    fun difference (fst, snd) =
      List.filter
      (fn c => not (List.exists (fn chr => chr = c) (String.explode snd)))
      (String.explode fst)

    fun getDecoder patterns =
      let
        val counts = getCounts patterns
        val segB = charByCount (counts, 6)
        val segE = charByCount (counts, 4)
        val segF = charByCount (counts, 9)
        val one = patternBySize (patterns, 2)
        val seven = patternBySize (patterns, 3)
        val segA = hd (difference (seven, one))
        val four = patternBySize (patterns, 4)
        val segD = hd (difference (String.implode (difference (four, one)), Char.toString segB))
        val segC = hd (difference(one, Char.toString segF))
        val segG = hd (difference(String.implode allChars, String.implode [segA, segB, segC, segD, segE, segF]))
      in
        fn c => if c = segA then #"a"
           else if c = segB then #"b"
           else if c = segC then #"c"
           else if c = segD then #"d"
           else if c = segE then #"e"
           else if c = segF then #"f"
           else                  #"g"
      end

    fun sort s =
      String.implode (ListMergeSort.sort (op >) (String.explode s))

    exception UnknownDigit

    fun decode (decoder, digits) =
      let
        val decoded = List.map (fn digit => String.implode (List.map decoder (String.explode digit))) digits

        fun toNum digit =
          case digit of
               "abcefg" => #"0"
             | "cf" => #"1"
             | "acdeg" => #"2"
             | "acdfg" => #"3"
             | "bcdf" => #"4"
             | "abdfg" => #"5"
             | "abdefg" => #"6"
             | "acf" => #"7"
             | "abcdefg" => #"8"
             | "abcdfg" => #"9"
             | _ => raise UnknownDigit
      in
        (Option.valOf o Int.fromString) (String.implode (List.map (toNum o sort) decoded))
      end
       
      fun decodeLine (patterns, digits) =
        decode (getDecoder patterns, digits)

    in
      List.foldr (fn (line, acc) => acc + ((decodeLine o parseLine) line)) 0 lines
    end
