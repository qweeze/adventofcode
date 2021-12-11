fun readlines (infile : string) =
  let
    val stream = TextIO.openIn infile
    fun loop stream =
      case TextIO.inputLine stream of
         SOME line => line :: loop stream
       | NONE      => []
  in
    loop stream before TextIO.closeIn stream
  end


fun part1 () =
  let 
    val lines = readlines "input/day2"

    fun parseLine line =
      let
        val [direction, value] = (String.tokens Char.isSpace) line
      in
        (direction, valOf (Int.fromString value))
      end

    fun move(command, (pos, depth)) =
      case command of
          ("forward", value) => (pos + value, depth)
        | ("down", value) => (pos, depth + value)
        | ("up", value) => (pos, depth - value)

    val (pos, depth) = List.foldl move (0, 0) (List.map parseLine lines)
  in
    pos * depth
  end


fun part2 () =
  let 
    val lines = readlines "input/day2"

    fun parseLine line =
      let
        val [direction, value] = (String.tokens Char.isSpace) line
      in
        (direction, (Int.toLarge o valOf o Int.fromString) value)
      end

    fun move(command, (pos, depth, aim)) =
      case command of
          ("forward", value) => (pos + value, depth + (aim * value), aim)
        | ("down", value) => (pos, depth, aim + value)
        | ("up", value) => (pos, depth, aim - value)

    val (pos, depth, _) = List.foldl move (0, 0, 0) (List.map parseLine lines)
  in
    pos * depth
  end
