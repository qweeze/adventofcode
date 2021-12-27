use "utils.sml";


fun parseLine sep line =
    List.map
      (Option.valOf o Int.fromString)
      (String.tokens (fn chr => chr = sep) line)


fun splitBy pred xs =
  let
    fun split' (x, groups) =
      case (pred x, groups) of
           (true, []) => []
         | (false, []) => [[x]]
         | (true, g::gs) => []::g::gs
         | (false, g::gs) => (x::g)::gs
  in
    List.foldr split' [] xs
  end


type Board = {grid: int list list, marks: (int * int) list}

fun parseInput () = 
  let
    val firstLine::_::lines = readlines "input/day4"
    val numbers = parseLine #"," firstLine
    val gridLines = splitBy (fn line => line = "") lines
    val grids = List.map (List.map (parseLine #" ")) gridLines
    val boards = List.map (fn grid => {
      grid = grid,
      marks = [] : (int * int) list
    }) grids
  in
    (boards, numbers)
  end


val (boards, numbers) = parseInput ()
val size = List.length (#grid (hd boards))
val idxs = List.tabulate (size * size, fn x => (x div size, x mod size))


fun getByIdx xs (i, j) =
  List.nth (List.nth (xs, i), j)


fun drawNumber number board =
  case List.find
    (fn idx => (getByIdx (#grid board) idx) = number)
    idxs
  of
       SOME idx => {grid = #grid board, marks = idx::(#marks board)}
     | NONE => board


fun checkWinner (board: Board) = 
  let
    val (rows, cols) = ListPair.unzip (#marks board)
    fun checkRowOrCol (lst, n) =
      if n < 0 then false
      else
        List.length (List.filter (fn x => x = n) lst) = size
        orelse checkRowOrCol (lst, n - 1)
  in
    checkRowOrCol (rows, size) orelse checkRowOrCol (cols, size)
  end


fun getScore (board: Board, number) =
  let
    val nonMarked = List.filter
      (fn idx => not (List.exists (fn el => el = idx) (#marks board)))
      idxs
  in
    number * List.foldr
      (fn (idx, acc) => acc + getByIdx (#grid board) idx)
      0
      nonMarked
  end


fun part1() =
  let
    fun play (boards, numbers) =
      let 
        val number::numbers' = numbers
        val boards = List.map (drawNumber number) boards
      in
        case List.find checkWinner boards of
             NONE => play (boards, numbers')
           | SOME board => (board, number)
      end
  in
    (getScore o play) (boards, numbers)
  end


fun part2() =
  let
    fun play (boards, numbers) =
      let 
        val number::numbers' = numbers
        val boards = List.map (drawNumber number) boards
        val (won, rest) = List.partition checkWinner boards
      in
        if List.length boards = 1 andalso List.length won = 1
        then (hd won, number)
        else play (rest, numbers')
      end
  in
    (getScore o play) (boards, numbers)
  end
