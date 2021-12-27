use "utils.sml";


fun parseLine line =
  let
    val [x1, y1, x2, y2] =
      List.mapPartial
      Int.fromString
      (String.tokens (fn c => c = #"," orelse c = #" ") line)
  in
    (x1, y1, x2, y2)
  end

    
fun part1() =
  let
    val lines = readlines "input/day5"
    val grid = Array2.array (1000, 1000, 0)
    val isOrthogonal = fn (x1, y1, x2, y2) => x1 = x2 orelse y1 = y2
    val coords = List.filter isOrthogonal (List.map parseLine lines)

    fun lineToRegion arr (x1, y1, x2, y2) =
      let
        val (row, col, nrows, ncols) =
          if x1 = x2
          then (x1, Int.min(y1, y2), 1, Int.abs(y2 - y1) + 1)
          else (Int.min(x1, x2), y1, Int.abs(x1 - x2) + 1, 1)
      in
        {base=arr, col=col, row=row, nrows=SOME nrows, ncols=SOME ncols}
      end

    val addLine = Array2.modifyi Array2.RowMajor (fn (i, j, el) => el + 1)
  in
    ignore (List.app (addLine o (lineToRegion grid)) coords);
    Array2.fold Array2.RowMajor (fn (x, acc) => if x >= 2 then acc + 1 else acc) 0 grid
  end


fun part2() =
  let
    val lines = readlines "input/day5"
    val coords = List.map (mkLine o parseLine) lines
    val grid = Array2.array (1000, 1000, 0)

    fun mkLine (x1, y1, x2, y2) =
      if x1 = x2 then
        List.tabulate(Int.abs(y2 - y1) + 1, fn i => (x1, Int.min(y1, y2) + i))
      else if y1 = y2 then
        List.tabulate(Int.abs(x2 - x1) + 1, fn i => (Int.min(x1, x2) + i, y1))
      else
        let
          val sx = if x2 > x1 then 1 else ~1
          val sy = if y2 > y1 then 1 else ~1
        in
          List.tabulate(Int.abs(x2 - x1) + 1, fn i => (x1 + sx * i, y1 + sy * i))
        end

    fun increment arr (i, j) =
      Array2.update (arr, i, j, Array2.sub (arr, i, j) + 1)
  in
    ignore (List.app (List.app (increment grid)) coords);
    Array2.fold Array2.RowMajor (fn (x, acc) => if x >= 2 then acc + 1 else acc) 0 grid
  end
