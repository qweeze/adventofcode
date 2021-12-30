use "utils.sml";


fun parseHeightmap () =
  let
    val lines = readlines "input/day9"
    val charToInt = Option.valOf o Int.fromString o Char.toString
  in
    Array2.fromList (List.map ((List.map charToInt) o String.explode) lines)
  end


fun isLowPoint (arr, el, i, j) =
  let
    val isValid = fn (i, j) => i >= 0 andalso j >= 0 andalso i < Array2.nRows arr andalso j < Array2.nCols arr
    val neighbours = List.filter isValid [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
    val higherNeigbours = List.filter (fn (i, j) => el >= Array2.sub (arr, i, j)) neighbours
  in
    List.length higherNeigbours = 0
  end


fun part1 () = 
  let
    val arr = parseHeightmap ()
  in
    Array2.foldi
    Array2.RowMajor
    (fn (i, j, el, acc) => acc + (if isLowPoint (arr, el, i, j) then 1 + el else 0))
    0
    {base=arr, row=0, col=0, nrows=NONE, ncols=NONE}
  end


structure IntIntSet = ListSetFn (
  struct
    type ord_key = int * int
    fun compare ((x1, y1), (x2, y2)) =
      case (Int.compare (x1, x2), Int.compare (y1, y2)) of
          (GREATER, _) => GREATER
        | (LESS, _) => LESS
        | (EQUAL, order) => order
  end
)


fun traverse (arr, seen, cells) =
  let
    val isInBasin = fn (i, j) => 
        i >= 0 andalso j >= 0 andalso i < Array2.nRows arr andalso j < Array2.nCols arr
        andalso Array2.sub (arr, i, j) <> 9
        andalso not (IntIntSet.member (seen, (i, j)))

    val getNeighbours = fn (i, j) =>
      List.filter isInBasin [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)]
  in
    case cells of
         [] => (seen, IntIntSet.empty)
       | cell::cells' =>   
           let
             val nextCells = if IntIntSet.member (seen, cell) then cells' else getNeighbours cell @ cells'
             val (seen, basin) = traverse (arr, IntIntSet.add (seen, cell), nextCells)
           in
             (seen, IntIntSet.add (basin, cell))
           end
  end


fun checkBasin (arr, seen, i, j) =
  if IntIntSet.member (seen, (i, j)) then (seen, NONE)
  else if Array2.sub (arr, i, j) = 9 then (seen, NONE)
  else let
    val (seen, basin) = (traverse (arr, seen, [(i, j)]))
  in
    (seen, SOME (IntIntSet.numItems basin))
  end


fun part2 () = 
  let
    val arr = parseHeightmap ()
    val (seen, sizes) = (IntIntSet.empty, [])
    val (_, sizes) =
      Array2.foldi
      Array2.RowMajor
      (fn (i, j, el, (seen, sizes)) => case checkBasin (arr, seen, i, j) of 
                                  (seen, SOME size) => (seen, size :: sizes)
                                | (seen, NONE) => (seen, sizes))
      (seen, sizes)
      {base=arr, row=0, col=0, nrows=NONE, ncols=NONE}

    val sizes = ListMergeSort.sort (op <) sizes
  in
    List.nth (sizes, 0) * List.nth (sizes, 1) * List.nth (sizes, 2)
  end
