fun readlines (infile : string) =
  let
    val stream = TextIO.openIn infile

    fun stripNL str =
      String.implode (List.filter (fn chr => chr <> #"\n") (String.explode str))

    fun loop stream =
      case TextIO.inputLine stream of
         SOME line => stripNL line :: loop stream
       | NONE      => []
  in
    loop stream before TextIO.closeIn stream
  end
