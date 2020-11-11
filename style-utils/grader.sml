structure Grader: sig
  val print_bound_names: FileParser.parseResult -> unit
end = struct
  fun println s = print (s ^ "\n")
  val print_bound_names = (app println) o Elaborator.bound_names
end
