structure Grader: sig
  val print_bound_names: FileParser.parseResult -> unit
  val exp_contains: FileParser.parseResult -> Symbol.symbol -> string -> bool
  val anywhere_contains: FileParser.parseResult -> string -> bool
end = struct
  fun println s = print (s ^ "\n")

  val print_bound_names = (app println) o Elaborator.bound_names

  structure RE = RegExpFn(structure P = AwkSyntax
                          structure E = DfaEngine)

  fun contains (exps: string list) (str: string) =
    let
      val regexp = RE.compileString str
      (* fun gets (ss, n) = Substring.string (Substring.slice (ss, 0, SOME n)) *)
      fun find e = RE.find regexp Substring.getc (Substring.full e)
      val matches = Option.isSome o find
    in
      List.exists matches exps
    end

  fun exp_contains (pr: FileParser.parseResult) sym str =
    let
      val sym_table = AstUtil.symbol_table (#ast pr)
      val exps = AstUtil.SymbolMap.find (sym_table, sym)
      val expss = Option.map (List.map (AstUtil.pp_e_to_string 80 99)) exps
    in
      Option.isSome exps andalso contains (Option.valOf expss) str
    end

  fun anywhere_contains (pr: FileParser.parseResult) str =
    let
      val sym_table = AstUtil.symbol_table (#ast pr)
      val exps = List.concat (AstUtil.SymbolMap.listItems sym_table)
      val expss = List.map (AstUtil.pp_e_to_string 80 99) exps
    in
      contains expss str
    end

end
