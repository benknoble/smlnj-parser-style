structure GraderUtils: sig
  val print_bound_names: FileParser.parseResult -> unit
  val exp_contains: Symbol.symbol -> string -> FileParser.parseResult -> bool
  val anywhere_contains: string -> FileParser.parseResult -> bool
  val fun_contains: string -> FileParser.parseResult -> bool

  val alts: string list -> string
  type 'a check = (FileParser.parseResult -> 'a) * string
  type reportable
  val mk_reportable: ('a -> string) -> 'a check -> reportable
  datatype result = Pass | Fail
  val pass: bool -> result
  val fail: bool -> result
  val result_to_string: result -> string
  val calls: string -> string -> FileParser.parseResult -> bool
  val calls_with_arg: string -> string -> string -> FileParser.parseResult -> bool
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

  fun exp_contains sym str (pr: FileParser.parseResult) =
    let
      val (sym_table, _) = AstUtil.symbol_table (#ast pr)
      val exps = AstUtil.SymbolMap.find (sym_table, sym)
      val expss = Option.map (List.map (AstUtil.pp_e_to_string 80 99)) exps
    in
      Option.isSome exps andalso contains (Option.valOf expss) str
    end

  fun anywhere_contains str (pr: FileParser.parseResult) =
    let
      val (sym_table, _) = AstUtil.symbol_table (#ast pr)
      val exps = List.concat (AstUtil.SymbolMap.listItems sym_table)
      val expss = List.map (AstUtil.pp_e_to_string 80 99) exps
    in
      contains expss str
    end

  fun fun_contains str (pr: FileParser.parseResult) =
    let
      val (_, fun_table) = AstUtil.symbol_table (#ast pr)
      val exps = List.concat (AstUtil.SymbolMap.listItems fun_table)
      val expss = List.map (AstUtil.pp_e_to_string 80 99) exps
    in
      contains expss str
    end

  val alts = String.concatWith "|"

  type 'a check = (FileParser.parseResult -> 'a) * string
  type reportable = FileParser.parseResult -> unit

  fun mk_reportable to_string (check, name) pr =
    print (name ^ ": " ^ (to_string (check pr)) ^ "\n")

  datatype result = Pass | Fail
  fun pass b = if b then Pass else Fail
  fun fail b = pass (not b)
  fun result_to_string r =
    case r
      of Pass => "pass"
       | Fail => "fail"

  fun calls_with_arg value func arg =
    exp_contains (Symbol.varSymbol value) (func ^ ".*" ^ arg)

  fun calls value func = exp_contains (Symbol.varSymbol value) func

end

signature GRADER = sig
  val runall: string -> unit
end

functor GraderFn(
  type result
  val checks: result GraderUtils.check list
  val result_to_string: result -> string
): GRADER = struct
  val reports: GraderUtils.reportable list = map (GraderUtils.mk_reportable result_to_string) checks
  fun runall file =
    let val pr = FileParser.parse file
    in app (fn r => r pr) reports
    end
end
