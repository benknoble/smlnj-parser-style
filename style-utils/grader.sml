(* utilities for grading SML source statically *)
structure GraderUtils: sig
  (* prints the top-level names bound by the source *)
  val print_bound_names: FileParser.parseResult -> unit

  (* true iff the expressions corresponding to the binding of the symbol match
   * the regular expression denoted by the string *)
  val exp_contains: Symbol.symbol -> string -> FileParser.parseResult -> bool
  (* true iff any of the ast expressions match the regular expression *)
  val anywhere_contains: string -> FileParser.parseResult -> bool
  (* true iff any of the expressions corresponding to a function-declaration
   * match the regular expression *)
  val fun_contains: string -> FileParser.parseResult -> bool

  (* concatenates a list of regular-expressions into a single regular-expression
   * matching any of them ("alternates") *)
  val alts: string list -> string
  (* type of named checks returning some value of type 'a *)
  type 'a check = (FileParser.parseResult -> 'a) * string
  (* type of reportable checks *)
  type reportable
  (* turns a check into a reportable; needs to know how to display results *)
  val mk_reportable: ('a -> string) -> 'a check -> reportable
  (* a result that makes a good fit for the 'a in 'a check *)
  datatype result = Pass | Fail
  (* conversion functions from bool -> result
   *
   * it is thus easy to create result checks by composing pass or fail with the
   * #1 of a bool check *)
  val pass: bool -> result
  val fail: bool -> result
  (* conversion from result -> string; a good fit for the argument to
   * mk_reportable *)
  val result_to_string: result -> string

  (* true iff the expression bound to the first param calls the second
   *
   * e.g., if we have fun foo x = bar x, then
   * calls "foo" "bar" … should be true *)
  val calls: string -> string -> FileParser.parseResult -> bool
  (* same as calls, but the third param represents an arg to the function being
   * called
   *
   * in the earlier example, we should have that
   * calls "foo" "bar" "x" … be true *)
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

(* A grader program *)
signature GRADER = sig
  (* runs all the checks, producing some consumable output *)
  val runall: string -> unit
end

(* creates a GRADER that prints out output like
 *
 * test name: result *)
functor GraderFn(
  (* the result type of the checks in checks *)
  type result
  (* a list of checks to run *)
  val checks: result GraderUtils.check list
  (* a conversion from result -> string for mk_reportable *)
  val result_to_string: result -> string
): GRADER = struct
  val reports: GraderUtils.reportable list = map (GraderUtils.mk_reportable result_to_string) checks
  fun runall file =
    let val pr = FileParser.parse file
    in app (fn r => r pr) reports
    end
end
