structure A3Grader: sig
  val runall: string -> unit
end = struct

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

  val disallowed_re = alts [
    "List\\.app"
  , "app"
  , "List\\.foldl"
  , "foldl"
  , "List\\.foldr"
  , "foldr"
  , "List\\.exists"
  , "case"
  ]
  val no_disallowed_functions = fail o (Grader.anywhere_contains disallowed_re)

  fun calls_with_arg value func arg =
    Grader.exp_contains (Grader.varSymbol value) (func ^ ".*" ^ arg)

  fun calls value func = Grader.exp_contains (Grader.varSymbol value) func

  val matchingDerivedSafe_calls_matchingSafe_with_matcher =
    pass o (calls_with_arg "matchingDerivedSafe" "matchingSafe" "derivedSafeMatcher")
  val matchingGivenSafe_calls_matchingSafe_with_matcher =
    pass o (calls_with_arg "matchingGivenSafe" "matchingSafe" "givenSafeMatcher")

  val curryableInterpolatedSafe_calls_interpolatedSafe =
    pass o (calls "curryableInterpolatedSafe" "interpolatedSafe")
  val curriedOnceInterpolatedSafe_calls_curryableInterpolatedSafe =
    pass o (calls "curriedOnceInterpolatedSafe" "curryableInterpolatedSafe")
  val curriedTwiceInterpolatedSafe_calls_curriedOnceInterpolatedSafe =
    pass o (calls "curriedTwiceInterpolatedSafe" "curriedOnceInterpolatedSafe")

  val curriedMatchingGivenSafe_calls_curryableMatchingSafe_with_givenSafeMatcher =
    pass o (calls_with_arg "curriedMatchingGivenSafe" "curryableMatchingSafe" "givenSafeMatcher")
  val curriedMatchingDerivedSafe_calls_curryableMatchingSafe_with_derivedSafeMatcher =
    pass o (calls_with_arg "curriedMatchingDerivedSafe" "curryableMatchingSafe" "derivedSafeMatcher")

  val checks: result check list = [
    (no_disallowed_functions, "no disallowed functions")
  , (matchingDerivedSafe_calls_matchingSafe_with_matcher, "matching derived safe calls matching safe with matcher")
  , (matchingGivenSafe_calls_matchingSafe_with_matcher, "matching given safe calls matching safe with matcher")
  , (curryableInterpolatedSafe_calls_interpolatedSafe, "curryable interpolated safe calls interpolated safe")
  , (curriedOnceInterpolatedSafe_calls_curryableInterpolatedSafe, "curried once interpolated safe calls curryable interpolated safe")
  , (curriedTwiceInterpolatedSafe_calls_curriedOnceInterpolatedSafe, "curried twice interpolated safe calls curried once interpolated safe")
  , (curriedMatchingGivenSafe_calls_curryableMatchingSafe_with_givenSafeMatcher, "curried matching given safe calls curryable matching safe with given safe matcher")
  , (curriedMatchingDerivedSafe_calls_curryableMatchingSafe_with_derivedSafeMatcher, "curried matching derived safe calls curryable matching safe with derived safe matcher")
  ]
  val reports: reportable list = map (mk_reportable result_to_string) checks

  fun runall file =
    let
      val pr = FileParser.parse file
    in
      app (fn r => r pr) reports
    end
end
