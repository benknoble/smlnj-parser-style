(* GRADER for Fall 2020 Assignment 3 *)
structure A3Grader: GRADER = GraderFn(
  struct
    (* a useful shortcut to define
     *   type result
     *   val result_to_string
     * as required by GraderFn
     * and enable pass/fail composition with other boolean checks
     *)
    open GraderUtils

    (* disallowed builtins--we want to encourage recurisve list-processing, not
     * thinking in terms of these useful functions *)
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
    val no_disallowed_functions = fail o (anywhere_contains disallowed_re)

    (* composition: these functions are intended to re-use others *)
    val matchingDerivedSafe_calls_matchingSafe_with_matcher =
      pass o (calls_with_arg "matchingDerivedSafe" "matchingSafe" "derivedSafeMatcher")
    val matchingGivenSafe_calls_matchingSafe_with_matcher =
      pass o (calls_with_arg "matchingGivenSafe" "matchingSafe" "givenSafeMatcher")

    (* composition by currying: fixing a parameter is currying *)
    val curryableInterpolatedSafe_calls_interpolatedSafe =
      pass o (calls "curryableInterpolatedSafe" "interpolatedSafe")
    val curriedOnceInterpolatedSafe_calls_curryableInterpolatedSafe =
      pass o (calls "curriedOnceInterpolatedSafe" "curryableInterpolatedSafe")
    val curriedTwiceInterpolatedSafe_calls_curriedOnceInterpolatedSafe =
      pass o (calls "curriedTwiceInterpolatedSafe" "curriedOnceInterpolatedSafe")

    (* composition: these functions are intended to re-use others *)
    val curriedMatchingGivenSafe_calls_curryableMatchingSafe_with_givenSafeMatcher =
      pass o (calls_with_arg "curriedMatchingGivenSafe" "curryableMatchingSafe" "givenSafeMatcher")
    val curriedMatchingDerivedSafe_calls_curryableMatchingSafe_with_derivedSafeMatcher =
      pass o (calls_with_arg "curriedMatchingDerivedSafe" "curryableMatchingSafe" "derivedSafeMatcher")

    (* a magic number is anything not exactly 0 or 1. They are allowed in
     * val-declarations, however, to create named-constants *)
    val no_magic_numbers_in_functions =
      fail o (fun_contains "[ \\f\\n\\r\\t][2-9]+[ \\f\\n\\r\\t]")

    (* the required "val checks" for the GraderFn *)
    val checks: result check list = [
      (no_disallowed_functions, "no disallowed functions")
    , (matchingDerivedSafe_calls_matchingSafe_with_matcher, "matching derived safe calls matching safe with matcher")
    , (matchingGivenSafe_calls_matchingSafe_with_matcher, "matching given safe calls matching safe with matcher")
    , (curryableInterpolatedSafe_calls_interpolatedSafe, "curryable interpolated safe calls interpolated safe")
    , (curriedOnceInterpolatedSafe_calls_curryableInterpolatedSafe, "curried once interpolated safe calls curryable interpolated safe")
    , (curriedTwiceInterpolatedSafe_calls_curriedOnceInterpolatedSafe, "curried twice interpolated safe calls curried once interpolated safe")
    , (curriedMatchingGivenSafe_calls_curryableMatchingSafe_with_givenSafeMatcher, "curried matching given safe calls curryable matching safe with given safe matcher")
    , (curriedMatchingDerivedSafe_calls_curryableMatchingSafe_with_derivedSafeMatcher, "curried matching derived safe calls curryable matching safe with derived safe matcher")
    , (no_magic_numbers_in_functions, "no magic numbers in functions")
    ]
  end
)
