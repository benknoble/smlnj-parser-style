structure A3Grader: GRADER = GraderFn(
  struct
    open GraderUtils

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
  end
)
