# SML-NJ Parser & Style-checks

[![This project is considered experimental](https://img.shields.io/badge/status-experimental-critical.svg)](https://benknoble.github.io/status/experimental/)

For UNC Comp 524

This repository contains supporting libraries and style-checks for SML programs
for UNC Comp 524. They depend on [SML/NJ](https://www.smlnj.org/), which
helpfully exposes Compiler internals as structures which can be used, e.g., to
parse a file and create an AST.

An example of parsing a file follows (`sml style-utils/sources.cm`):

```sml
(* make an AST *)
val parsed = FileParser.parse "...";
#ast parsed;

(* pretty print it *)
val depth = ...; (* 1, 2, …, 999, … *)
FileParser.pp depth parsed;
```

Creating style-checks for UNC Comp 524 is work-in-progress; an example of making
a grader is in `524-f20-a3/a3grader.sml` (see also `524-f20-a3.cm`). The core
idea is to write some checks with a return type `result` and a stringifier
`result_to_string`, and pass that structure to `GraderFn`---this gives you a
structure ascribing to `GRADER`, whose `runall` method takes a string
representing a file-path and runs all of your checks. For example,
`A3Grader.runall` on a passing implementation:

```
$ sml -m 524-f20-a3.cm
- A3Grader.runall "SocialDistance.sml";
no disallowed functions: pass
matching derived safe calls matching safe with matcher: pass
matching given safe calls matching safe with matcher: pass
curryable interpolated safe calls interpolated safe: pass
curried once interpolated safe calls curryable interpolated safe: pass
curried twice interpolated safe calls curried once interpolated safe: pass
curried matching given safe calls curryable matching safe with given safe matcher: pass
curried matching derived safe calls curryable matching safe with derived safe matcher: pass
no magic numbers in functions: pass
val it = () : unit
```

Utilities, such as an easy-to-use `result` type and stringifier and
regexp-matcher for declarations, are in `GraderUtils`. Opening it is a quick way
to get started building a grader.

Note that `pass b` is `Pass` if `b` is true, while `fail b` is `Fail` if `b` is
true (and vice-versa for when `b` is false). This makes it easy to write checks
like

```sml
(* fail when f is true *)
val does_not_f = fail o f
(* pass when g is true *)
val does_g = pass o g
```

---

All SML/NJ systems contain compiled versions of the libraries we depend on.

To obtain a reference or source copy of the libraries that we depend on, the
simplest route is to edit `config/targets` in your installation to include the
line
```
request src-smlnj
```
Then run `config/install.{sh,bat}` with appropriate permissions. Extract
`compiler.tgz` and `system.tgz` from the installation directory to obtain the
full source.
