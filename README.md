# SML-NJ Parser & Style-checks

[![This project is considered experimental](https://img.shields.io/badge/status-experimental-critical.svg)](https://benknoble.github.io/status/experimental/)

For UNC Comp 524

This repository contains supporting libraries and style-checks for SML programs
for UNC Comp 524.

This repository also contains the Parser and Basics libraries excised from the
SML-NJ compiler. We kept them as a reference, though it neither is directly
actually used. Instead, we load `$smlnj/compiler/current.cm`, which should work
on all platforms with an SML-NJ system.

An example of parsing a file follows (`sml style-utils/sources.cm`):

```sml
(* make an AST *)
val parsed = FileParser.parse "...";
#ast parsed;

(* pretty print it *)
val depth = ...; (* 1, 2, …, 999, … *)
FileParser.pp depth parsed;
```

Creating style-checks for UNC Comp 524 is work-in-progress.

---

All of the original code is licensed and copyrighted under the original
[license](./LICENSE) and [copyright](./COPYRIGHT).

Subsequent modifications to other sources (roughly, commits beyond
`23dec5d91239d68972b3572a78a86b1360834b54` and excluding modifications to the
following files) are copyright (2020) UNC.

Listing of original files covered by [license](./LICENSE) and
[copyright](./COPYRIGHT):

```
.gitignore
Basics/basics.cm
Basics/compiler/compileexn.sml
Basics/compiler/endianess-big.sml
Basics/compiler/endianess-little.sml
Basics/compiler/int-const.sml
Basics/compiler/real-const.sml
Basics/compiler/target.sig
Basics/compiler/target32.sml
Basics/compiler/target64.sml
Basics/control/basiccontrol.sml
Basics/errormsg/errormsg.sig
Basics/errormsg/errormsg.sml
Basics/pid/persmap.sml
Basics/pid/persstamps.sig
Basics/pid/persstamps.sml
Basics/pid/pidenv.sig
Basics/pid/pidenv.sml
Basics/print/pputil.sig
Basics/print/pputil.sml
Basics/print/prettyprint.sml
Basics/print/printcontrol.sml
Basics/print/printutil.sig
Basics/print/printutil.sml
Basics/reals/README
Basics/reals/real-lit.sml
Basics/reals/real-to-bits-fn.sml
Basics/reals/real-to-bits.sig
Basics/reals/real64-to-bits.sml
Basics/source/pathnames.sml
Basics/source/source.sig
Basics/source/source.sml
Basics/source/sourcemap.sig
Basics/source/sourcemap.sml
Basics/source/sourceutil.sml
Basics/stats/stats.sml
Basics/symbol/fastsymbol.sig
Basics/symbol/fastsymbol.sml
Basics/symbol/fixity.sml
Basics/symbol/specialsyms.sml
Basics/symbol/symbol-hashtable.sml
Basics/symbol/symbol.sig
Basics/symbol/symbol.sml
Basics/util/wordstr-hashtable.sml
COPYRIGHT
INDEX
LICENSE
MAP
Parse/ast/ast.sig
Parse/ast/ast.sml
Parse/ast/astutil.sig
Parse/ast/astutil.sml
Parse/ast/ppast.sig
Parse/ast/ppast.sml
Parse/lex/.gitignore
Parse/lex/ml.lex
Parse/lex/sml.lex
Parse/lex/tokentable.sml
Parse/lex/user.sml
Parse/main/ml-parser.sml
Parse/main/parse-result.sml
Parse/main/parser.sig
Parse/main/parsercontrol.sml
Parse/main/sml-parser.sml
Parse/main/smlfile.sml
Parse/parse/.gitignore
Parse/parse/ml.grm
Parse/parse/sml.grm
Parse/parser.cm
README-very-very-old
```
