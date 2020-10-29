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

Creating style-checks for UNC Comp 524 is work-in-progress.

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
