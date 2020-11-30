(* structure wrapping up the visible compiler's elaborator in a hopefully useful
 * way *)
structure Elaborator: sig
  (* uses the visible compiler's elaborator to elaborate the ast
   *
   * this includes things like type-checking, fixity-parsing, etc. (I think)
   *
   * currently gives some spurious warnings about structures not being found
   * from the basis, even though we forcibly load it *)
  val elaborate: FileParser.parseResult -> Absyn.dec * StaticEnv.staticEnv

  (* given the ast result, returns a list of names bound at the top-level
   *
   * useful for checking identifiers against style rules *)
  val bound_names: FileParser.parseResult -> string list
end = struct
  fun elaborate ({ast, source, ...}: FileParser.parseResult) =
    let
      (* force loading of the entire basis library, so that the standard basis
       * is available for parsing *)
      val _ = CM.make "$/basis.cm"
      val cinfo = CompInfo.mkCompInfo { source=source
                                      , transform=(fn x => x)
                                      , mkStampGenerator=Stamps.newGenerator
                                      }
      val base_env = #get (EnvRef.base ()) ()
      val base_static = Environment.staticPart base_env
      val elaborated = ElabTop.elabTop (ast, base_static, cinfo)
    in
      (* if CompInfo.anyErrors cinfo *)
      (* then (Absyn.SEQdec nil, StaticEnv.empty) *)
      (* else elaborated *)
      elaborated
    end

  val bound_names = (map Symbol.name) o StaticEnv.symbols o #2 o elaborate
end
