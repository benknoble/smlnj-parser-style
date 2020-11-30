(* structure with parsing utilities from the visible compiler *)
structure FileParser: sig
  (* wraps up everything you might need from parsing *)
  type parseResult = { ast: Ast.dec
                     , device: PrettyPrint.device
                     , source: Source.inputSource
                     }

  (* parses a file (name given by the first param) into an ast *)
  val parse: string -> parseResult

  (* pretty-prints the ast using the attached device. first param is depth *)
  val pp: int -> parseResult -> unit

  (* similar to pp, but first params are width & depth. instead of printing,
   * returns the string *)
  val pp_to_string: int -> int -> parseResult -> string
end = struct
  type parseResult = { ast: Ast.dec
                     , device: PrettyPrint.device
                     , source: Source.inputSource
                     }

  fun parse filepath =
    let
      val source_stream = TextIO.openIn filepath
      val device = PrettyPrint.defaultDevice
      val source = Source.newSource (filepath, source_stream, false, device)
      val ast = SmlFile.parse source
    in
      {ast=ast, device=device, source=source}
    end

  fun pp depth ({source, device, ast, ...}: parseResult) =
    let val stream = PrettyPrint.openStream device
    in PPAst.ppDec (SOME source) stream (ast, depth)
    end

  fun pp_to_string width depth ({source, ast, ...}: parseResult) =
    PrettyPrint.pp_to_string width (PPAst.ppDec (SOME source)) (ast, depth)
end
