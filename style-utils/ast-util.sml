(* utilities for manipulating ast's *)
structure AstUtil: sig
  (* practically equivalent to FileParser.pp *)
  val pp: int -> Ast.dec -> unit

  (* practically equivalent to FileParser.pp_to_string *)
  val pp_to_string: int -> int -> Ast.dec -> string

  (* practically equivalent to FileParser.pp_to_string, but for Ast.exp *)
  val pp_e_to_string: int -> int -> Ast.exp -> string

  (* strips an ast of all Mark* constructors *)
  val strip_marks: Ast.dec -> Ast.dec

  (* a useful structure for handling maps from symbols to other values *)
  structure SymbolMap: ORD_MAP where type Key.ord_key = Symbol.symbol

  (* collects all top-level val, valrec, and fun declarations into a map
   *
   * #1 in the result contains everything;
   * #2 contains only the fun declarations
   *
   * a symbol maps to a list of expressions in order of declaration; that is,
   * since SML permits re-binding symbols, this table comprises all of them *)
  val symbol_table: Ast.dec -> Ast.exp list SymbolMap.map * Ast.exp list SymbolMap.map
end = struct
  open Ast

  fun pp depth ast =
    let val stream = PrettyPrint.openStream PrettyPrint.defaultDevice
    in PPAst.ppDec NONE stream (ast, depth)
    end

  fun pp_to_string width depth ast =
    PrettyPrint.pp_to_string width (PPAst.ppDec NONE) (ast, depth)

  fun pp_e_to_string width depth ast =
    PrettyPrint.pp_to_string width (PPAst.ppExp NONE) (ast, depth)

  fun strip_marks dec =
    case dec
      of ValDec (vbs, tyvs) =>
           ValDec (map strip_marks_vb vbs, map strip_marks_tyvar tyvs)
       | ValrecDec (rvbs, tyvs) =>
           ValrecDec (map strip_marks_rvb rvbs, map strip_marks_tyvar tyvs)
       | DoDec e => DoDec (strip_marks_exp e)
       | FunDec (fbs, tyvs) =>
           FunDec (map strip_marks_fb fbs, map strip_marks_tyvar tyvs)
       | TypeDec tbs => TypeDec (map strip_marks_tb tbs)
       | DatatypeDec {datatycs, withtycs} =>
           DatatypeDec {datatycs=(map strip_marks_db datatycs), withtycs=(map strip_marks_tb withtycs)}
       | DataReplDec _ => dec
       | AbstypeDec {abstycs, withtycs, body} =>
           AbstypeDec {abstycs=abstycs, withtycs=withtycs, body=(strip_marks body)}
       | ExceptionDec ebs => ExceptionDec (map strip_marks_eb ebs)
       | StrDec strbs => StrDec (map strip_marks_strb strbs)
       | FctDec fctbs => FctDec (map strip_marks_fctb fctbs)
       | SigDec sigbs => SigDec (map strip_marks_sigb sigbs)
       | FsigDec fsigbs => FsigDec (map strip_marks_fsigb fsigbs)
       | LocalDec (dec1, dec2) => LocalDec (strip_marks dec1, strip_marks dec2)
       | SeqDec decs => SeqDec (map strip_marks decs)
       | OpenDec _ => dec
       | OvldDec (s, es) => OvldDec (s, map strip_marks_exp es)
       | FixDec _ => dec
       | MarkDec (dec', region) => strip_marks dec'

  and strip_marks_exp_fixitem {item, fixity, region} =
    {item=(strip_marks_exp item), fixity=fixity, region=region}

  and strip_marks_pat_fixitem {item, fixity, region} =
    {item=(strip_marks_pat item), fixity=fixity, region=region}

  and strip_marks_exp e =
    case e
      of FnExp rules => FnExp (map strip_marks_rule rules)
       | FlatAppExp items => FlatAppExp (map strip_marks_exp_fixitem items)
       | AppExp {function, argument} =>
           AppExp {function=(strip_marks_exp function), argument=(strip_marks_exp argument)}
       | CaseExp {expr, rules} =>
           CaseExp {expr=(strip_marks_exp expr), rules=(map strip_marks_rule rules)}
       | LetExp {dec, expr} =>
           LetExp {dec=(strip_marks dec), expr=(strip_marks_exp expr)}
       | SeqExp es => SeqExp (map strip_marks_exp es)
       | RecordExp s_es =>
           RecordExp (map (fn (s, e) => (s, strip_marks_exp e)) s_es)
       | ListExp es => ListExp (map strip_marks_exp es)
       | TupleExp es => TupleExp (map strip_marks_exp es)
       | ConstraintExp {expr, constraint} =>
           ConstraintExp {expr=(strip_marks_exp expr), constraint=(strip_marks_ty constraint)}
       | HandleExp {expr, rules} =>
           HandleExp {expr=(strip_marks_exp expr), rules=(map strip_marks_rule rules)}
       | RaiseExp e' => RaiseExp (strip_marks_exp e')
       | IfExp {test, thenCase, elseCase} =>
           IfExp {test=(strip_marks_exp test), thenCase=(strip_marks_exp thenCase), elseCase=(strip_marks_exp elseCase)}
       | AndalsoExp (e1, e2) =>
           AndalsoExp (strip_marks_exp e1, strip_marks_exp e2)
       | OrelseExp (e1, e2) =>
           OrelseExp (strip_marks_exp e1, strip_marks_exp e2)
       | VectorExp es => VectorExp (map strip_marks_exp es)
       | WhileExp {test, expr} =>
           WhileExp {test=(strip_marks_exp test), expr=(strip_marks_exp expr)}
       | MarkExp (e', _) => strip_marks_exp e'
       | _ => e

  and strip_marks_rule (Rule {pat, exp}) =
    Rule {pat=(strip_marks_pat pat), exp=(strip_marks_exp exp)}

  and strip_marks_pat p =
    case p
      of RecordPat {def, flexibility} =>
           RecordPat {def=(map (fn (s, p) => (s, strip_marks_pat p)) def), flexibility=flexibility}
       | ListPat ps => ListPat (map strip_marks_pat ps)
       | TuplePat ps => TuplePat (map strip_marks_pat ps)
       | FlatAppPat items => FlatAppPat (map strip_marks_pat_fixitem items)
       | AppPat {constr, argument} =>
           AppPat {constr=(strip_marks_pat constr), argument=(strip_marks_pat argument)}
       | ConstraintPat {pattern, constraint} =>
           ConstraintPat {pattern=(strip_marks_pat pattern), constraint=(strip_marks_ty constraint)}
       | LayeredPat {varPat, expPat} =>
           LayeredPat {varPat=(strip_marks_pat varPat), expPat=(strip_marks_pat expPat)}
       | VectorPat ps => VectorPat (map strip_marks_pat ps)
       | MarkPat (p', _) => strip_marks_pat p'
       | OrPat ps => OrPat (map strip_marks_pat ps)
       | _ => p

  and strip_marks_str str =
    case str
      of BaseStr dec => BaseStr (strip_marks dec)
       | ConstrainedStr (str', sig') =>
           ConstrainedStr (strip_marks_str str', strip_marks_sig_const sig')
       | AppStr (p, strs) =>
           AppStr (p, map (fn (s, b) => (strip_marks_str s, b)) strs)
       | AppStrI (p, strs) =>
           AppStrI (p, map (fn (s, b) => (strip_marks_str s, b)) strs)
       | LetStr (d, str') => LetStr (strip_marks d, strip_marks_str str')
       | MarkStr (str', _) => strip_marks_str str'
       | _ => str

  and strip_marks_sig_const s =
    case s
      of NoSig => NoSig
       | Transparent s' => Transparent (strip_marks_sig s')
       | Opaque s' => Opaque (strip_marks_sig s')

  and strip_marks_fct fct =
    case fct
      of VarFct (p, fsig) => VarFct (p, strip_marks_fsig_const fsig)
       | BaseFct {params, body, constraint} =>
           BaseFct {params=(map (fn (s, sig') => (s, strip_marks_sig sig')) params),
                    body=(strip_marks_str body),
                    constraint=(strip_marks_sig_const constraint)}
       | LetFct (d, fct') => LetFct (strip_marks d, strip_marks_fct fct')
       | AppFct (p, ss, fsig) =>
           AppFct (p, map (fn (s, b) => (strip_marks_str s, b)) ss, strip_marks_fsig_const fsig)
       | MarkFct (fct', _) => strip_marks_fct fct'

  and strip_marks_fsig_const s =
    case s
      of NoSig => NoSig
       | Transparent s' => Transparent (strip_marks_fsig s')
       | Opaque s' => Opaque (strip_marks_fsig s')

  and strip_marks_where w =
    case w
      of WhType (ss, tys, ty) =>
           WhType (ss, map strip_marks_tyvar tys, strip_marks_ty ty)
       | _ => w

  and strip_marks_sig s =
    case s
      of AugSig (s, ws) => AugSig (strip_marks_sig s, map strip_marks_where ws)
       | BaseSig ss => BaseSig (map strip_marks_spec ss)
       | MarkSig (s', _) => strip_marks_sig s'
       | _ => s

  and strip_marks_fsig f =
    case f
      of BaseFsig {param, result} =>
           BaseFsig {param=(map (fn (s, sig') => (s, strip_marks_sig sig')) param),
                     result=(strip_marks_sig result)}
       | MarkFsig (f', _) => strip_marks_fsig f'
       | _ => f

  and strip_marks_spec s =
    case s
      of StrSpec ss =>
           StrSpec (map (fn (s, sig', p) => (s, strip_marks_sig sig', p)) ss)
       | TycSpec (ss, b) =>
           TycSpec (map (fn (s, tys, ty) => (s, map strip_marks_tyvar tys, Option.map strip_marks_ty ty)) ss,
                    b)
       | FctSpec ss =>
           FctSpec (map (fn (s, fsig) => (s, strip_marks_fsig fsig)) ss)
       | ValSpec ss => ValSpec (map (fn (s, ty) => (s, strip_marks_ty ty)) ss)
       | DataSpec {datatycs, withtycs} =>
           DataSpec {datatycs=(map strip_marks_db datatycs),
                     withtycs=(map strip_marks_tb withtycs)}
       | ExceSpec ss =>
           ExceSpec (map (fn (s, t) => (s, Option.map strip_marks_ty t)) ss)
       | IncludeSpec sig' => IncludeSpec (strip_marks_sig sig')
       | MarkSpec (s', _) => strip_marks_spec s'
       | _ => s

  and strip_marks_vb v =
    case v
      of Vb {pat, exp, lazyp} =>
           Vb {pat=(strip_marks_pat pat),
               exp=(strip_marks_exp exp),
               lazyp=lazyp}
       | MarkVb (v', _) => strip_marks_vb v'

  and strip_marks_rvb v =
    case v
      of Rvb {var, fixity, exp, resultty, lazyp} =>
           Rvb {var=var, fixity=fixity, exp=(strip_marks_exp exp),
                resultty=(Option.map strip_marks_ty resultty), lazyp=lazyp}
       | MarkRvb (v', _) => strip_marks_rvb v'

  and strip_marks_fb f =
    case f
      of Fb (cs, b) => Fb (map strip_marks_clause cs, b)
       | MarkFb (f', _) => strip_marks_fb f'

  and strip_marks_clause (Clause {pats, resultty, exp}) =
    Clause {pats=(map strip_marks_pat_fixitem pats),
            resultty=(Option.map strip_marks_ty resultty),
            exp=(strip_marks_exp exp)}

  and strip_marks_tb t =
    case t
      of Tb {tyc, def, tyvars} =>
           Tb {tyc=tyc, def=(strip_marks_ty def),
               tyvars=(map strip_marks_tyvar tyvars)}
       | MarkTb (t', _) => strip_marks_tb t'

  and strip_marks_db d =
    case d
      of Db {tyc, tyvars, rhs, lazyp} =>
           Db {tyc=tyc, tyvars=(map strip_marks_tyvar tyvars),
               rhs=(map (fn (s, t) => (s, Option.map strip_marks_ty t)) rhs),
               lazyp=lazyp}
       | MarkDb (d', _) => strip_marks_db d'

  and strip_marks_eb e =
    case e
      of EbGen {exn, etype} => EbGen {exn=exn, etype=(Option.map strip_marks_ty etype)}
       | MarkEb (e', _) => strip_marks_eb e'
       | _ => e

  and strip_marks_strb s =
    case s
      of Strb {name, def, constraint} =>
           Strb {name=name, def=(strip_marks_str def),
                 constraint=(strip_marks_sig_const constraint)}
       | MarkStrb (s', _) => strip_marks_strb s'

  and strip_marks_fctb f =
    case f
      of Fctb {name, def} => Fctb {name=name, def=(strip_marks_fct def)}
       | MarkFctb (f', _) => strip_marks_fctb f'

  and strip_marks_sigb s =
    case s
      of Sigb {name, def} => Sigb {name=name, def=(strip_marks_sig def)}
       | MarkSigb (s', _) => strip_marks_sigb s'

  and strip_marks_fsigb f =
    case f
      of Fsigb {name, def} => Fsigb {name=name, def=(strip_marks_fsig def)}
       | MarkFsigb (f', _) => strip_marks_fsigb f'

  and strip_marks_tyvar t =
    case t
      of MarkTyv (t', _) => strip_marks_tyvar t'
       | _ => t

  and strip_marks_ty t =
    case t
      of VarTy t' => VarTy (strip_marks_tyvar t')
       | ConTy (ss, ts) => ConTy (ss, map strip_marks_ty ts)
       | RecordTy sts => RecordTy (map (fn (s, t) => (s, strip_marks_ty t)) sts)
       | TupleTy ts => TupleTy (map strip_marks_ty ts)
       | MarkTy (t', _) => strip_marks_ty t'

  structure SymbolMap = RedBlackMapFn(struct
    type ord_key = Symbol.symbol
    fun compare (s1, s2) = String.compare (Symbol.name s1, Symbol.name s2)
  end)
  structure SM = SymbolMap

  fun symbol_table dec =
    let
      val no_marks = strip_marks dec
      val ins = SM.insertWith (op @)
      val path_to_sym = Symbol.varSymbol o SymPath.toString o SymPath.SPATH
      fun symbol_table' dec map =
        case dec
          of ValDec (vbs, tyvs) =>
               foldl (fn (v, m) => symbol_table_vb v m) map vbs
           | ValrecDec (rvbs, tyvs) =>
               foldl (fn (v, m) => symbol_table_rvb v m) map rvbs
           | FunDec (fbs, tyvs) =>
               foldl (fn (f, m) => symbol_table_fb f m) map fbs
           | LocalDec (_, dec') => symbol_table' dec' map
           | SeqDec decs => foldl (fn (d, m) => symbol_table' d m) map decs
           | _ => map
      and symbol_table_vb (Vb {pat, exp, ...}) (m, fm) = (symbol_table_pat pat exp m, fm)
      and symbol_table_pat p e m =
        case p
          of VarPat path => ins (m, path_to_sym path, [e])
           | RecordPat {def, ...} =>
               foldl (fn (s, m) => ins (m, s, [e])) m (map #1 def)
          | ListPat ps => foldl (fn (p, m) => symbol_table_pat p e m) m ps
          | TuplePat ps => foldl (fn (p, m) => symbol_table_pat p e m) m ps
          | FlatAppPat pis =>
               foldl (fn (p, m) => symbol_table_pat p e m) m (map #item pis)
          | AppPat {argument, ...} => symbol_table_pat argument e m
          | ConstraintPat {pattern, ...} => symbol_table_pat pattern e m
          | LayeredPat {varPat, expPat} =>
              symbol_table_pat varPat e (symbol_table_pat expPat e m)
          | VectorPat ps => foldl (fn (p, m) => symbol_table_pat p e m) m ps
          | OrPat ps => foldl (fn (p, m) => symbol_table_pat p e m) m ps
          | _ => m
      and symbol_table_rvb (Rvb {var, exp, ...}) (m, fm) = (ins (m, var, [exp]), fm)
      and symbol_table_fb (Fb (clauses, _)) (m, fm) =
        let
          val exps = map (fn (Clause {exp, ...}) => exp) clauses
          (* binds name -----------------vvvv *)
          val Clause {pats={item=VarPat [name], ...}::_, ...} = hd clauses
        in
          (ins (m, name, exps), ins (fm, name, exps))
        end
    in
      symbol_table' no_marks (SM.empty, SM.empty)
    end

end
