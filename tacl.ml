type op_code =
  | Sum
  | Sub
  | Mult
  | Div
  | Mod
  | Gt
  | Gte
  | Lt
  | Lte
  | Eq
  | Neq
  | And
  | Or
  | Not
  | Neg
  | IfExpr
  | ToReal

type var_type =
  | Bool
  | Int
  | Real
  | Void
  | TypeError of string list

let acceptable_opexprs =
  let expand op = List.fold_left (fun l (ret, args) -> (op, ret, args)::l) in
  let add ops ts l = List.fold_left (fun l op -> expand op l ts) l ops in
  []
  |> add [ToReal] [(Real, [Int])]
  |> add [Sum; Sub; Mult; Div] [(Real, [Real; Real]); (Int, [Int; Int])]
  |> add [Gt; Gte; Lt; Lte] [(Bool, [Real; Real]); (Bool, [Int; Int])]
  |> add [Eq; Neq]
    [(Bool, [Real; Real]); (Bool, [Int; Int]); (Bool, [Bool; Bool])]
  |> add [And; Or] [(Bool, [Bool; Bool])]
  |> add [Not] [(Bool, [Bool])]
  |> add [Neg] [(Real, [Real; Real]); (Int, [Int; Int])]
  |> add [IfExpr]
    [(Real, [Bool; Real; Real]);
    (Int, [Bool; Int; Int]);
    (Bool, [Bool; Bool; Bool])]

type var_location =
  | Var
  | Local
  | Arg

type expr =
  | Operation of op_code * expr list * var_type
  | FunCall of string * expr list * var_type
  | BoolLit of string
  | IntLit of string
  | RealLit of string
  | VarExpr of string * var_location * var_type

let op_expr code args = Operation(code, args, Void)
let fun_call name args = FunCall(name, args, Void)
let bool_lit s = BoolLit (if s = "true" then "1" else "0")
let int_lit s = IntLit s
let real_lit s = RealLit s
let var_expr s = VarExpr(s, Var, Void)

let expr_type = function
  | Operation(_, _, t) -> t
  | FunCall(_, _, t) -> t
  | BoolLit _ -> Bool
  | IntLit _ -> Int
  | RealLit _ -> Real
  | VarExpr(_, _, t) -> t




type stmt =
  | Assign of string * var_location * var_type * expr
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt
  | ProcCall of string * expr list
  | BlockStmt of stmt list
  | PrintStmt of expr

let assign_stmt v e = Assign(v, Var, Void, e)
let block_stmt ss = BlockStmt ss
let if_stmt c t e = IfStmt(c, t, match e with None -> BlockStmt[] | Some s -> s)
let while_stmt c b = WhileStmt(c, b)
let proc_call name args = ProcCall(name, args)
let print_stmt e = PrintStmt e

type fun_decl = {
  fun_name : string;
  formal_args: (var_type * string) list;
  rtype : var_type;
  ldecls: (var_type * string * expr option) list;
  body: stmt list;
  ret_expr: expr;
}

type proc_decl = {
  proc_name : string;
  formal_args: (var_type * string) list;
  ldecls: (var_type * string * expr option) list;
  body: stmt list;
}
type decl =
  | GlobalVarDecl of var_type * string * expr option
  | FunDecl of fun_decl
  | ProcDecl of proc_decl

let dict_size = 42
type scope =
  | LocalScope of (string, (var_location * var_type)) Hashtbl.t * scope
  | GlobalScope of (string, var_type) Hashtbl.t *
    (string, (var_type * var_type list)) Hashtbl.t

let global_scope () =
  GlobalScope(Hashtbl.create dict_size, Hashtbl.create dict_size)

let local_scope parent args =
  let tbl = Hashtbl.create dict_size in
  let rec aux = function
    | [] -> tbl
    | (typ, name)::t -> ((Hashtbl.add tbl name (Arg, typ)); aux t)
  in
  LocalScope(aux args, parent)

let rec find_var sco name = match sco with
  | GlobalScope(vars, _) ->
    (try (Var, Hashtbl.find vars name)
    with Not_found -> (Var, TypeError[name ^ " is not in scope"]))
  | LocalScope(vars, parent) ->
    (try Hashtbl.find vars name
    with Not_found ->  find_var parent name)

let in_scope sco name = match sco with
  | GlobalScope(vars, _)-> Hashtbl.mem vars name
  | LocalScope(vars, _)-> Hashtbl.mem vars name

let add_var sco name typ = match sco with
  | GlobalScope(vars, _) -> Hashtbl.add vars name typ
  | LocalScope(vars, _) -> Hashtbl.add vars name (Local, typ)

let rec find_fun sco name = match sco with
  | GlobalScope(_, funcs) ->
    (try Hashtbl.find funcs name
    with Not_found -> (TypeError[name ^ " is not defined"], []))
  | LocalScope(_, parent) -> find_fun parent name

let add_fun sco name typ args = match sco with
  | GlobalScope(_, funcs) ->
    Hashtbl.add funcs name (typ, List.map (fun (t, _) -> t) args)
  | _ -> failwith "cannot add functions to local scope"

let fun_exists sco name = match find_fun sco name with
  | (TypeError _, _) -> false
  | _ -> true




let validate_type e t =
  let et = expr_type e in
  if et = t
  then Some e
  else if (et = Int && t = Real)
  then Some(Operation(ToReal, [e], Real))
  else None

let rec validate_type_list es ts =
  match es, ts with
  | [], [] -> Some []
  | [], _ -> None
  | _, [] -> None
  | e::es, t::ts -> match validate_type e t with
    | None -> None
    | Some e -> match validate_type_list es ts with
      | None -> None
      | Some es -> Some (e::es)

let rec typecheck_expr sco e =
  let typecheck_list es =
    let es = List.map (typecheck_expr sco) es in
    let errs = List.fold_left (fun acc e ->
      match expr_type e with
      | TypeError l -> acc @ l
      | _ -> acc
      ) [] es
    in (es, errs)
  in
  let typecheck_funcall name es =
    match find_fun sco name, typecheck_list es with
    | (TypeError l0, _), (cargs, l1) -> FunCall(name, cargs, TypeError(l0@l1))
    | (_, _), (cargs, (_::_ as l)) -> FunCall(name, cargs, TypeError l)
    | (ret, fargs), (cargs, []) ->
      match validate_type_list cargs fargs with
      | Some es -> FunCall(name, es, ret)
      | None -> FunCall(name, cargs, TypeError["wrong arguments for "^ name])
  in
  let typecheck_op op args =
    let rec aux args = function
      | [] -> Operation(op, args, TypeError["invalid args for operator"])
      | (operat, ret, fargs)::t ->
        if operat = op
        then
          match validate_type_list args fargs with
          | None -> aux args t
          | Some es -> Operation(op, es, ret)
        else
          aux args t
    in
    let (args, errs) = typecheck_list args in
    if errs = []
    then aux args acceptable_opexprs
    else Operation(op, args, TypeError errs)
  in
  match e with
  | IntLit _ as e -> e
  | RealLit _ as e -> e
  | BoolLit _ as e -> e
  | VarExpr(name, _, _) -> let (l, t) = find_var sco name in VarExpr(name, l, t)
  | FunCall(name, es, _) -> typecheck_funcall name es
  | Operation(op, args, _) -> typecheck_op op args


let type_errors = function
  | TypeError(errs) -> errs
  | _ -> []
let expr_errors e = type_errors (expr_type e)


let check_assign sco name loc typ e =
  let e0 = typecheck_expr sco e in
  let expr_t = expr_type e0 in
  let errors = (type_errors typ) @ (type_errors expr_t) in
  if errors != []
  then (Assign(name, loc, typ, e0), errors)
  else if typ = Real && expr_t = Int
  then (Assign(name, loc, typ, Operation(ToReal, [e0], Real)), [])
  else (Assign(name, loc, typ, e0),
    if typ != expr_t then ["assignment has wrong type"] else [])

let rec typecheck_stmt sco s =
  let check_while e s =
    let e0 = typecheck_expr sco e in
    let (s0, errs) = typecheck_stmt sco s in
    let errors = match expr_type e0 with
      | Bool -> []
      | TypeError ls -> ls
      | _ -> ["non bool expr given to while"]
    in
    (WhileStmt(e0, s0), errors @ errs)
  in
  let check_ifstmt e th el =
    let e0 = typecheck_expr sco e in
    let (th0, errt) = typecheck_stmt sco th in
    let (el0, erre) = typecheck_stmt sco el in
    let errors = match expr_type e0 with
      | Bool -> []
      | TypeError l -> l
      | _ -> ["non bool expr given to if statement"]
    in
    (IfStmt(e0, th0, el0), errors @ errt @ erre)
  in
  let check_block l =
    let t = List.map (typecheck_stmt sco) l in
    let (ss, errs) = List.split t in
    (BlockStmt ss, List.fold_left (@) [] errs)
  in
  let check_proc_call name args =
    let (func_t, args_t) = find_fun sco name in
    let es0 = List.map (typecheck_expr sco) args in
    let argerrs = List.map expr_errors es0 |> List.fold_left (@) [] in
    match func_t, validate_type_list es0 args_t with
    | TypeError l, _ -> (ProcCall(name, es0), l @ argerrs)
    | Void, _ when argerrs != [] -> (ProcCall(name, es0), argerrs)
    | Void, None -> (ProcCall(name, es0), ["wrong arguments for " ^ name])
    | Void, Some es1 -> (ProcCall(name, es1), [])
    | _, _ -> (ProcCall(name, es0), [name ^ "is a function, not a procedure"])
  in
  match s with
  | Assign(name, _, _, e) ->
    let (loc, typ) = find_var sco name in
    check_assign sco name loc typ e
  | WhileStmt(e, s) -> check_while e s
  | IfStmt(c, s0, s1) -> check_ifstmt c s0 s1
  | BlockStmt ss -> check_block ss
  | PrintStmt e -> let e0 = typecheck_expr sco e in (PrintStmt e0, expr_errors e0)
  | ProcCall(n, args) -> check_proc_call n args

let typecheck_vardecl sco name loc typ optinit =
  match in_scope sco name, optinit with
    | false, None -> (add_var sco name typ; ([], []))
    | false, Some e ->
      let (st, errs) = check_assign sco name loc typ e in
      (add_var sco name typ; ([st], errs))
    | true, None -> ([], ["redeclaration of " ^ name])
    | true, Some e ->
      ([], ("redeclaration of " ^name)::(expr_errors (typecheck_expr sco e)))


let typecheck_proc sco args decls ss =
  let nsco = local_scope sco args in
  let rec aux = function
    | [] -> ([], [])
    | (typ, name, optinit)::t ->
      let (sts, errs) = typecheck_vardecl nsco name Local typ optinit in
      let (rsts, rerrs) = aux t in
      (sts@rsts, errs@rerrs)
  in
  let (declstmts, declerrs) = aux decls in
  let ss0 = List.map (typecheck_stmt nsco) ss in
  let (ss1, stmterrs) = List.split ss0 in
  (nsco, declstmts@ss1, List.fold_left (@) declerrs stmterrs)


let rec split_decls sco  decls =
  let add_pf name typ args =
    if fun_exists sco name
    then ["redefinition of " ^ name ]
    else (add_fun sco name typ args; [])
  in
  match decls with
  | [] -> ([], [], [])
  | (ProcDecl { proc_name; formal_args; _} as p)::t ->
    let err = add_pf proc_name Void formal_args in
    let (pfs, vars, errs) = split_decls sco t in
    (p::pfs, vars, err@errs)
  | (FunDecl {fun_name; rtype; formal_args} as f)::t ->
    let err = add_pf fun_name rtype formal_args in
    let (pfs, vars, errs) = split_decls sco t in
    (f::pfs, vars, err@errs)
  | (GlobalVarDecl(_, _, _) as v)::t ->
    let (pfs, vars, errs) = split_decls sco t in
    (pfs, v::vars, errs)

let typecheck_pf sco = function
  | ProcDecl {proc_name; formal_args; ldecls; body} ->
    let (_, nb, errors) = typecheck_proc sco formal_args ldecls body in
    (ProcDecl{proc_name; formal_args; ldecls; body=nb}, errors)
  | FunDecl {fun_name; formal_args; ldecls; rtype; ret_expr; body} ->
    let (nsco, nb, errs) = typecheck_proc sco formal_args ldecls body in
    let e0 = typecheck_expr nsco ret_expr in
    let e1 =
      if rtype = Real && expr_type e0 = Int
      then Operation(ToReal, [e0], Real)
      else e0
    in
    let e_errs = expr_errors e1 in
    let t_errors = errs@e_errs @
      (if expr_type e1 != rtype then ["invalid return expression"] else []) in
    (FunDecl {fun_name; formal_args; ldecls; rtype; ret_expr=e1; body=nb}, t_errors)
  | _ -> failwith "impossible"

let typecheck_gv sco = function
  | GlobalVarDecl(typ, name, optinit) ->
    typecheck_vardecl sco name Var typ optinit
  | _ -> failwith "impossible"

let typecheck_program prog =
  let sco = global_scope () in
  let (funs, vars, errors) = split_decls sco prog in
  let (top_stmts, errs0) =
    List.map (typecheck_gv sco) vars |> List.split in
  let (pf, errs1) =
    List.map (typecheck_pf sco) funs |> List.split in
  let tstmt = List.fold_left (@) [] top_stmts in
  let total_errors = List.fold_left (@) errors (errs0@errs1) in
  (tstmt, pf, total_errors)












type execution_context = {mutable t:int; mutable fp:int; mutable l:int}

let new_execc () = {t=0; fp=0; l=0}

let getreg_t co = let {t;_} = co in co.t <- t + 1; "t" ^(string_of_int t)
let getreg_fp co = let {fp;_} = co in co.fp <- fp + 1; "fp" ^(string_of_int fp)
let getlabel co = let {l;_} = co in co.l <- l + 1; "l" ^(string_of_int l)
let getreg co typ = if typ = Real then getreg_fp co else getreg_t co

let typ_prefix typ = if typ = Real then "fp_" else "i_"
let loc_prefix = function
  | Var -> ""
  | Local -> "l"
  | Arg -> "a"

let rec comma_sep = function
  | [] -> ""
  | [s] -> s
  | s0::s1::t -> s0 ^ ", " ^ (comma_sep (s1::t))

let op_to_s typ opcode =
  (typ_prefix typ) ^ match opcode with
    | Sum -> "sum"
    | Sub -> "sub"
    | Mult -> "mult"
    | Div -> "div"
    | Neg -> "inv"
    | Eq -> "eq"
    | Neq -> "ne"
    | Lt -> "lt"
    | _ -> failwith "unsupported opcode"

let addlabel block label =
  match block with
  | [] -> [[label^":"]]
  | l::t -> ((label^":")::l)::t

let rec intermed_code_expr co reg e =
  let arg_list args = List.map (fun e ->
    let reg = getreg co (expr_type e) in
    (reg, intermed_code_expr co reg e)
    ) args |> List.split in
  match e with
  | BoolLit "true" -> [[reg; "<-"; "i_value"; "1"]]
  | BoolLit "false" -> [[reg; "<-"; "i_value"; "0"]]
  | IntLit v -> [[reg; "<-"; "i_value"; v]]
  | RealLit v -> [[reg; "<-"; "r_value"; v]]
  | VarExpr(name, loc, typ) ->
    [[reg; "<-"; (typ_prefix typ)^(loc_prefix loc)^"load"; "@"^name]]
  | FunCall(name, args, typ) ->
    let (args_reg, blocks) = arg_list args in
    let block = List.fold_left (@) [] blocks in
    block @ [[
      reg; "<-"; (typ_prefix typ)^"call";
      name ^ ", [" ^(comma_sep args_reg) ^ "]"]]
  | Operation(opcode, [a0; a1], typ) when List.mem opcode [Sum;Sub;Mult;Div;Mod] ->
    let aux_reg = getreg co typ in
    let block = (intermed_code_expr co reg a0) @
      (intermed_code_expr co aux_reg a1) in
    let opstr = if opcode = Mod then "mod" else op_to_s typ opcode in
    block @ [[reg; "<-"; opstr; comma_sep [reg; aux_reg]]]
  | Operation(ToReal, [a0], _) ->
    let aux_reg = getreg_t co in
    let block = intermed_code_expr co aux_reg a0 in
    block @ [[reg; "<-"; "itor"; aux_reg]]
  | Operation(opcode, [a0], typ) when opcode = Neg || opcode = Not ->
    let block = intermed_code_expr co reg a0 in
    let opstr = if opcode = Not then "not" else op_to_s typ opcode in
    block @ [[reg; "<-"; opstr; reg]]
  | Operation(opcode, [a0; a1], _) when List.mem opcode [Eq; Neq; Lt] ->
    let sub_typ = expr_type a0 in
    let (aux0, aux1) = (getreg co sub_typ, getreg co sub_typ) in
    let block = (intermed_code_expr co aux0 a0) @ (intermed_code_expr co aux1 a1) in
    block @ [[reg; "<-"; op_to_s sub_typ opcode; comma_sep [aux0; aux1]]]
  | Operation(Gt, [a0; a1], _) ->
    intermed_code_expr co reg (Operation(Lt, [a1; a0], Bool))
  | Operation(Lte, [a0; a1], _) ->
    intermed_code_expr co reg (Operation(Not,[Operation(Lt,[a1; a0], Bool)],Bool))
  | Operation(Gte, [a0; a1], _) ->
    intermed_code_expr co reg (Operation(Not,[Operation(Lt,[a0; a1], Bool)],Bool))
  | Operation(IfExpr, [c; a0; a1], typ) ->
    let aux_reg = if typ = Real then getreg_t co else reg in
    let (l0, l1, l2) = (getlabel co, getlabel co, getlabel co) in
    let c_block = intermed_code_expr co aux_reg c in
    let b0 = addlabel (intermed_code_expr co reg a0) l0 in
    let b1 = addlabel (intermed_code_expr co reg a1) l1 in
    c_block @
    [["cjump"; comma_sep [aux_reg; l0; l1]]] @
    b0 @ [["jump"; l2]] @ b1 @ (addlabel [] l2)
  | Operation(And, [a0; a1], _) ->
    let (l0, l1) = (getlabel co, getlabel co) in
    let b0 = intermed_code_expr co reg a0 in
    let b1 = addlabel (intermed_code_expr co reg a1) l0 in
    b0 @ [["cjump"; comma_sep [reg; l0; l1]]] @ b1 @ (addlabel [] l1)
  | Operation(Or, [a0; a1], _) ->
    let (l0, l1) = (getlabel co, getlabel co) in
    let b0 = intermed_code_expr co reg a0 in
    let b1 = addlabel (intermed_code_expr co reg a1) l0 in
    b0 @ [["cjump"; comma_sep [reg; l1; l0]]] @ b1 @ (addlabel [] l1)
  | _ -> failwith "unsupoorted expression"

let rec intermed_code_stmt co s =
  match s with
  | Assign(name, loc, typ, e) ->
    let reg = getreg co typ in
    let block = intermed_code_expr co reg e in
    block @ [["@"^name; "<-"; (typ_prefix typ)^(loc_prefix loc)^"store"; reg]]
  | IfStmt(c, s0, s1) ->
    let reg = getreg_t co in
    let c_block = intermed_code_expr co reg c in
    let (l0, l1, l2) = (getlabel co, getlabel co, getlabel co) in
    let b0 = addlabel (intermed_code_stmt co s0) l0 in
    let b1 = addlabel (intermed_code_stmt co s1) l1 in
    c_block @ [["cjump"; comma_sep [reg; l0; l1]]] @
    b0 @ [["jump"; l2]] @ b1 @ (addlabel [] l2)
  | WhileStmt(c, s0) ->
    let reg = getreg_t co in
    let (l0, l1, l2) = (getlabel co, getlabel co, getlabel co) in
    let c_block = addlabel (intermed_code_expr co reg c) l0 in
    let b0 = addlabel (intermed_code_stmt co s0) l1 in
    c_block @ [["cjump"; comma_sep [reg; l1; l2]]] @
    b0 @ [["jump"; l0]] @ (addlabel [] l2)
  | BlockStmt(ss) ->
    ss |> List.map (intermed_code_stmt co) |> List.fold_left (@) []
  | PrintStmt(e) ->
    let typ = expr_type e in
    let reg = getreg co typ in
    let prefix = match typ with
      | Bool -> "b_"
      | _ -> typ_prefix typ
    in
    [[prefix ^ "print"; reg]]
  | ProcCall(name, args) ->
    let arg_list args = List.map (fun e ->
      let reg = getreg co (expr_type e) in
      (reg, intermed_code_expr co reg e)
      ) args |> List.split in
    let (args_reg, blocks) = arg_list args in
    let block = List.fold_left (@) [] blocks in
    block @ [[
      "call"; name ^ ", [" ^(comma_sep args_reg) ^ "]"]]


let ins_to_s =
  List.fold_left (fun all s ->
    all ^ (List.fold_left (fun s t -> s ^ " " ^ t) "" s) ^ "\n") ""
let intermed_code_fun = function
  | ProcDecl{proc_name; body; _} ->
    "\nprocedure @" ^ proc_name ^ "\n" ^
      (ins_to_s (intermed_code_stmt (new_execc ()) (BlockStmt body))) ^ "\n"
  | FunDecl{fun_name; body; ret_expr; rtype} ->
    let ctxt = new_execc () in
    let ins0 = intermed_code_stmt ctxt (BlockStmt body) in
    let reg = getreg ctxt rtype in
    let ins1 = intermed_code_expr ctxt reg ret_expr in
    let ins2 = [[(typ_prefix rtype) ^ "ret"; reg]] in
    "\nfunction @" ^ fun_name ^ "\n" ^ (ins_to_s (ins0 @ ins1 @ ins2)) ^ "\n"
  | _ -> failwith "can't generate code for this"

let compile_program prog =
  let (top_stmts, funcs, errs) = typecheck_program prog in
  if errs = []
  then
    let decls = funcs |> List.map intermed_code_fun |> List.fold_left (^) "" in
    let ctxt = new_execc () in
    let top_ins = top_stmts |> List.map (intermed_code_stmt ctxt) |>
      List.fold_left (@) [] |> ins_to_s in
    print_string(top_ins ^ decls)
  else
    List.iter prerr_endline errs
