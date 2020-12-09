open SmallCTypes
open Utils
open TokenTypes

(* Parsing helpers *)

let tok_list = ref []

(* Returns next token in the list. *)
let lookahead () : token =
  match !tok_list with
  | [] -> raise (Failure "no tokens")
  | h :: t -> h

(* Matches the top token in the list. *)
let consume (a : token) : unit =
  match !tok_list with
  | h :: t when a = h -> tok_list := t
  | _ -> raise (InvalidInputException "bad match")

(* Parsing *)

let rec parse_expr () =
	let exp = parse_Expr2() in exp

and parse_Expr2() : expr = 
	parse_OrExpr()

and parse_OrExpr () : expr =
	let a = parse_AndExpr() in
	match lookahead () with 
		| Tok_Or -> consume Tok_Or; let o = parse_OrExpr() in Or (a, o)
		| _ -> a

and parse_AndExpr () : expr =
	let e = parse_EqualityExpr() in
	match lookahead () with
		| Tok_And -> consume Tok_And; let a = parse_AndExpr() in And (e, a)
		| _ -> e

and parse_EqualityExpr () : expr =
	let r = parse_RelationalExpr() in
	match lookahead () with
		| Tok_Equal -> consume Tok_Equal; let e = parse_EqualityExpr () in Equal (r, e)
		| Tok_NotEqual -> consume Tok_NotEqual; let e = parse_EqualityExpr () in 
				NotEqual (r, e)
		| _ -> r

and parse_RelationalExpr () : expr = 
	let a = parse_AdditiveExpr() in 
	match lookahead () with
		| Tok_Greater -> consume Tok_Greater; let r = parse_RelationalExpr () in 
				Greater (a, r)
		| Tok_GreaterEqual -> consume Tok_GreaterEqual; let r = parse_RelationalExpr() in
				GreaterEqual (a, r)
		| Tok_Less -> consume Tok_Less; let r = parse_RelationalExpr () in Less (a, r)
		| Tok_LessEqual -> consume Tok_LessEqual; let r = parse_RelationalExpr () in
				LessEqual (a, r)
		| _ -> a

and parse_AdditiveExpr() : expr = 
	let m = parse_MultiplicativeExpr() in
	match lookahead () with
		| Tok_Add -> consume Tok_Add; let a = parse_AdditiveExpr () in Add (m, a)
		| Tok_Sub -> consume Tok_Sub; let a = parse_AdditiveExpr () in Sub (m, a)
		| _ -> m

and parse_MultiplicativeExpr() : expr = 
	let p = parse_PowerExpr() in
	match lookahead () with
		| Tok_Mult -> consume Tok_Mult; let m = parse_MultiplicativeExpr () in
				Mult (p, m)
		| Tok_Div -> consume Tok_Div; let m = parse_MultiplicativeExpr () in
				Div (p, m)
		| _ -> p

and parse_PowerExpr() : expr =
	let u = parse_UnaryExpr() in
	match lookahead () with
		| Tok_Pow -> consume Tok_Pow; let p = parse_PowerExpr () in Pow (u, p)
		| _ -> u

and parse_UnaryExpr() : expr = 
	match lookahead () with
		| Tok_Not -> consume Tok_Not; let u = parse_UnaryExpr () in Not u
		| _ -> parse_PrimaryExpr ()

and parse_PrimaryExpr() : expr =
	match lookahead () with
		| Tok_Int i -> consume (Tok_Int i); (Int i)
		| Tok_Bool b -> consume (Tok_Bool b); (Bool b)
		| Tok_ID s -> consume (Tok_ID s); (ID s)
		| Tok_LParen -> let _ = consume Tok_LParen in 
				let e = parse_Expr2() in 
				let _ = consume Tok_RParen in
				e
		| _ -> raise (InvalidInputException "bad match")

let rec parse_stmt () =
        let stm = parse_Stmt () in stm

and parse_Stmt () : stmt = 
	let s = parse_StmtOptions () in
	match lookahead () with
		| Tok_Int_Type -> Seq (s, parse_Stmt())
		| Tok_Bool_Type -> Seq (s, parse_Stmt())
		| Tok_ID x -> Seq (s, parse_Stmt())
		| Tok_Print -> Seq (s, parse_Stmt())
		| Tok_If -> Seq (s, parse_Stmt())
		| Tok_For -> Seq (s, parse_Stmt())
		| Tok_While -> Seq (s, parse_Stmt())
		| _ when s = NoOp -> NoOp
		| _ -> Seq (s, NoOp)
		

and parse_StmtOptions () : stmt = 
	match lookahead () with 
		| Tok_Int_Type -> parse_DeclareStmt()
		| Tok_Bool_Type -> parse_DeclareStmt()
		| Tok_ID s -> parse_AssignStmt()
		| Tok_Print -> parse_PrintStmt()
		| Tok_If -> parse_IfStmt()
		| Tok_For -> parse_ForStmt()
		| Tok_While -> parse_WhileStmt()
		| _ -> NoOp

and parse_DeclareStmt () : stmt = 
	let typ = (match lookahead () with 
		| Tok_Int_Type -> consume Tok_Int_Type; Int_Type
		| Tok_Bool_Type -> consume Tok_Bool_Type; Bool_Type
		| _ -> raise (InvalidInputException "bad match")) in
	let id = (match lookahead () with 
		| Tok_ID s -> consume (Tok_ID s); s
		| _ -> raise (InvalidInputException "bad match")) in
	let _ = consume Tok_Semi in
	Declare (typ, id)

and parse_AssignStmt () : stmt =
	let id = (match lookahead() with 
		| Tok_ID s -> consume (Tok_ID s); s
		| _ -> raise (InvalidInputException "bad match")) in
	let _ = consume Tok_Assign in
	let e = parse_expr () in
	let _ = consume Tok_Semi in
	Assign (id, e)

and parse_PrintStmt () : stmt =
	let _ = consume Tok_Print in
	let _ = consume Tok_LParen in
	let e = parse_expr () in
	let _ = consume Tok_RParen in
	let _ = consume Tok_Semi in
	Print e

and parse_IfStmt () : stmt =
	let _ = consume Tok_If in
	let _ = consume Tok_LParen in
	let e = parse_expr() in
	let _ = consume Tok_RParen in
	let _ = consume Tok_LBrace in
	let s = parse_Stmt() in
	let _ = consume Tok_RBrace in
	let els = (match lookahead() with 
		| Tok_Else -> let _ = consume Tok_Else in
				let _ = consume Tok_LBrace in
				let s2 = parse_Stmt () in
				let _ = consume Tok_RBrace in
				s2
		| _ -> NoOp) in
	If (e, s, els)

and parse_ForStmt () : stmt = 
	let _ = consume Tok_For in	
	let _ = consume Tok_LParen in
	let id = (match lookahead () with 
		| Tok_ID s -> let _ = consume (Tok_ID s) in s
		| _ -> raise (InvalidInputException "bad match")) in
	let _ = consume Tok_From in
	let e1 = parse_expr() in
	let _ = consume Tok_To in
	let e2 = parse_expr() in
	let _ = consume Tok_RParen in
	let _ = consume Tok_LBrace in
	let s = parse_Stmt() in
	let _ = consume Tok_RBrace in
	For (id, e1, e2, s)

and parse_WhileStmt () : stmt = 
	let _ = consume Tok_While in
	let _ = consume Tok_LParen in
	let e = parse_expr() in
	let _ = consume Tok_RParen in 
	let _ = consume Tok_LBrace in
	let s = parse_Stmt() in
	let _ = consume Tok_RBrace in
	While (e, s)

let parse_main toks =
        tok_list := toks;
	let _ = consume (Tok_Int_Type) in
  	let _ = consume (Tok_Main) in
	let _ = consume Tok_LParen in
	let _ = consume Tok_RParen in 
	let _ = consume Tok_LBrace in
	let s = parse_stmt() in
	let _ = consume Tok_RBrace in
	if !tok_list <> [EOF] then 
		raise (InvalidInputException "main")
	else 
		s




(* PLEASE UNCOMMENT BASED ON YOUR IMPLEMENTATION *)
(* ONLY ONE LINE SHOULD BE UNCOMMENTED IN EACH FUNCTION *)
let parse_expr_wrapper toks =
    (* UNCOMMENT the following line if you did the parser FUNCTIONALLY *)
    (* let (_, e) = parse_expr toks in e *)
    (* UNCOMMENT the following line if you did the parser IMPERATIVELY *)
    tok_list := toks; parse_expr ()

let parse_stmt_wrapper toks =
    (* UNCOMMENT the following line if you did the parser FUNCTIONALLY *)
    (* let (_, e) = parse_stmt toks in e *)
    (* UNCOMMENT the following line if you did the parser IMPERATIVELY *)
    tok_list := toks; parse_stmt ()
