open SmallCTypes
open EvalUtils
open TokenTypes

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec eval_expr env t =
    match t with
    | Int i -> Int_Val i 
    | Bool b -> Bool_Val b
    | ID s -> if (List.mem_assoc s env) then (List.assoc s env) else raise (DeclareError "ID not in env")
    | Add (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> i 
                             | _ -> raise (TypeError "Add first")) in
		     let y = (match d with
			     | Int_Val i -> i
                             | _ -> raise (TypeError "Add second")) in
		     Int_Val (x+y))
    | Sub (a, b) ->  (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> i 
                             | _ -> raise (TypeError "Sub first")) in
		     let y = (match d with
			     | Int_Val i -> i
                             | _ -> raise (TypeError "Sub second")) in
		     Int_Val (x-y))
    | Mult (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> i 
                             | _ -> raise (TypeError "Mult first")) in
		     let y = (match d with
			     | Int_Val i -> i
                             | _ -> raise (TypeError "Mult second")) in
		     Int_Val (x*y))
     | Div (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> i 
                             | _ -> raise (TypeError "Div first")) in
		     let y = (match d with
			     | Int_Val i -> i
                             | _ -> raise (TypeError "Div second")) in
		     if y = 0 then raise (DivByZeroError) else Int_Val (x/y))
     | Pow (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> float_of_int i 
                             | _ -> raise (TypeError "Pow first")) in
		     let y = (match d with
			     | Int_Val i -> float_of_int i
                             | _ -> raise (TypeError "Pow second")) in
		     Int_Val (int_of_float (floor (x**y))))
     | Or (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Bool_Val b -> b 
                             | _ -> raise (TypeError "Or first")) in
		     let y = (match d with
			     | Bool_Val b -> b
                             | _ -> raise (TypeError "Or second")) in
		     Bool_Val (x || y))
     | And (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Bool_Val b -> b 
                             | _ -> raise (TypeError "And first")) in
		     let y = (match d with
			     | Bool_Val b -> b
                             | _ -> raise (TypeError "And second")) in
		     Bool_Val (x && y))
     | Not a -> (let c = eval_expr env a in
		     let x = (match c with
		             | Bool_Val b -> b 
                             | _ -> raise (TypeError "Not first")) in
		     if x = true then Bool_Val false else Bool_Val true)
     | Greater (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> i 
                             | _ -> raise (TypeError "Greater first")) in
		     let y = (match d with
			     | Int_Val i -> i
                             | _ -> raise (TypeError "Greater second")) in
		     if x > y then Bool_Val true else Bool_Val false)
     | Less (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> i 
                             | _ -> raise (TypeError "Less first")) in
		     let y = (match d with
			     | Int_Val i -> i
                             | _ -> raise (TypeError "Less second")) in
		     if x < y then Bool_Val true else Bool_Val false)
     | GreaterEqual (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> i 
                             | _ -> raise (TypeError "GreaterEqual first")) in
		     let y = (match d with
			     | Int_Val i -> i
                             | _ -> raise (TypeError "GreaterEqual second")) in
		     if x >= y then Bool_Val true else Bool_Val false)
     | LessEqual (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let x = (match c with
		             | Int_Val i -> i 
                             | _ -> raise (TypeError "LessEqual first")) in
		     let y = (match d with
			     | Int_Val i -> i
                             | _ -> raise (TypeError "LessEqual second")) in
		     if x <= y then Bool_Val true else Bool_Val false)
     | Equal (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let decision = (match c with
		             | Int_Val i ->  (match d with 
                                              | Int_Val _ -> 0
					      | Bool_Val _ -> raise (TypeError "Equal not Ints"))
                             | Bool_Val b -> (match d with 
                                              | Int_Val _ -> raise (TypeError "Equal not Bools")
					      | Bool_Val _ -> 1)) in
                     if decision = 0 then (let x = (match c with
					   | Int_Val i -> i 
                                           | _ -> raise (TypeError "Equal first")) in
		                           let y = (match d with
			                   | Int_Val i -> i
                                           | _ -> raise (TypeError "Equal second")) in
					   if x = y then Bool_Val true else Bool_Val false) else (let x = (match c with
													 | Bool_Val b -> b 
                                                                                                         | _ -> raise (TypeError "Equal first")) in
		                                                                                         let y = (match d with
			                                                                                 | Bool_Val b -> b
                                                                                                         | _ -> raise (TypeError "Equal second")) in
					                                                                 if x = y then Bool_Val true else Bool_Val false))
     | NotEqual (a, b) -> (let c = eval_expr env a in
                     let d = eval_expr env b in
		     let decision = (match c with
		             | Int_Val i ->  (match d with 
                                              | Int_Val _ -> 0
					      | Bool_Val _ -> raise (TypeError "Equal not Ints"))
                             | Bool_Val b -> (match d with 
                                              | Int_Val _ -> raise (TypeError "Equal not Bools")
					      | Bool_Val _ -> 1)) in
                     if decision = 0 then (let x = (match c with
					   | Int_Val i -> i 
                                           | _ -> raise (TypeError "Equal first")) in
		                           let y = (match d with
			                   | Int_Val i -> i
                                           | _ -> raise (TypeError "Equal second")) in
					   if x <> y then Bool_Val true else Bool_Val false) else (let x = (match c with
													 | Bool_Val b -> b 
                                                                                                         | _ -> raise (TypeError "Equal first")) in
		                                                                                         let y = (match d with
			                                                                                 | Bool_Val b -> b
                                                                                                         | _ -> raise (TypeError "Equal second")) in
					                                                                 if x <> y then Bool_Val true else Bool_Val false))

let rec eval_stmt env s =
      match s with 
      | NoOp -> env
      | Seq (a, b) -> let ev1 = (eval_stmt env a) in (eval_stmt ev1 b)
      | Declare (data, id) -> (if (List.mem_assoc id env) then (raise (DeclareError "new"))
				  else (match data with
					 | Int_Type -> (id, Int_Val 0)::env
                                         | Bool_Type -> (id, Bool_Val false)::env))
      | Assign (id, exp) -> (if (List.mem_assoc id env) = false then (raise (DeclareError "assign"))
                                  else (let x = eval_expr env exp in
                                        let y = List.assoc id env in
                                        match x with
                                        | Int_Val i -> (match y with
                                                       | Int_Val _ -> (id, Int_Val i)::(List.remove_assoc id env)
                                                       | Bool_Val _ -> raise (TypeError "Assign Int"))
                                        | Bool_Val b -> (match y with
                                                       | Bool_Val _ -> (id, Bool_Val b)::(List.remove_assoc id env)
                                                       | Int_Val _ -> raise (TypeError "Assign Bool"))))
      | If (exp, s1, s2) -> let x = eval_expr env exp in
                             (match x with
                              | Bool_Val b -> if b then (eval_stmt env s1) else (eval_stmt env s2)
                              | Int_Val _ -> raise (TypeError "If"))
      | While (exp, body) -> let x = eval_expr env exp in
                             (match x with
                              | Bool_Val b -> if b then (eval_stmt (eval_stmt env body) s) else env
                              | Int_Val _ -> raise (TypeError "While"))
      | For (id, start, fin, body) -> (let c = eval_expr env start in
				       let x = (match c with
			       		    | Int_Val i -> i
                                            | _ -> raise (TypeError "start not int")) in
				       let d = eval_expr env fin in
				       let y = (match d with
					    | Int_Val i -> i 
                                            | _ -> raise (TypeError "end not int")) in
				       let inc_env = (id, Int_Val x)::(List.remove_assoc id env) in
				       let inc_val = List.assoc id inc_env in
				       let inc_expr = (match inc_val with
						      | Int_Val i -> i
                                                      | _ -> raise (TypeError "id not int after change")) in    
                                       if (x >= inc_expr && inc_expr <= y) then 
				       (let start_inc_env = (id, Int_Val (inc_expr+1))::(List.remove_assoc id inc_env) in
					let body_env = eval_stmt start_inc_env body in
				       let new_val = List.assoc id body_env in
				       let new_expr = (match new_val with
						      | Int_Val i -> i
                                                      | _ -> raise (TypeError "id not int after change")) in
				       let cont_env = eval_stmt body_env (For (id, (Int (new_expr)), fin, body)) in
				       cont_env)
				       else
				       (inc_env)
				      )     
      | Print exp -> (let x = eval_expr env exp in
                      match x with
                      | Int_Val i -> (let _ = (print_output_int i) in
					  let _ = print_output_newline () in env)
                      | Bool_Val b -> (let _ = (print_output_bool b) in 
                                          let _ = print_output_newline () in env))
                                   
