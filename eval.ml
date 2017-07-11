open Syntax

open Parser
open Lexer




let rec load s env = 
	"", s @ env


let subst_assoc = List.append

let rec print_pattern pa = 
	match pa with
	| Const(x,xs) -> 
		(match xs with
		 | [] -> print_string x
		 | r :: rs -> 
			print_string (x ^ "(");
			print_pattern r;
			List.iter (fun x -> print_string ","; print_pattern x) rs;
			print_string ")")
	| Param(p) -> print_string p

let print_func (na,pas) = 
	print_string (na ^ "(");
	match pas with
	| [] -> print_string ")" 
	| x :: xs -> 
		print_pattern x;
		List.iter (fun p -> print_string ","; print_pattern p) xs;
		print_string ")" 

let print_funcs = List.iter (fun f -> print_func f; print_string ", ")

let print_decl (d,bd) = 
	print_func d;
	if List.length bd = 0 then 
		print_string ".\n"
	else (
		print_string " :- ";
		List.iter print_func bd;
		print_string ".\n")

let print_env env = 
	List.iter print_decl env

let rec print_subst sub = 
	match sub with
	| [] -> ()
	| (s,x) :: xs -> 
		print_string (s ^ " ::= ");
		print_pattern x;
		print_string ", ";
		print_subst xs

let rec subst_pattern (fr,tot) pa =
	match pa with
	| Const(na,xs) -> Const(na,List.map (fun p -> subst_pattern (fr,tot) p) xs)
	| Param x -> if x = fr then tot else pa

let rec subst_pattern_rec sub pa = 
	match sub with
	| [] -> pa
	| x :: xs -> subst_pattern x (subst_pattern_rec xs pa)


let rec find_pat_fvs pa = 
	match pa with
	| Const(na,xs) -> List.flatten (List.map find_pat_fvs xs)
	| Param x -> [x]


let rec unique v = 
	match v with
	| [] -> []
	| x :: xs -> 
		let rv = unique xs in
			if List.mem x rv then rv else (x :: rv)



let rec find_func_fvs (fn,xs) = 
	unique (List.flatten (List.map find_pat_fvs xs))

let extract_subst sub func = 
	let fvs = find_func_fvs func in
		List.map (fun x -> (x,subst_pattern_rec sub (Param(x)))) fvs

let rec find_decl_fvs (fn,xs) = 
	unique ((find_func_fvs fn) @ (List.flatten (List.map find_func_fvs xs)))


let subst_patterns sub (pas : pattern list) = List.map (fun pa -> subst_pattern_rec sub pa) pas

let subst_func sub ((na,bo) : func) = (na,subst_patterns sub bo)

let subst_funcs sub (prs : func list) = List.map (fun x -> subst_func sub x) prs

let subst_decl sub ((f,ds) : decl) = (subst_func sub f,subst_funcs sub ds)

let subst_envs sub es = List.map (fun x -> subst_decl sub x) es


let rec unify_pattern : pattern -> pattern -> subst option = fun x -> fun y -> 
	(* print_string " x .. ";
	print_pattern x;
	print_string "  y .. ";
	print_pattern y;
	print_newline (); *)
	match x,y with
	| Param a, b | b,Param a -> Some [(a,b)] (* とりあえず、出現チェックをしない *)
	| Const (x,xs), Const(y,ys) -> 
		if x = y then unify_patterns xs ys else None

and unify_patterns (vx : pattern list) vy =
	match vx,vy with
	| [],[] -> Some []
	| (x :: xs),(y :: ys) -> 
		(match unify_pattern x y with
		 | None -> None
		 | Some s1 -> 
		 	match unify_patterns (subst_patterns s1 xs) (subst_patterns s1 ys) with
		 	| None -> None
		 	| Some s2 -> Some (subst_assoc s1 s2))
	| _ -> None

(*
let rec unify_funcs vx vy = 
	match vx,vy with
	| [],[] -> Some []
	| ((nx,x) :: xs),((ny,y) :: ys) -> 
		if nx = ny then
			(match unify_patterns x y with
			 | None -> None
			 | Some s1 -> 
			 	match unify_funcs (subst_funcs s1 xs) (subst_funcs s1 ys) with
			 	| None -> None
			 	| Some s2 -> Some (subst_assoc s1 s2))
		else None
	| _ -> None
*)



let gen_var =
	let c = ref 0 in (fun () -> (c := (!c) + 1; !c))

let fleshen_decl decl = 
	let fvs = find_decl_fvs decl in
	let sub = List.map (fun x -> (x,Param("$" ^ (string_of_int (gen_var ())) ^ x))) fvs in
		subst_decl sub decl

(*
継続っぽいことをしないといけない
継続渡しにして、その中でexceptionを呼んでもらおう、と。
*)

let rec iter_dfs (funs: func list) (subst : subst) (env :env) cont = 
	(*
	print_subst subst;
	print_newline ();
	print_string (string_of_int (List.length funs));
	print_newline ();
	print_funcs funs;
	print_newline ();
	print_newline (); *)
	match funs with
	| [] -> cont subst
	| (fn,fvs) :: xs -> 
		let rec itersearch : env -> unit = fun nenv ->
			match nenv with
			| [] -> ()
			| decl :: renv ->
				let ((k,vs),asps) = fleshen_decl decl in
				(if k = fn then
					match unify_patterns fvs vs with
					| None -> ()
					| Some tsub -> 
						(* print_string "subst!![\n";
						print_subst tsub;
						print_string "\n]\n"; *)
						iter_dfs ((subst_funcs tsub asps) @ xs) (subst_assoc tsub subst) env cont
				else ());
					itersearch renv
		in
			itersearch env


exception Finish_search

let rec query fn env = 
	try
		iter_dfs [fn] [] env (fun sub-> 
			let ts = extract_subst sub fn in
			print_subst ts;
			print_string "\nAny more? ";
			flush stdout;
			let s = read_line () in
				if s = ";" then () 
				else raise Finish_search);
		print_string "false.\n" 
	with
		| Finish_search -> ()

(*
# load['tes.pl'].
add(s(z),Y,Z).
*)
