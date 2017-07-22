open Syntax

open Parser
open Lexer


let rec load s env = 
	"", s @ env

let subst_assoc a b = List.append a b

let rec listpattern2list pa = 
	match pa with
	| Const("@cons",[a;b]) -> 
		(match listpattern2list b with
		 | None -> None
		 | Some xs -> Some (a :: xs))
	| Const("@nil",[]) -> Some []
	| _ -> None


let rec print_pattern_list_rest v = 
	match v with
	| [] -> print_string "]"
	| [x] -> 
		print_pattern x;
		print_string "]"
	| x :: xs -> 
		print_pattern x;
		print_string ",";
		print_pattern_list_rest xs

and print_pattern pa = 
	match listpattern2list pa with
	| Some v -> 
		print_string "[";
		print_pattern_list_rest v
	| None -> (
		match pa with
		| Const("@cons",[a;b]) -> 
		print_string "[";
		print_pattern a;
		print_string "|";
		print_pattern b;
		print_string "]"
		| Const(x,xs) -> 
			(match xs with
			 | [] -> print_string x
			 | r :: rs -> 
				print_string (x ^ "(");
				print_pattern r;
				List.iter (fun x -> print_string ","; print_pattern x) rs;
				print_string ")")
		| Param(p) -> print_string p)



let print_func (na,pas) = 
	print_string na;
	match pas with
	| [] -> ()
	| x :: xs -> 
		print_string "(";
		print_pattern x;
		List.iter (fun p -> print_string ","; print_pattern p) xs;
		print_string ")" 

let print_func_with_cut fc = 
	match fc with
	| Func(f) -> print_func f


let print_decl (d,bd) = 
	print_func d;
	match bd with
	| [] -> print_string ".\n"
	| x :: xs -> (
		print_string " :- ";
		print_func_with_cut x;
		List.iter (fun p -> print_string ", "; print_func_with_cut p)  xs;
		print_string ".\n")

let print_env env = 
	List.iter print_decl env



let rec print_subst sub = 
	match sub with
	| [] -> print_string  "true."
	| (s,x) :: xs -> 
		print_string "[ ";
		print_string (s ^ " ::= ");
		print_pattern x;
		List.iter (fun (ns,nx) -> 
			print_string ", ";
			print_string (ns ^ " ::= ");
			print_pattern nx) xs;
		print_string " ]"

let rec subst_pattern (fr,tot) pa =
	match pa with
	| Const(na,xs) -> Const(na,List.map (fun p -> subst_pattern (fr,tot) p) xs)
	| Param x -> if x = "_" then pa (* •Ï”_‚¾‚¯‚ÍA‚»‚Ì‚Ü‚Ü‘‚«Š·‚¦‚È‚¢‚æ‚¤‚É‚·‚é *)
		else if x = fr then tot else pa

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


let find_func_fvs (_,xs) = 
	List.flatten (List.map find_pat_fvs xs)

let rec find_func_cut_fvs fc = 
	match fc with
	| Func(f) -> find_func_fvs f
	
let extract_subst sub (funcs :func_with_cut list) = 
	let fvs = List.fold_left (fun s -> fun f -> 
		match f with
		| Func(x) -> (find_func_fvs x) @ s) [] funcs in
	List.map (fun x -> (x,subst_pattern_rec sub (Param(x)))) (unique fvs)

let rec find_decl_fvs ((fn,xs) : decl)  = 
	unique ((find_func_fvs fn) @ (List.flatten (List.map find_func_cut_fvs xs)))


let subst_patterns sub (pas : pattern list) = List.map (fun pa -> subst_pattern_rec sub pa) pas


let subst_func sub (na,bo) = 
	(na,subst_patterns sub bo)

let subst_func_cut sub (fc : func_with_cut) = 
	match fc with
	| Func(f) -> Func(subst_func sub f)

let subst_funcs sub (prs : func_with_cut list) = List.map (fun x -> subst_func_cut sub x) prs

let subst_decl (sub:subst) ((f,ds) : decl) = (subst_func sub f,subst_funcs sub ds)

let subst_envs sub es = List.map (fun x -> subst_decl sub x) es


let rec unify_pattern : pattern -> pattern -> subst option = fun x -> fun y -> 
	(* 
	print_string " x .. ";
	print_pattern x;
	print_string "  y .. ";
	print_pattern y;
	print_newline ();
	*)
	match x,y with
	| Param "_",_ | _,Param "_" -> Some [] (* _ ‚Ìê‡‚Í’uŠ·‚ð’Ç‰Á‚¹‚¸‚É’Ê‰ß‚³‚¹‚é *)
	| Param a, b | b,Param a -> 
		if List.mem a (find_pat_fvs b) then None else Some [(a,b)] (* oŒ»ŒŸ¸‚·‚é *)
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
		 	| Some s2 -> Some (subst_assoc s2 s1))
	| _ -> None

let gen_var =
	let c = ref 0 in (fun () -> (c := (!c) + 1; !c))

let fleshen_decl (decl : decl) = 
	let fvs = find_decl_fvs decl in
	let sub = List.map (fun x -> (x,Param("$" ^ (x ^ (string_of_int (gen_var ())))))) fvs in
		subst_decl sub decl

(*
Œp‘±‚Á‚Û‚¢‚±‚Æ‚ð‚µ‚È‚¢‚Æ‚¢‚¯‚È‚¢
Œp‘±“n‚µ‚É‚µ‚ÄA‚»‚Ì’†‚Åexception‚ðŒÄ‚ñ‚Å‚à‚ç‚¨‚¤A‚ÆB
*)


let rec iter_bfs ((funs: func_with_cut list),(subst : subst)) (env :env) cont = 
	(*
	print_subst subst;
	print_newline ();
	print_string (string_of_int (List.length funs));
	print_newline ();
	List.iter print_func_with_cut funs;
	print_newline ();
	print_newline (); 
	*)
	match funs with
	| [] -> cont subst; []
	| (Func(fn,fvs)) :: xs -> 
		let rec bfsmap acc nenv =
			match nenv with
			| [] -> acc
			| decl :: renv ->
				let ((k,vs),asps) = fleshen_decl decl in
				match (if k = fn then
					match unify_patterns fvs vs with
					| None -> None
					| Some tsub -> 
						(* print_string "subst!![\n";
					print_subst tsub;
						print_string "\n]\n"; *)
							Some ((subst_funcs tsub (asps @ xs)),(subst_assoc tsub subst))
				else None) with
				| None -> bfsmap acc renv
				| Some afs -> bfsmap (afs :: acc) renv
		in
			bfsmap [] env

let rec iter_bfs_rec funsubs env cont = 
	match funsubs with
	| [] -> ()
	| _ -> 
		let tfs = List.flatten (List.map (fun fsb -> iter_bfs fsb env cont) funsubs) in
		iter_bfs_rec tfs env cont

exception Finish_search

let rec query fns env = 
	try
		iter_bfs_rec [(fns,[])] env (fun sub-> 
			let ts = extract_subst sub fns in
			print_subst ts;
			(* print_string "\nAny more?"; *)
			flush stdout;
			let s = read_line () in
				if s = ";" then () 
				else raise Finish_search);
		print_string "false.\n" 
	with
		| Finish_search -> ()
