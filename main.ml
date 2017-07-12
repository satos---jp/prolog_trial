open Syntax
open Eval



let rec repl env = 
	print_string "?- ";
	flush stdout;
	try
		let dir = Parser.repl Lexer.token (Lexing.from_channel stdin) in
			match dir with
			| Load filename -> 
				let ds = Parser.decls Lexer.token (Lexing.from_channel (open_in filename)) in
				let defs,toenv = load ds env in
					print_endline ("load " ^ filename);
					repl toenv
			| List -> 
				print_env env;
				print_newline ();
				repl env
			| Query func -> 
				query func env;
				repl env
			| Quit -> 
				()
	with
		| Parsing.Parse_error ->  (
			print_endline "parse_error";
			repl env)
		| Failure s -> (
			Printf.printf "Failure(%s)\n" s;
			repl env)

let _ = repl []
