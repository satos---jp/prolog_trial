
type pattern = 
	| Const of string * (pattern list)
	| Param of string

type func = string * (pattern list)

type func_with_cut = 
	| Func of func
	| Cut of int

type decl = func * (func_with_cut list)

type env = decl list

type subst = (string * pattern) list

type direct = 
	| Load of string
	| List
	| Query of func_with_cut list
	| Quit


(*
hoge(x).
huga(y(nyan,piyo)).
huga(y(huga,piyo)).
huga(y(X),_) :- huga(y(X,_)).

はい。
(あんまり従わんでもいい気がするけど)
*)

