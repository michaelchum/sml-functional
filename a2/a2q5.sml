(* Michael Ho 260532097 *)

(* QUESTION 5 *)

(* The following two datatypes are taken directly from the assignment intructions *)
datatype Exptree =
	Const of int |
	Var of string |
	Add of Exptree * Exptree |
	Mul of Exptree * Exptree;

type Bindings = (string * int) list

(* The input type of lookup is (x:string,y) and not Bindings because it was specified: val lookup = fn : string * (string * 'a) list -> 'a option *)
(* This assumes the list is in order to retrieve the latest integer *)
fun lookup (a:string,[]) = NONE
	| lookup (a:string,(x:string,y)::xs) =
		if (a=x andalso (lookup(a,xs)=NONE)) then SOME y
		else lookup(a,xs)

(* Here insert always inserts by order of the STRING (as the key) not the int, it uses String.compare(a,x) to check lexicographially which string should come before *)
fun insert (a:string, b, []) = [(a,b)]
	| insert (a:string, b, (x:string,y)::xs) = 
		if (String.compare(a,x)=LESS) then [(a,b),(x,y)]@xs
		else (x,y)::insert(a,b,xs)

(* eval3 always return an option, valOf is used to convert the option to int type for addition and multiplication *)
fun eval3 (Const(x):Exptree, y:Bindings) = SOME x
	| eval3 (Var(x), y:Bindings) = lookup(x,y)
	| eval3 (Add(a:Exptree,b:Exptree), y:Bindings) = SOME (Option.valOf(eval3(a,y)) + Option.valOf(eval3(b,y)))
	| eval3 (Mul(a:Exptree,b:Exptree), y:Bindings) = SOME (Option.valOf(eval3(a,y)) * Option.valOf(eval3(b,y)))

