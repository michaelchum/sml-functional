(* Michael Ho *)
(* 260532097 *)

(* Question 3.  *)
datatype exp = Nat of int | Plus of exp * exp | Minus of exp * exp |
Mult of exp * exp | If of exp * exp * exp | Bool of bool | 
And of exp * exp | Not of exp | Eq of exp * exp | Lte of exp * exp | 
Var of string | Let of exp * (string * exp) | Fun of string * string * exp |
Apply of exp * exp

(* An auxiliary function to remove all copies of an element from a list. *)
fun remove(s,[])    = []
  | remove(s, x::l) = if (s=x) then remove(s,l) else x::(remove(s,l))

(* This function I wrote constructs the list of all free variables in an
expression.  It does not bother about removing duplicates. The remove
function above will remove duplicates when necessary. The variables are
stored as strings; we do not keep the Var constructor. *)

(* Examples for testing *)

val ex1 = Let(Nat(5),("a",(Plus(Var("a"),Nat(2)))))
val ex2 = Plus(Var("a"),Nat(2))
val ex3 = Let(Var("y"),("x",Plus(Var("x"),Var("z"))))
val ex4 = Let(Nat(3),("z",(Let(Nat(2),("y",ex3)))))
val ex5 = Fun("fac","n",If(Eq(Var("n"),Nat(0)),Nat(1), Mult(Var("n"),(Apply(Var("fac"),(Minus(Var("n"),Nat(1))))))))
val ex6 = Fun("f","n",Plus(Nat(3),Var("n")))
val ex7 = Fun("g","n",If(Eq(Var("n"),Nat(0)),Nat(0),Plus(Nat(1),Apply(Var("g"),(Minus(Var("n"),Nat(1)))))))

(* Helper function to determine if a string is inside a string list *)
fun contains (x:string,[]:string list):bool = false
	| contains (x,y::ys) = if(x=y) then true
	else contains(x,ys)

(* free_list function only takes an exp as input *)
fun free_list(x:exp):string list =
	let 
		val string_list = []
		(* helper function which takes as input, the same exp AND a string list a_string_list of all the ASSIGNED(NON-FREE) variables *)
		(* Therefore once a variable has been assigned (using Let) it is added to a_string_list *)
		(* Each variable is checked if it is contained in a_string_list, if it is not, it must be a free variable that's never been assigned*)
		fun helper(x:exp,a_string_list:string list) = 
			(* Handle each case of exp differently with recursion *)
			case x of 
				Nat(a) => []
				| Plus(a,b) => helper(a,a_string_list) @ helper(b,a_string_list)
				| Minus(a,b) => helper(a,a_string_list) @ helper(b,a_string_list)
				| Mult(a,b) => helper(a,a_string_list) @ helper(b,a_string_list)
				| If(a,b,c) => helper(a,a_string_list) @ helper(b,a_string_list) @ helper(c,a_string_list)
				| Bool(a) => []
				| And(a,b) => helper(a,a_string_list) @ helper(b,a_string_list)
				| Not(a) => helper(a,a_string_list)
				| Eq(a,b) => helper(a,a_string_list) @ helper(b,a_string_list)
				| Lte(a,b) => helper(a,a_string_list) @ helper(b,a_string_list)
				| Var(a) => if(contains(a,a_string_list)) then []
				else [a]
				(* This is the complicated part, if the first input of Let is a Var, we have to check whether this Var has been assigned beforehand *)
				(* If the Var's argument has been assigned, both the argument and the binding variables are not free variables *)
				(* If the Var's argument has never been assigned, it is a free variable but the replaced string (the binding variable) is not *)
				| Let(Var(y),(b,c)) => if(contains(y,a_string_list)) then helper(c,[y,b]@a_string_list) (* Both y and b are not free variables *)
						else y::helper(c,b::a_string_list) (* b is not a free variable because it is bound to y but y is a free variable *)
				| Let(a,(b,c)) => helper(c,b::a_string_list) (* b binds to an exp value which is not Var, thus it is not a free variable *)
				| Fun(a,b,c) => helper(c,[a,b] @ a_string_list) (* The function name and input parameter are not free variables *)
				| Apply(a,b) => helper(a,a_string_list) @ helper(b,a_string_list)
	in
		helper(x, string_list)
	end