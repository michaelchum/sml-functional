(* Michael Ho 260532097 *)

open Real

(* QUESTION 1 *)

fun abs(x:real) = if (x < 0.0) then ~x else x
fun close(x:real,y:real,tol) = (abs(x-y) < tol)
exception DivZero
exception Diverge
fun deriv (f, dx:real) = fn x => ((f(x + dx) - f(x))/dx)

(* Helper function to decrease integer by 1 because I have some issue with the - operator being real * real *)
fun dec(x:int) = let val y = real(x) in round(y-1.0) end

(* In order to avoid divde-by-zero situations we raise an exception. *)
fun improve(guess:real,f,tol) = 
  let
     val derivative = deriv(f,0.01)
     val den = derivative(guess)
  in
    if (close(den,0.0,tol:real)) then raise DivZero
    else 
      guess-(f(guess)/den)
  end

(* In order to avoid divergence, we abort the computation if it takes more
than 10000 steps. *)

fun newton(f,guess:real,tol:real) =
  let
    fun helper(f,guess:real,tol:real,counter:int) =
      if (counter = 0) then raise Diverge
      else
      	let 
      		val improvedguess = improve(guess,f,tol)
      	in
      		if (close(f(improvedguess),0.0,tol)) then improvedguess
      		else
        		helper(f,improvedguess,tol,dec(counter))
        end
  in
    helper(f,guess,tol,10000)
  end