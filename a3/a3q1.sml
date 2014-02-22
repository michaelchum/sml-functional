(* Michael Ho *)
(* 260532097 *)

(* Here is the datatype for Question 1. *)
datatype 'a rlist = Empty | RCons of 'a * (('a rlist) ref)

(* The code below is to display an rlist as an ordinary list or to show it
on the screen.  The function ``less'' is an example for testing purposes.
Please note the types carefully.
*)

fun display Empty = nil
  | display (RCons(h, rt)) = h :: (display(!rt))

val foo = ref (RCons(1, ref (RCons(2, ref Empty))))

fun less(x,y) = (x < y)

fun show (ref Empty) = (print("\n"))
  | show (ref (RCons(h,rt))) = (print(Int.toString(h)^","); show(rt))

(* Use tail recursion and check using the function f's boolean output to correctly acknowledge where to insert the element *)
(* Everything here is referenced *)
fun insert (f:('a * 'a) -> bool, i:'a, ref Empty:'a rlist ref) = ref (RCons(i, ref Empty)) (* If an element is inserted to an empty list *)
    | insert (f, i, ref (RCons(h,rt))) =
      let val check = f(i,h) in
        if (check) then ref (RCons(i, ref (RCons(h,rt))))
        else ref (RCons(h, insert(f,i,rt)))
      end

