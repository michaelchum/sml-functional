(* Michael Ho 260532097 *)

open Real

(* QUESTION 1 *)

fun sumlist [] = 0.0
  | sumlist (x::xs) = x + sumlist(xs)

fun squarelist [] = []
  | squarelist ((x:real)::xs) = (x*x)::(squarelist(xs))

(* The following functions assume that the list in not empty.*)
fun mean l = sumlist(l)/(fromInt(length(l)))

fun subtract (nil,n) = nil
	| subtract (((x:real)::xs), (n:real)) = (x-n)::subtract(xs,n)

(* Works with real list only *)
fun variance (l) = sumlist(squarelist(subtract(l,mean(l))))/fromInt(length(l))

(* QUESTION 2 *)
(* Runs in O(n) *)
fun member (x,nil) = false
	| member (x,l) = if(x=hd(l)) then true 
	else member(x,tl(l))

fun remove (x,nil) = []
	| remove (x,l) = if(x=hd(l)) then remove(x,tl(l))
	else hd(l)::remove(x,tl(l))

(* QUESTION 3 *)
fun isolate [] = []
	| isolate l = hd(l)::isolate(remove(hd(l),tl(l)))

(* QUESTION 4 *)
fun common ([],[]) = []
	| common ([],l) = []
	| common (l1,l2) = if(member(hd(l1),l2)) then hd(l1)::common(remove(hd(l1),tl(l1)),remove(hd(l1),l2))
	else common(remove(hd(l1),tl(l1)),l2)

(* QUESTION 5 *)

(* ONLY WORKS WITH A REAL LIST *)

fun split [] = ([],[])
	| split [x] = ([x],[]) 
	| split (x1::x2::xs) = let val (ys, zs) = split xs in ((x1::ys), (x2::zs)) end;

fun merge ([],[]) = []
	| merge (l1,[]) = l1
	| merge ([],l1) = l1
	| merge (l1,l2) = if(hd(l1)<hd(l2)) then hd(l1)::hd(l2)::merge(tl(l1),tl(l2)) else hd(l2)::hd(l1)::merge(tl(l1),tl(l2))

fun mergesort [] = []
	| mergesort [x]  = [x]
	| mergesort l = let val (left, right) = split(l) in merge(mergesort(left),mergesort(right)) end;