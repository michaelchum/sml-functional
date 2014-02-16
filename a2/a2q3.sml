(* Michael Ho 260532097 *)

open Real

(* QUESTION 3 *)

datatype 'a tree = Empty | Node of 'a tree * 'a * 'a tree

(* Insert function to build a binary search tree taken from Lecture 5 *)
fun insert (n:int,Empty) = Node(Empty, n, Empty): int tree |
    insert (n,Node(l,m,r): int tree) = if (real(n) < real(m)) then Node(insert(n,l), m, r)
                                            else Node(l, m, insert(n,r));

(* Very simple function assuming preorder means retrieving recursively in order of NODE ITSELF, then LEFT SUBTREE, then RIGHT SUBTREE *)
(* Of course the result depends on the structure of the input tree (in this case, the order in which it was inserted and the properties of a BST) *)        
fun flatten Empty = []
  | flatten (Node(l,n,r)) = n::flatten(l)@flatten(r);


