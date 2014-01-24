fun catalan 0 = 1
| catalan n = ((4 * n - 2) * catalan(n - 1))
div (n + 1)
fun print_catalans(n) =
if (n > 15) then ()
else (print (Int.toString(catalan n) ^ "\n");
print_catalans(n+1))
