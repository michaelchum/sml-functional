(* Michael Ho *)
(* 260532097 *)

(* Question 2 outline. *)
datatype transactions =
         Withdraw of int | Deposit of int | Check_balance

exception wrongPassword
exception overDrawn of int

fun make_protected_account(opening_balance: int,password: string) =
    let
        val balance = ref opening_balance
        val passwd = password
    in
     let
        (* val acc = *)
        fun acc (pw:string,t:transactions) = 
            if (pw=passwd) then
                case t of
                    Withdraw(amount) => if (!balance > amount) then (balance := !balance - amount; print("The new balance is "^Int.toString(!balance)^"\n"))
                    else raise overDrawn(!balance)
                    | Deposit(amount) => (balance := !balance + amount; print("The new balance is "^Int.toString(!balance)^"\n"))
                    | Check_balance => print("The balance is "^Int.toString(!balance)^"\n")
            else raise wrongPassword

     in
         fn (pw:string, t: transactions) =>
            (acc(pw,t)
             handle wrongPassword => print("Wrong Password.\n")
                  | (overDrawn n) =>
                    print
                        ("Insufficient funds for this transaction. The balance is "^Int.toString(n)^"\n"))
     end
    end