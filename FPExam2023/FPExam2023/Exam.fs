module Exam2023
(* If you are importing this into F# interactive then comment out
   the line above and remove the comment for the line bellow.

   Do note that the project will not compile if you do this, but 
   it does allow you to work in interactive mode and you can just remove the '=' 
   to make the project compile again.

   You will also need to load JParsec.fs. Do this by typing
   #load "JParsec.fs" 
   in the interactive environment. You may need the entire path.

   Do not remove the module declaration (even though that does work) because you may inadvertently
   introduce indentation errors in your code that may be hard to find if you want
   to switch back to project mode. 

   Alternative, keep the module declaration as is, but load ExamInteractive.fsx into the interactive environment
   *)
(*
 module Exam2023 = 
 *)
(* 1: Logic *)

    type prop =  
    | TT  
    | FF  
    | And of prop * prop  
    | Or of prop * prop
    
    let p1 = And(TT, FF)  
    let p2 = Or(TT, FF)  
    let p3 = And(Or(TT, And(TT, FF)), TT)  
    let p4 = And(Or(TT, And(TT, FF)), Or(FF, And(TT, FF)))
    
(* Question 1.1: Evaluation *)
    let rec eval p = 
        match p with
        | TT -> true
        | FF -> false
        | And(x, y) -> (eval x) && (eval y)
        | Or(x, y) -> (eval x) || (eval y)

    
(* Question 1.2: Negation and implication *)
    let rec negate p =
        match p with
        | TT -> FF
        | FF -> TT
        | And(x, y) -> Or((negate x), (negate y)) 
        | Or(x, y) -> And((negate x), (negate y))

    let rec implies p q = Or ((negate p), q)

(* Question 1.3: Bounded universal quantifiers *)
    let rec forall f lst  = 
        match lst with 
        | [] -> TT
        | x::xs -> And((f x), (forall f xs))  
        


(* Question 1.4: Bounded existential quantifiers *)

    let exists f lst = 
        lst |> List.map f |> List.fold (fun acc x -> Or(acc, x)) FF
    
(* Question 1.5: Bounded unique existential quantifiers *)

    let existsOne f lst = 
        lst 
        |> List.map f 
        |> List.filter (fun x -> x = TT) 
        |> function
            | [_] -> TT
            | _ -> FF

    let existsOne2 f lst =
        let lst' = List.map (fun x -> (x, f x)) lst
        lst'
        |> List.map (fun (x, fx) -> And(fx, forall (fun (y, fy) -> if x = y then TT else negate(fy)) lst'))
        |> List.fold (fun acc x -> Or(acc, x)) FF

(* 2: Code Comprehension *)
 
    let rec foo xs ys =  
        match xs, ys with  
        | _       , []                  -> Some xs   
        | x :: xs', y :: ys' when x = y -> foo xs' ys'   
        | _       , _                   -> None  
          
    let rec bar xs ys =
        match foo xs ys with
        | Some zs -> bar zs ys
        | None -> match xs with
                  | [] -> []
                  | x :: xs' -> x :: (bar xs' ys)  

    let baz (a : string) (b : string) =  
        bar [for c in a -> c] [for c in b -> c] |>  
        List.fold (fun acc c -> acc + string c) ""

(* Question 2.1: Types, names and behaviour *)

    (* 
    
    Q: What are the types of functions foo, bar, and baz?

    A: 
        foo has type list<'a> -> list<'a> -> option<list<'a>>
        bar has type list<'a> -> list<'a> -> list<'a>
        baz has type string -> string -> string

    Q: What do the function foo, bar, and baz do.
       Focus on what they do rather than how they do it.

    A: 
        foo takes two lists xs and ys of the same type, 
        and checks if the first element is the same, 
        if it is then it removes the element from both list
        and recursively calls with the two new lists. 
        If ys is empty, then it returns xs.
        If the first elements of the lists are not the same, then it returns None.
        
        Shorter explanation:
        foo takes two lists xs and ys, and checks if ys is a prefix of xs,
        if it is it removes the prefix.
        If ys is not a prefix of xs it returns None.

        bar takes 2 lists xs and ys and checks if ys is a sublist of xs,
        if it is it removes the sublist every time it occurs in xs and return the new list.

        baz takes 2 strings a and b and checks if b is a substring of a,
        if it is it removes the substring every time it occurs in a.

    
    Q: What would be appropriate names for functions 
       foo, bar, and baz?

    A: 
        foo: tryRemovePrefix
        bar: removeSublist
        baz: removeSubstring  
    *)
        

(* Question 2.2: Code snippets *)

 
    (* 
    The function baz contains the following three code snippets. 

    * A: `[for c in a -> c]`
    * B: `[for c in b -> c]`
    * C: `List.fold (fun acc c -> acc + string c) ""`

    Q: In the context of the baz function, i.e. assuming that `a` and `b` are strings, 
       what are the types of snippets A, B, and C and what are they -- 
       focus on what they do rather than how they do it.
    
    A:
        A: list<char>
        B: list<char>
        C: list<char> -> string

    
    Q: Explain the use of the `|>`-operator in the baz function.

    A: 
        |> is the piping operator. It takes a value and uses it as an argument in the function that you pipe into.
        In the context of the baz function the piping operator is used to take the value that is returned from the bar function
        and uses that as the last argument for the List.fold function.

    *)

(* Question 2.3: No recursion *) 

    let foo2 xs ys = 
        let ysLen = List.length ys
        let xs', xs'' = List.splitAt ysLen xs 
        if xs' = ys then Some xs'' else None


(* Question 2.4 *)

    (*

    Q: The function `bar` is not tail recursive. Demonstrate why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation. 
       You need to make clear what aspects of the evaluation tell you that the function 
       is not tail recursive. Keep in mind that all steps in an evaluation chain must 
       evaluate to the same value ( (5 + 4) * 3 --> 9 * 3 --> 27 , for instance).
       
       You do not have to step through the foo-function. You are allowed to evaluate 
       that function immediately.

    A: 
        < Did not bother >
    *)

(* Question 2.5 *)

    let barTail xs ys = 
        let rec aux xs ys c =
            match foo xs ys with
            | Some zs -> aux zs ys c
            | None -> match xs with
                      | [] -> [] |> c
                      | x :: xs' -> aux xs' ys (fun r -> x :: r |> c)
        aux xs ys id
    

(* 3: Collatz Conjecture *)

(* Question 3.1: Collatz sequences *)

    let collatz x = 
        let rec aux x xs =
            match x with
            | _ when x <= 0 -> failwith $"Non positive number: {x}"
            | _ when x = 1 -> (1 :: xs)
            | _ when x % 2 = 0 -> aux (x/2) (x :: xs)
            | _ -> aux (3 * x + 1) (x :: xs)
        (aux x []) |> List.rev
    


(* Question 3.2: Even and odd Collatz sequence elements *)

    let evenOddCollatz x = 
        let rec aux x (E, O) =
            match x with
            | _ when x <= 0 -> failwith $"Non positive number: {x}"
            | _ when x = 1 -> (E, O + 1)
            | _ when x % 2 = 0 -> aux (x/2) (E + 1, O)
            | _ -> aux (3 * x + 1) (E, O + 1)
        aux x (0,0) 

(* Question 3.3: Maximum length Collatz Sequence *)
  
    let maxCollatz x y = 
        let rec aux z longest = 
            match z with 
            | _ when z < x -> longest
            | _ ->
                let length = collatz z |> List.length 
                match length with
                | _ when length <= snd longest -> aux (z - 1) longest
                | _ -> aux (z - 1) (z, length)
        aux y (0, 0)


(* Question 3.4: Collecting by length *)
    let collect x y = 
        let rec aux z map = 
            match z with 
            | _ when z < x -> map
            | _ ->
                let length = collatz z |> List.length 
                match Map.tryFind length map with 
                | Some s -> 
                    let s' = Set.add z s 
                    let map' = Map.add length s' map 
                    aux (z-1) map'
                | None -> 
                    let s = Set.add z Set.empty 
                    let map' = Map.add length s map
                    aux (z-1) map'
        aux y Map.empty
    
(* Question 3.5: Parallel maximum Collatz sequence *)
    let parallelMaxCollatz x y n = 
        failwith "Not implemented"



(* 4: Memory machines *)

    type expr =  
    | Num    of int              // Integer literal
    | Lookup of expr             // Memory lookup
    | Plus   of expr * expr      // Addition
    | Minus  of expr * expr      // Subtraction
          
    type stmnt =  
    | Assign of expr * expr      // Assign value to memory location
    | While  of expr * prog      // While loop
      
    and prog = stmnt list        // Programs are sequences of statements

    let (.+.) e1 e2 = Plus(e1, e2)  
    let (.-.) e1 e2 = Minus(e1, e2)  
    let (.<-.) e1 e2 = Assign (e1, e2)
    
    // Starting from memory {0, 0, 2, 0}
    let fibProg x =  
        [Num 0 .<-. Num x       // {x, 0, 2, 0}
         Num 1 .<-. Num 1       // {x, 1, 2, 0}
         Num 2 .<-. Num 0       // {x, 1, 0, 0}
         While (Lookup (Num 0), 
                [Num 0 .<-. Lookup (Num 0) .-. Num 1  
                 Num 3 .<-. Lookup (Num 1)  
                 Num 1 .<-. Lookup (Num 1) .+. Lookup (Num 2)  
                 Num 2 .<-. Lookup (Num 3)  
                ]) // after loop {0, fib (x + 1), fib x, fib x}
         ]

(* Question 4.1: Memory blocks *)

    type mem = M of array<int>

    let emptyMem x  = 
        Array.create x 0 |> M
    let lookup  (M m) i = Array.get m i 
    let assign (M m) i v = 
        let m' = M (Array.insertAt i v m)
        m'

(* Question 4.2: Evaluation *)

    let rec evalExpr m e = 
        match e with
        | Num x -> x
        | Lookup e' -> evalExpr m e' |> lookup m 
        | Plus (e1, e2) -> (evalExpr m e1 |> lookup m) + (evalExpr m e2 |> lookup m) 
        | Minus (e1, e2) -> (evalExpr m e1 |> lookup m) - (evalExpr m e2 |> lookup m) 


    let rec evalStmnt m s = 
        match s with
        | Assign (e1, e2) -> 
            let m' = assign m (evalExpr m e1) (evalExpr m e2)
            m'
        | While (e, p) -> 
            match evalExpr m e with
            | 0 -> m
            | _ -> evalProg m (p @ [While(e, p)])
    and evalProg m p =
        match p with
        | [] -> m
        | _ -> List.fold (fun m' stmnt -> evalStmnt m' stmnt) m p

    
(* Question 4.3: State monad *)
    type StateMonad<'a> = SM of (mem -> ('a * mem) option)  
      
    let ret x = SM (fun s -> Some (x, s))  
    let fail  = SM (fun _ -> None)  
    let bind f (SM a) : StateMonad<'b> =   
        SM (fun s ->   
            match a s with   
            | Some (x, s') ->  let (SM g) = f x               
                               g s'  
            | None -> None)  
          
    let (>>=) x f = bind f x  
    let (>>>=) x y = x >>= (fun _ -> y)  
      
    let evalSM m (SM f) = f m

    let lookup2 _ = failwith "not implemented"
    let assign2 _ = failwith "not implemented"

(* Question 4.4: State monad evaluation *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = StateBuilder()

    let evalExpr2 _ = failwith "not implemented"
    let evalStmnt2 _ = failwith "not implemented"
    let evalProg2 _ = failwith "not implemented"
    
(* Question 4.5: Parsing *)
    
    open JParsec.TextParser
      
    let ParseExpr, eref = createParserForwardedToRef<expr>()  
    let ParseAtom, aref = createParserForwardedToRef<expr>()  
      
    let parseExpr _ = failwith "not implemented" // Parse addition and minus
          
    let parseAtom _ = failwith "not implemented" // Parse numbers and lookups

//    Uncomment the following two lines once you finish parseExpr and parseAtom             
//    do aref := parseAtom  
//    do eref := parseExpr  
      