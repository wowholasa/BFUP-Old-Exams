﻿module Exam2022
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
 module Exam2022 = 
 *)

(* 1: Grayscale images *)

    type grayscale =
    | Square of uint8
    | Quad of grayscale * grayscale * grayscale * grayscale
    
    let img = 
      Quad (Square 255uy, 
            Square 128uy, 
            Quad(Square 255uy, 
                 Square 128uy, 
                 Square 192uy,
                 Square 64uy),
            Square 0uy)
    
(* Question 1.1 *)
    let rec countWhite img =
        match img with
        | Square value ->
            match value with 
            | 255uy -> 1
            | _ -> 0

        | Quad (q1, q2, q3, q4) -> 
            countWhite q1 + 
            countWhite q2 +
            countWhite q3 +
            countWhite q4

(* Question 1.2 *)
    let rec rotateRight img =
        match img with 
        | Square _ -> img
        | Quad (q1, q2, q3, q4) -> 
            Quad (
                rotateRight q4,
                rotateRight q1,
                rotateRight q2,
                rotateRight q3
            )

(* Question 1.3 *)
    let rec map (mapper : uint8 -> grayscale) img = 
        match img with
        | Square value -> mapper value
        | Quad (q1, q2, q3, q4) ->
            Quad (
                map mapper q1,
                map mapper q2,
                map mapper q3,
                map mapper q4
            )

    let bitmap img = 
        let aux (value : uint8) =
            match value with
            | v when v <= 127uy -> Square 0uy
            | _ -> Square 255uy
        map aux img


(* Question 1.4 *)

    let rec fold (folder: 'a -> uint8 -> 'a) (acc: 'a) (img: grayscale): 'a =
        match img with
        | Square value -> folder acc value
        | Quad (q1, q2, q3, q4) ->
            let acc = fold folder acc q1
            let acc = fold folder acc q2
            let acc = fold folder acc q3
            fold folder acc q4


    let countWhite2 img = failwith ""

(* 2: Code Comprehension *)
    let rec foo =
        function
        | 0 -> ""
        | x when x % 2 = 0 -> foo (x / 2) + "0"
        | x when x % 2 = 1 -> foo (x / 2) + "1"

    let rec bar =
        function
        | []      -> []
        | x :: xs -> (foo x) :: (bar xs)

(* Question 2.1 *)

    (* 
    
    Q: What are the types of functions foo and bar?

    A: foo has the type int -> string
       bar has the type int list -> string list 


    Q: What does the function bar do.
       Focus on what it does rather than how it does it.

    A: foo takes an integer and returns it's binary string representation
       bar takes a list of integers and returns a list of the binary string representations.
    
    Q: What would be appropriate names for functions 
       foo and bar?

    A: foo: convertToBinary
       bar: convertListToBinary 
        
    Q: The function foo does not return reasonable results for all possible inputs.
       What requirements must we have on the input to foo in order to get reasonable results?
    
    A: The input must be non-negative.
    *)
        

(* Question 2.2 *)

 
    (* 
    The function foo compiles with a warning. 

    
    Q: What warning and why?

    A: <Your answer goes here>

    *)

    let foo2 _ = failwith "not implemented"

(* Question 2.3 *) 

    let bar2 _ = failwith "not implemented"

(* Question 2.4 *)

    (*

    Q: Neither foo nor bar is tail recursive. Pick one (not both) of them and explain why.
       To make a compelling argument you should evaluate a function call of the function,
       similarly to what is done in Chapter 1.4 of HR, and reason about that evaluation.
       You need to make clear what aspects of the evaluation tell you that the function is not tail recursive.
       Keep in mind that all steps in an evaluation chain must evaluate to the same value
       ((5 + 4) * 3 --> 9 * 3 --> 27, for instance).

    A: <Your answer goes here>
    
    Q: Even though neither `foo` nor `bar` is tail recursive only one of them runs the risk of overflowing the stack.
       Which one and why does  the other one not risk overflowing the stack?

    A: <Your answer goes here>

    *)
(* Question 2.5 *)

    let fooTail _ = failwith "not implemented"

(* Question 2.6 *)
    let barTail _ = failwith "not implemented"

(* 3: Matrix operations *)

    type matrix = int[,]

    let init f rows cols = Array2D.init rows cols f

    let numRows (m : matrix) = Array2D.length1 m
    let numCols (m : matrix) = Array2D.length2 m

    let get (m : matrix) row col = m.[row, col]
    let set (m : matrix) row col v = m.[row, col] <- v

    let print (m : matrix) =
        for row in 0..numRows m - 1 do
            for col in 0..numCols m - 1 do
                printf "%d\t" (get m row col)
            printfn ""

(* Question 3.1 *)

    let failDimensions _ = failwith "not implemented"

(* Question 3.2 *)

    let add _ = failwith "not implemented"

(* Question 3.3 *)
    
    let m1 = (init (fun i j -> i * 3 + j + 1) 2 3) 
    let m2 = (init (fun j k -> j * 2 + k + 1) 3 2)

    let dotProduct _ = failwith "not implemented"
    let mult _ = failwith "not implemented"

(* Question 3.4 *)
    let parInit _ = failwith "not implemented"

(* 4: Stack machines *)

    type cmd = Push of int | Add | Mult
    type stackProgram = cmd list

(* Question 4.1 *)

    type stack = unit (* replace this entire type with your own *)
    let emptyStack _ = failwith "not implemented"

(* Question 4.2 *)

    let runStackProgram _ = failwith "not implemented"

(* Question 4.3 *)
    
    type StateMonad<'a> = SM of (stack -> ('a * stack) option)

    let ret x = SM (fun s -> Some (x, s))
    let fail  = SM (fun _ -> None)
    let bind f (SM a) : StateMonad<'b> = 
        SM (fun s -> 
            match a s with 
            | Some (x, s') -> 
                let (SM g) = f x             
                g s'
            | None -> None)
        
    let (>>=) x f = bind f x
    let (>>>=) x y = x >>= (fun _ -> y)

    let evalSM (SM f) = f (emptyStack ())

    let push _ = failwith "not implemented"
    let pop _ = failwith "not implemented"

(* Question 4.4 *)

    type StateBuilder() =

        member this.Bind(f, x)    = bind x f
        member this.Return(x)     = ret x
        member this.ReturnFrom(x) = x
        member this.Combine(a, b) = a >>= (fun _ -> b)

    let state = new StateBuilder()

    let runStackProg2 _ = failwith "not implemented"
    
(* Question 4.5 *)
    
    open JParsec.TextParser

    let parseStackProg _ = failwith "not implemented"