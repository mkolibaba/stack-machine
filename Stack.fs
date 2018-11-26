module vm.stack

type Stack = int list

let binary op stack =
    match stack with
    | h::ht::t -> (op h ht)::t
    | s -> 
        printfn "cannot perform: stack contains less than 2 elements"
        s

let add stack = binary (+) stack
let sub stack = binary (-) stack
let mul stack = binary (*) stack

let div stack =
    match stack with
    | h::ht::t -> 
        match ht with
        | 0 -> 
            printfn "divide by zero"
            stack
        | _ -> (h / ht)::t
    | s -> 
        printfn "cannot div" 
        s

let dup stack =
    match stack with
    | h::t -> h::h::t
    | s -> 
        printfn "cannot dup" 
        s

let pop stack =
    match stack with
    | _::t -> t
    | s -> 
        printfn "cannot pop"
        s

let push stack x = x::stack