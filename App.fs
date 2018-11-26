module sm

open System
open System.Text.RegularExpressions
open vm.stack

type Memory = int list
type VM = {
    stack: Stack
    status: Option<string>
    memory: Memory
}
type Processor = VM -> VM

let memsize = 4

let emptyVM = {stack = []; status = None; memory = List.replicate memsize 0}

let setStack (s: Stack) (vm: VM): VM = {vm with stack = s}
let setStatus (s: Option<string>) (vm: VM): VM = {vm with status = s}
let setMemory (m: Memory) (vm: VM): VM = {vm with memory = m}

type Semigroup<'a> = {
    append: 'a -> 'a -> 'a
}

type Monoid<'a> = {
    empty: 'a
    append: 'a -> 'a -> 'a
}

let strToInt =
    Int32.TryParse >> function 
    | true, v -> Some v
    | false, _ -> None

let rec insert v i l =
    match i, l with
    | 0, _::xs -> v::xs
    | i, x::xs -> x::insert v (i - 1) xs
    | _, [] -> failwith "index out of range"

[<EntryPoint>]
let main argv =
    let rec next (vm: VM): VM =
        printf ":> "
        let input = Console.ReadLine()
        match input with
        | "add" -> (add vm.stack, vm) |> ``set stack, next step``
        | "sub" -> (sub vm.stack, vm) |> ``set stack, next step``
        | "mul" -> (mul vm.stack, vm) |> ``set stack, next step``
        | "div" -> (div vm.stack, vm) |> ``set stack, next step``
        | "dup" -> (dup vm.stack, vm) |> ``set stack, next step``
        | "pop" -> (pop vm.stack, vm) |> ``set stack, next step``
        | i when Regex.Match(i, @"\!mem.[0-9]").Success ->
            let idx = i.Replace("!mem.", "") |> strToInt
            setMemory (insert vm.stack.Head idx.Value vm.memory) vm |> next
        | i when Regex.Match(i, @"\?mem.[0-9]").Success ->
            let idx = i.Replace("?mem.", "") |> strToInt
            (vm.memory.[idx.Value]::vm.stack, vm) |> ``set stack, next step``
        | "show" -> 
            printfn "%A" vm
            next vm
        | "exit" ->
            printfn "Exit. Result: %A" vm
            vm
        | x -> 
            match strToInt x with
            | Some xx -> (push vm.stack xx, vm) |> ``set stack, next step``
            | None -> 
                Console.WriteLine("Unknown command: " + x)
                next vm

    and ``set stack, next step`` tuple = tuple ||> setStack |> next

    next emptyVM |> ignore
    0