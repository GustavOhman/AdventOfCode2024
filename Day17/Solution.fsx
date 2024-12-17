#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus

let fileName = 
    fsi.CommandLineArgs 
    |> List.ofArray
    |> function 
    | _::s::_ -> Some s 
    | _ -> None

let readFile fileName =
    try
        File.ReadLines fileName
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

type Instruction =
    | Adv of int*int
    | Bxl of int*int
    | Bst of int
    | Jnz of int option
    | Bxc
    | Out of int
    | Bdv of int*int
    | Cdv of int*int

type State = {
    RegisterA:int
    RegisterB:int
    RegisterC:int
    Pointer: int
    Output: int List
}
type Program = int List

let combo state = function
| 0 -> 0
| 1 -> 1
| 2 -> 2
| 3 -> 3
| 4 -> state.RegisterA
| 5 -> state.RegisterB
| 6 -> state.RegisterC
| _ -> failwith "illegal combo"

let readInstruction program state =
    if state.Pointer > List.length program - 2 then None
    else 
        let operand = program[state.Pointer + 1]
        match program[state.Pointer] with
        | 0 -> Some <| Adv (state.RegisterA, pown 2 (combo state operand))
        | 1 -> Some <| Bxl (state.RegisterB, operand)
        | 2 -> Some <| Bst (combo state operand)
        | 3 -> Some <| Jnz (if state.RegisterA = 0 then None else Some operand)
        | 4 -> Some <| Bxc
        | 5 -> Some <| Out (combo state operand)
        | 6 -> Some <| Bdv (state.RegisterA, pown 2 (combo state operand))
        | 7 -> Some <| Cdv (state.RegisterA, pown 2 (combo state operand))
        | _ -> None

let program (input: string seq): Program =
    input |> List.ofSeq
    |> fun l ->
        l[4] |> String.replace "Program: " "" |> String.split [","] |>> int |> List.ofSeq

let startState (input: string seq): State =
    input |> List.ofSeq
    |> fun l ->
        {
            RegisterA = l[0] |> String.replace "Register A: " "" |> int
            RegisterB = l[1] |> String.replace "Register B: " "" |> int
            RegisterC = l[2] |> String.replace "Register C: " "" |> int
            Pointer = 0
            Output = []
        }

let performInstruction instruction state =
    instruction |> function
    | Some (Adv (a,b)) -> {state with RegisterA = a/b ; Pointer = state.Pointer + 2}
    | Some (Bxl (a,b)) -> {state with RegisterB = a ^^^ b ; Pointer = state.Pointer + 2}
    | Some (Bst a) -> {state with RegisterB = a % 8 ; Pointer = state.Pointer + 2}
    | Some (Jnz a) -> {state with Pointer = a |> Option.defaultValue (state.Pointer + 2) }
    | Some Bxc  -> {state with RegisterB = state.RegisterB ^^^ state.RegisterC ; Pointer = state.Pointer + 2}
    | Some (Out a) -> {state with Output = state.Output @ [(a%8)]; Pointer = state.Pointer + 2}
    | Some (Bdv (a,b)) -> {state with RegisterB = a/b ; Pointer = state.Pointer + 2}
    | Some (Cdv (a,b)) -> {state with RegisterC = a/b ; Pointer = state.Pointer + 2}
    | None -> state


let runProgram program state =
    let mutable state = state
    let mutable instruction = readInstruction program state
    while not (instruction = None) do
        state <- performInstruction instruction state
        instruction <- readInstruction program state
    state

let runProgramWithCheck program state =
    let mutable state = state
    let mutable instruction = readInstruction program state
    while not (instruction = None) do
        state <- performInstruction instruction state
        instruction <- 
            match instruction with
            | Some (Out _) ->
                let pairs = state.Output |> List.zipShortest program
                if pairs |> List.forall (fun (a, b) -> a = b) 
                then readInstruction program state
                else None
            | _ -> readInstruction program state
    state

let part1 input =
    runProgram (program input) (startState input)
    |> fun s -> s.Output
    |>> sprintf "%i"
    |> String.concat ","

let part2 input = 
    let program = program input
    let mutable run = 0
    let mutable state = startState input
    let regA run = run //5 + 8 * run
    while not (state.Output = program) do
        state <- {startState input with RegisterA = regA run}
        state <- runProgramWithCheck program state
        if state.Output.Length > 3 && state.Output[0..3] = program[0..3] then Console.WriteLine $"regA: {regA run} - {state.Output}"
        run <- run + 1
    regA (run - 1) |> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () -> 
                runProgram 
                    [0;1;5;4;3;0]
                    {
                        RegisterA = 729
                        RegisterB = 0
                        RegisterC = 0
                        Pointer = 0
                        Output = []
                    }
                |> fun s -> s.Output
                |> function | [4;6;3;5;6;3;5;2;1;0] -> Ok () | other -> Error $"Part 1 wrong: {other}"
            fun () -> 
                runProgram 
                    [0;3;5;4;3;0]
                    {
                        RegisterA = 117440
                        RegisterB = 0
                        RegisterC = 0
                        Pointer = 0
                        Output = []
                    }
                |> fun s -> s.Output
                |> function | [0;3;5;4;3;0] -> Ok () | other -> Error $"expected copy, got {other}"
        ]
    let run () =
        tests 
        |> List.fold (fun state test -> state |> Result.bind test ) (Ok ())
        |>> fun () -> "All tests Ok"

let input = fileName |>> readFile
match input with
| Some input ->
    Result.map3
        (sprintf "Successful run!\r\nTests: %s\r\nPart1: %s\r\nPart2: %s")
        (Tests.run ())
        (input |>> part1)
        (input |>> part2)
| None ->
    Tests.run () |>> sprintf "Tests only:\r\n %s"
|> function
| Ok s -> s
| Error s -> sprintf "Error: %s" s 
|> Console.WriteLine 
