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

let parseInput (input: string seq) =
    input
    |>> String.split [" "]
    |>> Seq.map int

type ReportType =
    | SafeIncrease
    | SafeDecrease
    | Unsafe

let evalReport (levels: int seq): ReportType =
    let increases =
        levels
        |> Seq.pairwise
        |>> fun (a, b) -> b - a
    
    if (Seq.forall (fun item -> item > 0 && item <= 3) increases) then SafeIncrease
    elif (Seq.forall (fun item -> item < 0 && item >= -3) increases) then SafeDecrease
    else Unsafe

let evalReport2 (levels: int seq): ReportType =
    match evalReport levels with
        | SafeIncrease -> SafeIncrease
        | SafeDecrease -> SafeDecrease
        | Unsafe ->
            seq {0..((Seq.length levels) - 1)}
            |>> fun place -> Seq.removeAt place levels
            |>> evalReport
            |> fun dampened ->
                if Seq.exists (fun x -> x = SafeIncrease) dampened then SafeIncrease
                elif Seq.exists (fun x -> x = SafeDecrease) dampened then SafeDecrease
                else Unsafe

let part1 input =
    input
    |> parseInput
    |>> evalReport
    |>> (function | SafeIncrease -> 1 | SafeDecrease -> 1 | Unsafe -> 0)
    |> Seq.sum
    |> sprintf "%i"

let part2 input = 
    input
    |> parseInput
    |>> evalReport2
    |>> (function | SafeIncrease -> 1 | SafeDecrease -> 1 | Unsafe -> 0)
    |> Seq.sum
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            //fun () -> Error "todo"
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
