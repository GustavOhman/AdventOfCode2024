#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus
open System.Text.RegularExpressions

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

let readInput (input: string seq) =
    input
    |>> fun s -> 
        s |> String.split ["   "]
        |> Seq.map int
        |> fun pair -> (Seq.item 0 pair , Seq.item 1 pair)

let part1 (input: string seq) =
    let left = 
        input
        |> readInput
        |>> fst
        |> List.ofSeq
        |> List.sort

    let right = 
        input
        |> readInput
        |>> snd
        |> List.ofSeq
        |> List.sort

    left
    |> List.mapi (fun i x -> x - right[i])
    |>> fun (x:int) -> Math.Abs x
    |> List.sum
    |> sprintf "%i"

let part2 (input: string seq) = 
    let left = 
        input
        |> readInput
        |>> fst

    let right = 
        input
        |> readInput
        |>> snd

    left
    |> Seq.sumBy (fun l ->
        let matches =
            right
            |> Seq.where(fun r -> r = l)
            |> Seq.length
        l * matches
        )
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
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
