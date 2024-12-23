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

let part1 input =
    input
    |>> fun s -> 
        Regex.Matches(s, "mul\(([0-9]+)\,([0-9]+)\)")
        |> Seq.cast
        |>> fun (m:Match) -> (int m.Groups[1].Value) * (int m.Groups[2].Value)
    |> Seq.concat
    |> Seq.sum
    |> sprintf  "%i"

let part2 input = 
    input
    |> Seq.fold (+) ""
    |> fun s -> 
        Regex.Replace(s, "don't\(\).+?(do\(\)|$)", "")
    |> fun s -> 
        Regex.Matches(s, "mul\(([0-9]+)\,([0-9]+)\)")
        |> Seq.cast
        |>> fun (m:Match) -> (int m.Groups[1].Value) * (int m.Groups[2].Value)
    |> Seq.sum
    |> sprintf  "%i"

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
