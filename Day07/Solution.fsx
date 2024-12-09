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


type Input = {
    Result: int64
    Parameters: int seq
}

let parseInput (input: string seq): Input seq =
    input
    |>> fun row ->
        row |> String.split [": ";" "]
        |> fun x -> {
            Result = Seq.head x |> int64
            Parameters = Seq.tail x |>> int
        }


let part1 input =
    let evalRow (row: Input) : bool =
        row.Parameters
        |> Seq.tail
        |> Seq.fold 
            (fun state item -> 
                state
                |> List.collect (fun value -> [value + (int64)item ; value * (int64)item ])
            ) 
            [(Seq.head row.Parameters |> int64)]
        |> List.exists (fun x -> x = row.Result)
    input
    |> parseInput
    |> Seq.where evalRow
    |> Seq.sumBy (fun row -> row.Result)
    |> sprintf "%i"

let part2 input = 
    let evalRow (row: Input) : bool =
        row.Parameters
        |> Seq.tail
        |> Seq.fold 
            (fun state item -> 
                state
                |> List.collect (fun value -> 
                    [
                        value + (int64)item
                        value * (int64)item
                        int64 $"{value}{item}"
                    ])
            ) 
            [(Seq.head row.Parameters |> int64)]
        |> List.exists (fun x -> x = row.Result)
    input
    |> parseInput
    |> Seq.where evalRow
    |> Seq.sumBy (fun row -> row.Result)
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
