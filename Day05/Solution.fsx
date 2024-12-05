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

type Input = {
    Rules : (int * int) list
    Pages : int list list
}

let readFile fileName =
    try
        File.ReadLines fileName
        |> Ok
    with
        ex -> Error $"Could not read file '%s{fileName}': %s{ex.Message}" 

let parseInput (input:string seq) : Input =
    input
    |> Seq.split [[""]]
    |> fun splitted ->
        {
            Rules =
                splitted
                |> Seq.item 0
                |>> ( String.split ["|"] >> fun pair -> (Seq.item 0 pair |> int, Seq.item 1 pair |> int))
                |> List.ofSeq
            Pages =
                splitted
                |> Seq.item 1
                |> List.ofSeq
                |>> (String.split [","])
                |>> Seq.map int
                |>> List.ofSeq
        }

let comparer (rules:(int*int) list) a b : int =
    if List.exists (fun item -> item = (a,b)) rules then -1
    elif List.exists (fun item -> item = (b,a)) rules then 1
    else 0

let middleNumber (pages: int list) =
    pages[pages.Length / 2]

let part1 input =
    let parsedInput = parseInput input
    parsedInput.Pages
    |> List.where (fun l -> List.sortWith (comparer parsedInput.Rules) l = l)
    |>> middleNumber
    |> List.sum
    |> sprintf "%i"

let part2 input = 
    let parsedInput = parseInput input
    parsedInput.Pages
    |> List.choose (fun l ->
        List.sortWith (comparer parsedInput.Rules) l
        |> fun sorted -> if sorted = l then None else Some sorted
    )
    |>> middleNumber
    |> List.sum
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
