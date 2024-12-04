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

let xmasCount (s:string) =
    //Console.WriteLine $"checking {s}"
    seq{0..s.Length - 4}
    |>> fun i -> s.Substring(i, 4)
    |> Seq.where (function | "XMAS" -> true | "SAMX" -> true | _ -> false)
    |> Seq.length

let diagonalize (text: string seq): string seq =
    let textAsArray = array2D text
    let firstTriangle =
        seq{0..(Seq.length text) - 1}
        |>> fun row -> 
            seq{0..row}
            |> Seq.rev
            |>> fun i ->
                textAsArray[i, row - i]
        |> Seq.map String.ofSeq
    let secondTriangle =
        seq{1..(Seq.length text) - 1}
        |>> fun col -> 
            seq{0..(Seq.length text) - 1 - col}
            |>> fun i -> 
                textAsArray[(Seq.length text) - 1 - i, col + i]
        |> Seq.map String.ofSeq
    Seq.concat [firstTriangle; secondTriangle]

let part1 input =
    let horiz =
        input
        |>> xmasCount
        |> Seq.sum
    let vert =
        input
        |> Seq.transpose
        |>> String.ofSeq
        |> fun x -> x
        |>> xmasCount
        |> Seq.sum
    let diag1 =
        input
        |> diagonalize
        |>> String.ofSeq
        |> fun x -> x
        |>> xmasCount
        |> Seq.sum
    let diag2 =
        input
        |>> Seq.rev
        |>> String.ofSeq
        |> diagonalize
        |>> xmasCount
        |> Seq.sum

    Console.WriteLine $"horiz: {horiz} vert: {vert} diag1: {diag1} diag2: {diag2}"
    horiz + vert + diag1 + diag2
    |> sprintf "%i"

let part2 input =
    let arr = array2D input
    seq{1..(Seq.length input) - 2}
    |>> fun x ->
        seq{1..(Seq.length input) - 2}
        |> Seq.choose (fun y ->
            match arr[x,y] with
            | 'A' -> Some (arr[(x - 1),(y - 1)], arr[(x + 1),(y + 1)], arr[(x - 1),(y + 1)], arr[(x + 1),(y - 1)])
            | _ -> None)
    |> Seq.concat
    |> Seq.sumBy (function
            | ('M','S','M','S') -> 1
            | ('M','S','S','M') -> 1
            | ('S','M','S','M') -> 1
            | ('S','M','M','S') -> 1
            | _ -> 0
        )
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () ->
                ["ABC";"DEF";"JKL"]
                |> diagonalize
                |> List.ofSeq
                |> function
                    | ["A";"DB";"JEC";"KF";"L"] -> Ok ()
                    | other -> Error $"{other}"
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
