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

type Stones = int64 seq

let parseInput (input: string seq): Stones =
    Seq.head input
    |> String.split [" "]
    |>> int64

let blink (stones:Stones) =
    // If the stone is engraved with the number 0, it is replaced by a stone engraved with the number 1.
    // If the stone is engraved with a number that has an even number of digits, it is replaced by two stones. 
    //    The left half of the digits are engraved on the new left stone, and the right half of the digits are engraved on the new right stone. (The new numbers don't keep extra leading zeroes: 1000 would become stones 10 and 0.)
    // If none of the other rules apply, the stone is replaced by a new stone; the old stone's number multiplied by 2024 is engraved on the new stone.
    stones
    |> Seq.map (fun stone ->
        if stone = 0L then [1L]
        else
            let stringstone = stone |> sprintf "%i"
            if stringstone.Length % 2 = 0 then
                [
                    String.take (stringstone.Length / 2) stringstone
                    String.skip (stringstone.Length / 2) stringstone
                ]
                |>> int64
            else [stone * 2024L])
    |> Seq.concat

let part1 input =
    let mutable stones = parseInput input
    seq{1..25}
    |> Seq.iter (fun _ -> stones<-blink stones)
    
    stones
    |> Seq.length
    |> sprintf "%i"

let part2 input = 
    let mutable stones = parseInput input |> Seq.map (fun x -> (x,1L)) |> Map.ofSeq
    seq{1..75}
    |> Seq.iter (fun i -> 
        Console.WriteLine $"{i}: {Seq.length stones}"
        let afterBlink = 
            stones
            |> Map.map (fun key item -> (item, blink [key]))
            |> Map.values
            |> Seq.fold
                (fun state (count, newStones) ->
                    newStones
                    |> Seq.fold
                        (fun acc stone ->
                            let prevCount = Map.tryFind stone acc |> Option.defaultValue 0L
                            acc |> Map.add stone (prevCount + count)
                        )
                        state
                )
                Map.empty
        stones<-afterBlink
    )

    stones.Values
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
