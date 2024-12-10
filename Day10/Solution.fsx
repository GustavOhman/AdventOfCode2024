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

type Coord = {
    X:int
    Y:int
}
let up c = {c with Y = c.Y + 1}
let down c = {c with Y = c.Y - 1}
let left c = {c with X = c.X - 1}
let right c = {c with X = c.X + 1}

type Input = int array2d

let parseInput (input: string seq): Input =
    input
    |>> Seq.map (fun c -> int c - int '0')
    |> array2D

let part1 input =
    let map = parseInput input
    let height c =
        if c.X >= 0 && c.X < Array2D.length2 map && c.Y >= 0 && c.Y < Array2D.length1 map then
            Some map[c.Y,c.X]
        else
            None
    let trailheads =
        map
        |> Array2D.mapi (fun y x i -> match i with | 0 -> Some {X = x; Y = y } | _ -> None )
        |> fun arr -> seq{ for x in arr do yield (x :?> Coord option)}
        |> Seq.choose id

    let tryWalk coord =
        let nextHeight = (height coord |> Option.defaultValue -10) + 1

        [
            coord |> up    |> (fun x  -> if height x = Some nextHeight then Some x else None)
            coord |> down  |> (fun x  -> if height x = Some nextHeight then Some x else None)
            coord |> left  |> (fun x  -> if height x = Some nextHeight then Some x else None)
            coord |> right |> (fun x  -> if height x = Some nextHeight then Some x else None)
        ]
        |> List.choose id

    trailheads
    |>> (
        fun head ->
        seq{1..9}
        |> Seq.fold
            (fun state i ->
                state |>> tryWalk |> List.concat |> List.distinct
            )
            [head]
        )
    |> Seq.sumBy Seq.length
    |> sprintf "%i"

let part2 input = 
    let map = parseInput input
    let height c =
        if c.X >= 0 && c.X < Array2D.length2 map && c.Y >= 0 && c.Y < Array2D.length1 map then
            Some map[c.Y,c.X]
        else
            None
    let trailheads =
        map
        |> Array2D.mapi (fun y x i -> match i with | 0 -> Some {X = x; Y = y } | _ -> None )
        |> fun arr -> seq{ for x in arr do yield (x :?> Coord option)}
        |> Seq.choose id

    let tryWalk coord =
        let nextHeight = (height coord |> Option.defaultValue -10) + 1
        [
            coord |> up    |> (fun x  -> if height x = Some nextHeight then Some x else None)
            coord |> down  |> (fun x  -> if height x = Some nextHeight then Some x else None)
            coord |> left  |> (fun x  -> if height x = Some nextHeight then Some x else None)
            coord |> right |> (fun x  -> if height x = Some nextHeight then Some x else None)
        ]
        |> List.choose id

    trailheads
    |>> (
        fun head ->
        seq{1..9}
        |> Seq.fold
            (fun (state: Coord list list) i ->
                state 
                |>> fun path ->
                    path
                    |> List.head
                    |> tryWalk
                    |>> (fun next -> next :: path)
                |> List.concat |> List.distinct
            )
            [[head]]
        )
    |> Seq.sumBy Seq.length
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
