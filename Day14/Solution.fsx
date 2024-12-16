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

type Coord = {
    X:int
    Y:int
}

let move dimensions v c = 
    {
        X = (c.X + v.X + dimensions.X) % dimensions.X
        Y = (c.Y + v.Y + dimensions.Y) % dimensions.Y
    }

type Input = {
    Pos : Coord
    Vel: Coord
}

let parseInput (input: string seq): Input seq=
    input
    |>> fun s ->  
        Regex.Match(s, "p=([0-9]+),([0-9]+) v=(\-?[0-9]+),(\-?[0-9]+)")
        |> fun m ->
            {
                Pos = {X = (int m.Groups[1].Value); Y = (int m.Groups[2].Value)}
                Vel = {X = (int m.Groups[3].Value); Y = (int m.Groups[4].Value)}
            }

let part1 input =
    let dimensions = {X = 101; Y = 103}
    input |> parseInput
    |>> fun robot ->
        seq{1..100}
        |> Seq.fold 
            (fun pos _i ->
                pos |> move dimensions robot.Vel )
            robot.Pos
    |> fun positions ->
        [
            positions |> Seq.where (fun pos -> pos.X < dimensions.X / 2 && pos.Y < dimensions.Y / 2)
            positions |> Seq.where (fun pos -> pos.X > dimensions.X / 2 && pos.Y < dimensions.Y / 2)
            positions |> Seq.where (fun pos -> pos.X < dimensions.X / 2 && pos.Y > dimensions.Y / 2)
            positions |> Seq.where (fun pos -> pos.X > dimensions.X / 2 && pos.Y > dimensions.Y / 2)
        ]
        |>> Seq.length
    |> List.fold (*) 1
    |> sprintf "%i"

let part2 input = 
    "todo"

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
