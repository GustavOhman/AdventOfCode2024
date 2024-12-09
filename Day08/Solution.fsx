#r "nuget: FSharpPlus"
//Using FSharpPlus for map (|>>) and bind (>>=) operators

open System
open System.IO
open Microsoft.FSharp.Collections
open FSharpPlus
open System.Collections.Generic

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

type Coord = {X:int;Y:int}

type Input = {
    MaxX: int
    MaxY: int
    Antennas: Map<char, Coord list>
}

let parseInput (input: string seq): Input =
    let antennas = new Dictionary<char, Coord list>()
    array2D input
    |> Array2D.iteri ( fun x y c ->
        match c with
        |  '.' -> ()
        | freq ->
            if antennas.ContainsKey freq then
                antennas[freq] <- {X=x;Y=y} :: antennas[freq]
            else
                antennas.Add(freq, [{X=x;Y=y}])
    )

    {
        MaxY = (Seq.head input |> String.length) - 1
        MaxX = (Seq.length input) - 1
        Antennas = 
            antennas :> seq<_>
            |> Seq.map (|KeyValue|)
            |> Map.ofSeq
    }

let pairs list =
    list
    |> List.fold
            (fun state item ->
                state @
                    (list 
                    |>> fun x -> x, item)
            )
            []

let part1 input =
    let map = parseInput input
    map.Antennas
    |> Map.values
    |> Seq.where (fun l -> l.Length > 1)
    |>>  (fun coords ->
        pairs coords
        |> List.where (fun (a, b) -> a <> b)
        |>> fun (c1,c2) -> 
            [
                {
                    X = c1.X + 2 * (c2.X - c1.X )
                    Y = c1.Y + 2 * (c2.Y - c1.Y )
                }
                {
                    X = c2.X + 2 * (c1.X - c2.X )
                    Y = c2.Y + 2 * (c1.Y - c2.Y )
                }
            ]
        |> List.concat
    )
    |>> List.where (fun c -> c.X >= 0 && c.Y >= 0 && c.X <= map.MaxX && c.Y <= map.MaxY)
    |> Seq.concat
    |> Set.ofSeq
    |> Seq.length
    |> sprintf "%i"

let part2 input = 
    let map = parseInput input
    map.Antennas
    |> Map.values
    |> Seq.where (fun l -> l.Length > 1)
    |>>  (fun coords ->
        pairs coords
        |> List.where (fun (a, b) -> a <> b)
        |>> fun (c1,c2) -> 
            //ugly brute force
            seq{-50..50}
            |> List.ofSeq
            |>> fun i ->
                {
                    X = c1.X + i * (c2.X - c1.X )
                    Y = c1.Y + i * (c2.Y - c1.Y )
                }
            |> fun x -> x
            |> List.where (fun c -> c.X >= 0 && c.Y >= 0 && c.X <= map.MaxX && c.Y <= map.MaxY)
        |> List.concat
    )
    |> Seq.concat
    |> Set.ofSeq
    |> Seq.length
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
