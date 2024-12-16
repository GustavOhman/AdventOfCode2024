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

let up c = {c with Y = c.Y - 1}
let down c = {c with Y = c.Y + 1}
let left c = {c with X = c.X - 1}
let right c = {c with X = c.X + 1}

type Direction = 
| Up
| Down
| Left
| Right

let step direction coord =
    match direction with
    | Up -> up coord
    | Down -> down coord
    | Left -> left coord
    | Right -> right coord

let turnCost d1 d2 =
    match (d1, d2) with
    | Up, Down
    | Down, Up
    | Right, Left
    | Left, Right -> 2000
    | a, b when a = b -> 0
    | _ -> 1000

type Input = {
    Start: Coord
    Finish: Coord
    Tiles: Coord Set
}

let parseInput (input: string seq): Input =
    let tiles, start, finish =
            input
            |> Seq.indexed
            |> Seq.fold 
                (fun (tiles, start, finish) (y,row) ->
                    row |> Seq.indexed
                    |> Seq.fold
                        (fun (tiles, start, finish) (x, c) ->
                            let coord = {X = x; Y = y}
                            match c with
                            | 'E' -> (Set.add coord tiles, start, Some coord)
                            | 'S' -> (Set.add coord tiles,Some coord, finish)
                            | '.' -> (Set.add coord tiles, start, finish)
                            | _ -> (tiles, start, finish)
                        )
                        (tiles, start, finish)
                )
                (Set.empty, None, None)
    {
        Start = start |> Option.defaultWith (fun () -> failwith "no start found")
        Finish = finish |> Option.defaultWith (fun () -> failwith "no finish found")
        Tiles = tiles
    }

let part1 input =
    let input = parseInput input
    Console.WriteLine $"{input.Tiles.Count} tiles"
    let mutable nodes =
        input.Tiles
        |> Seq.collect (fun c -> [Up;Down;Left;Right] |>> (fun direction -> (c, direction), Int32.MaxValue))
        |> Map.ofSeq
        |> Map.add (input.Start, Right) 0
    let mutable finishCost = []

    while (Map.values nodes |> Seq.exists (fun i -> i < Int32.MaxValue)) do
        let (node, direction), cost = nodes |> Map.toSeq |> Seq.minBy snd
        let steppos = ((node |> step direction), direction)
        if Map.containsKey steppos nodes && nodes[steppos] > cost + 1 then
            nodes <- nodes |> Map.add steppos (cost + 1)
        [Up;Down;Left;Right]
        |> Seq.where (fun dir -> not (dir = direction))
        |> Seq.iter (fun dir ->
            if Map.containsKey (node, dir) nodes && nodes[(node, dir)] > cost + turnCost direction dir then
                nodes <- nodes |> Map.add (node, dir) (cost + turnCost direction dir)
        )
        if (node = input.Finish) then
            finishCost <- cost :: finishCost
        nodes <- nodes |> Map.remove (node, direction)

    finishCost
    |> Seq.min
    |> sprintf "%i"

let part2 input = 
    "todo"
    //let input = parseInput input
    //let mutable nodes =
    //    input.Tiles
    //    |> Set.add input.Start
    //    |> Set.add input.Finish
    //    |> Seq.collect (fun c -> [Up;Down;Left;Right] |>> (fun direction -> (c, direction), Int32.MaxValue))
    //    |> Map.ofSeq
    //    |> Map.add (input.Start, Right) 0

    //let mutable paths: Map<(Coord*Direction),int*(Coord*Direction) Set> =
    //    nodes |> Map.mapValues (fun i -> i, Set.empty)

    //while (Map.values nodes |> Seq.exists (fun i -> i < Int32.MaxValue)) do
    //    let (node, direction), cost = nodes |> Map.toSeq |> Seq.minBy snd
    //    let steppos = ((node |> step direction), direction)
    //    if Map.containsKey steppos nodes && nodes[steppos] >= cost + 1 then
    //        if fst paths[steppos] > cost + 1
    //        then paths <- paths |> Map.add (steppos) ((cost + 1), (Set.ofList [node, direction]))
    //        elif fst paths[steppos] = cost + 1
    //        then paths <- paths |> Map.add (steppos) ((cost + 1), (snd paths[steppos]) |> Set.add (node, direction))

    //        nodes <- nodes |> Map.add steppos (cost + 1)
    //    [Up;Down;Left;Right]
    //    |> Seq.where (fun dir -> not (dir = direction))
    //    |> Seq.iter (fun dir ->
    //        let turnPos = node, dir
    //        let newCost = cost + turnCost direction dir
    //        if Map.containsKey (turnPos) nodes && nodes[turnPos] >= newCost then
    //            if fst paths[turnPos] > newCost
    //            then paths <- paths |> Map.add (turnPos) (newCost, (Set.ofList [turnPos]))
    //            elif fst paths[turnPos] = newCost
    //            then paths <- paths |> Map.add (turnPos) (newCost, (snd paths[turnPos]) |> Set.add (turnPos))

    //            nodes <- nodes |> Map.add (node, dir) (cost + turnCost direction dir)
    //    )
    //    nodes <- nodes |> Map.remove (node, direction)

    //let rec trackbackPaths state node =
    //    //Console.WriteLine $"{coord.X}, {coord.Y} State {Set.count state}"
    //    //Console.Write "paths are ["
    //    //paths[coord]
    //    //|> snd
    //    //|> Seq.iter (fun c -> Console.Write $"({c.X}, {c.Y}) ")
    //    //Console.WriteLine "]"
    //    if node = (input.Start, Right)
    //    then state |> Set.add node
    //    else
    //        let ahead = 
    //            (snd paths[node])
    //            //|> fun x -> 
    //            //    Console.Write "ahead is ["
    //            //    x
    //            //    |> Seq.iter (fun c -> Console.Write $"({c.X}, {c.Y}) ")
    //            //    Console.WriteLine "]"
    //            //    x
    //            |> Seq.map (trackbackPaths (Set.add node state)) |> Set.unionMany
    //        state |> Set.add node |> Set.union ahead
    
    //let minscore = paths |> Map.filter (fun (c,_) _ -> c = input.Finish) |> Map.values |> Seq.minBy fst |> fst
    //let endPos =
    //    paths |> Map.filter (fun _ (cost,_) -> cost = minscore) |> Map.keys

    //endPos
    //|>> fun p -> trackbackPaths Set.empty p
    //|>> Set.map fst
    //|> Set.unionMany
    //|> Set.count
    //|> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () -> ["#####";"#..E#";"#S.##";"#####"] |> part2 |> function | "4" -> Ok () | other -> Error other
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
