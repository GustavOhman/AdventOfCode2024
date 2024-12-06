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
    x:int
    y:int
}
type Direction = 
    | Up
    | Right
    | Down
    | Left

type Input = {
    Obstacles: Coord Set
    GuardPosition: Coord
    GuardDirection: Direction
    MaxX:int
    MaxY:int
}

let parseInput (input: string seq): Input =
    let mutable (obstacles: Coord Set) = Set.empty
    let mutable (guard: (Coord*Direction) option) = None
    array2D input
    |> Array2D.iteri (fun y x c ->
        match c with
        | '#' -> 
            obstacles <- obstacles.Add {x= x; y=y}
        | '^' -> 
            guard <- Some ({x= x; y=y}, Up)
        | '>' -> 
            guard <- Some ({x= x; y=y}, Right)
        | '<' -> 
            guard <- Some ({x= x; y=y}, Left)
        | 'v' -> 
            guard <- Some ({x= x; y=y}, Down)
        | _ -> ()
        )
    match guard with
    | Some (pos,dir) ->
        {
            Obstacles = obstacles
            GuardPosition = pos
            GuardDirection = dir
            MaxX = (input |> Seq.head |> String.length) - 1
            MaxY = (input |> Seq.length) - 1
        }
    | None -> failwith "No guard found"

let path (map:Input): (Coord Set*bool) =
    let mutable (path:(Coord*Direction) Set) = Set.empty
    let mutable (pos, dir):(Coord*Direction) = (map.GuardPosition, map.GuardDirection)
    let mutable loop = false
    while 
        pos.x >= 0 
        && pos.y >= 0 
        && pos.x <= map.MaxX 
        && pos.y <= map.MaxY 
        && not loop
        do
            if path |> Set.contains (pos, dir) then loop <- true
            else
                path <- path.Add (pos, dir)
                match dir with
                | Up ->
                    if (map.Obstacles.Contains {x=pos.x; y = pos.y - 1} ) then
                        dir <- Right
                    else
                        pos <- {pos with y = pos.y - 1}
                | Down ->
                    if (map.Obstacles.Contains {x=pos.x; y = pos.y + 1} ) then
                        dir <- Left
                    else
                        pos <- {pos with y = pos.y + 1}
                | Right ->
                    if (map.Obstacles.Contains {x=pos.x + 1; y = pos.y} ) then
                        dir <- Down
                    else
                        pos <- {pos with x = pos.x + 1}
                | Left ->
                    if (map.Obstacles.Contains {x=pos.x - 1; y = pos.y} ) then
                        dir <- Up
                    else
                        pos <- {pos with x = pos.x - 1}
    path |> Set.map fst, loop

let part1 input =
    input
    |> parseInput
    |> path
    |> fst
    |> Set.count
    |> sprintf "%i"

let part2 input =
    let parsedInput = parseInput input
    let potentialCoords =
        parsedInput
        |> path
        |> fst
        |> Set.remove parsedInput.GuardPosition
        |> Set.toSeq

    potentialCoords
    |>> fun c ->
        path {parsedInput with Obstacles = parsedInput.Obstacles |> Set.add c}
    |> Seq.where snd
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
