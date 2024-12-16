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

type Input = {
    Walls: Coord Set
    Boxes: Coord Set
    Empty: Coord Set
    Robot: Coord
    Moves: Direction seq
}

let parseInput (input: string seq): Input =
    let map, moves =
        input
        |> Seq.split [[""]]
        |> fun parts ->
            (Seq.head parts, Seq.tail parts |> Seq.concat |> Seq.concat)

    let walls, boxes, empty, robot =
            map
            |> Seq.indexed
            |> Seq.fold 
                (fun (w, b, e, r) (y,row) ->
                    row |> Seq.indexed
                    |> Seq.fold
                        (fun (w, b, e, r) (x, c) ->
                            let coord = {X = x; Y = y}
                            match c with
                            | '#' -> (Set.add coord w, b, e, r)
                            | 'O' -> (w, Set.add coord b, e, r)
                            | '@' -> (w, b, e, Some coord)
                            | '.' -> (w, b, Set.add coord e, r)
                            | _ -> failwith "unexpected tile"
                        )
                        (w, b, e, r)
                )
                (Set.empty, Set.empty, Set.empty, None)

    {
        Walls = walls
        Boxes = boxes
        Empty = empty
        Robot = robot |> Option.defaultWith (fun () -> failwith "no robot found")
        Moves = 
            moves 
            |>> function | '^' -> Up | 'v' -> Down | '<' -> Left | '>' -> Right | _ -> failwith "unknown direction"
    }

let part1 input =
    let input = parseInput input
    input.Moves
    |> Seq.fold
        (fun (boxes, empty, robot) dir ->
            //Console.WriteLine ()
            //Console.WriteLine $"moving {dir} from {robot.X}, {robot.Y}."
            //boxes
            //|> Seq.iter (fun box -> Console.Write $"({box.X}, {box.Y})")
            //Console.WriteLine ()
            match dir with
            | Up ->
                empty |> Seq.where (fun c -> c.X = robot.X && c.Y < robot.Y)
                |> Seq.sortByDescending (fun c -> c.Y)
                |> Seq.tryHead
                |> function
                    | Some emptySpace when input.Walls |> Seq.exists (fun c -> c.X = robot.X && c.Y < robot.Y && c.Y > emptySpace.Y) |> not -> 
                        (
                        boxes |> Set.map (fun c -> if c.X = robot.X && c.Y < robot.Y && c.Y > emptySpace.Y then up c else c) ,
                        (empty |> Set.remove emptySpace |> Set.add robot),
                        up robot
                        )
                    | _ -> (boxes, empty, robot)
            | Down ->
                empty |> Seq.where (fun c -> c.X = robot.X && c.Y > robot.Y)
                |> Seq.sortBy (fun c -> c.Y)
                |> Seq.tryHead
                |> function
                    | Some emptySpace when input.Walls |> Seq.exists (fun c -> c.X = robot.X && c.Y > robot.Y && c.Y < emptySpace.Y) |> not -> 
                        (
                        boxes |> Set.map (fun c -> if c.X = robot.X && c.Y > robot.Y && c.Y < emptySpace.Y then down c else c) ,
                        (empty |> Set.remove emptySpace |> Set.add robot),
                        down robot
                        )
                    | _ -> (boxes, empty, robot)
            | Left ->
                empty |> Seq.where (fun c -> c.Y = robot.Y && c.X < robot.X)
                |> Seq.sortByDescending (fun c -> c.X)
                |> Seq.tryHead
                |> function
                    | Some emptySpace when input.Walls |> Seq.exists (fun c -> c.Y = robot.Y && c.X < robot.X && c.X > emptySpace.X) |> not -> 
                        (
                        boxes |> Set.map (fun c -> if c.Y = robot.Y && c.X < robot.X && c.X > emptySpace.X then left c else c) ,
                        (empty |> Set.remove emptySpace |> Set.add robot),
                        left robot
                        )
                    | _ -> (boxes, empty, robot)
            | Right ->
                empty |> Seq.where (fun c -> c.Y = robot.Y && c.X > robot.X)
                |> Seq.sortBy (fun c -> c.X)
                |> Seq.tryHead
                |> function
                    | Some emptySpace when input.Walls |> Seq.exists (fun c -> c.Y = robot.Y && c.X > robot.X && c.X < emptySpace.X) |> not ->
                        (
                        boxes |> Set.map (fun c -> if c.Y = robot.Y && c.X > robot.X && c.X < emptySpace.X then right c else c) ,
                        (empty |> Set.remove emptySpace |> Set.add robot),
                        right robot
                        )
                    | _ -> (boxes, empty, robot)
        )
        (input.Boxes, input.Empty, input.Robot)
    |> fun (b, e, r) -> b
    |> Seq.sumBy (fun coord -> 100 * coord.Y + coord.X)
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
