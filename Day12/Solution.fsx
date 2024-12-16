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

type Input = Coord Set list

let parseInput (input: string seq): Input =
    let map = array2D input
    let char c =
        if c.X >= 0 && c.X < Array2D.length2 map && c.Y >= 0 && c.Y < Array2D.length1 map then
            Some map[c.Y,c.X]
        else
            None
    input
    |> Seq.indexed
    |> Seq.fold
        (fun rowState (y,line) ->
            //Console.WriteLine ()
            line
            |> Seq.indexed
            |> Seq.fold
                (fun lineState (x,c) ->
                    let coord = {X=x;Y=y}
                    match 
                        [
                            if char (down coord) = Some c then down coord |> Some else None
                            if char (left coord) = Some c then left coord |> Some else None
                        ]
                    with
                    | [None;None] ->
                            ([coord] |> Set.ofList) :: lineState
                    | [Some above; None] ->
                            lineState
                            |>> (fun coords -> if Set.contains above coords then Set.add coord coords else coords)
                    | [None; Some toLeft] ->
                            lineState
                            |>> (fun coords -> if Set.contains toLeft coords then Set.add coord coords else coords)
                    | [Some above; Some toLeft] ->
                            let setAbove = lineState |> List.find (fun item -> Set.contains above item)
                            let setToLeft = lineState |> List.find (fun item -> Set.contains toLeft item)
                            lineState
                            |>> (fun coords ->
                                if coords = setAbove || coords = setToLeft then Set.union setAbove setToLeft |> Set.add coord
                                else coords)
                            |> List.distinct
                    | _ -> failwith "oops"
                )
                rowState
        )
        []


let part1 input =
    input
    |> parseInput
    |> List.sumBy (
        fun field ->
            (Set.count field) *
            (field
            |> Seq.sumBy (fun coord ->
                4 - (
                [
                    up coord
                    down coord
                    left coord
                    right coord
                ]
                |> List.where (fun c -> Set.contains c field)
                |> List.length)
            ))
            //|> fun x -> Console.WriteLine $"count {Set.count field} price {x}";x
    )
    |> sprintf "%i"
type FencedSide =
    | N
    | E
    | W
    | S

let part2 input = 
    input
    |> parseInput
    |> List.sumBy (
        fun field ->
            (Set.count field) *
            (field
            |> Seq.map (fun coord ->
                coord,
                Set.empty
                |> fun set -> if Set.contains (up coord) field then set else Set.add N set 
                |> fun set -> if Set.contains (down coord) field then set else Set.add S set 
                |> fun set -> if Set.contains (left coord) field then set else Set.add W set 
                |> fun set -> if Set.contains (right coord) field then set else Set.add E set 
            )
            |> Map.ofSeq
            |> fun x -> x
            //|> Seq.where(snd >> Set.isEmpty >> not)
            |> fun sections ->
                sections
                |> Map.map (fun coord sides ->
                    sides
                    |> fun x -> 
                        if Set.contains N x
                            && sections.ContainsKey (left coord) && sections[left coord].Contains N
                        then Set.remove N x
                        else x
                    |> fun x -> 
                        if Set.contains S x
                            && sections.ContainsKey (left coord) && sections[left coord].Contains S
                        then Set.remove S x
                        else x
                    |> fun x -> 
                        if Set.contains E x
                            && sections.ContainsKey (up coord) && sections[up coord].Contains E
                        then Set.remove E x
                        else x
                    |> fun x -> 
                        if Set.contains W x
                            && sections.ContainsKey (up coord) && sections[up coord].Contains W
                        then Set.remove W x
                        else x
                )
            |> Map.values
            |> Seq.concat
            |> fun x -> x
            |> Seq.length
            )
    )
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            //fun () -> ["AAAA";"BBCD";"BBCC";"EEEC"] |> parseInput |> fun x -> if x.Length = 5 then Ok () else Error $"wrong number of fields {x}"
            fun () -> 
                ["RRRRIICCFF";"RRRRIICCCF";"VVRRRCCFFF";"VVRCCCJFFF";"VVVVCJJCFE";"VVIVCCJJEE";"VVIIICJJEE";"MIIIIIJJEE";"MIIISIJEEE";"MMMISSJEEE"]
                |> parseInput |> fun x -> if x.Length = 11 then Ok () else Error $"wrong number of fields ({x.Length})"
            
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
