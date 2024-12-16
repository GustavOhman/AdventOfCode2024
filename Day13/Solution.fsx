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

type Input = {
    ButtonA: Coord
    ButtonB: Coord
    Prize: Coord
}

let parseInput (input: string seq): Input seq =
    input
    |> Seq.split [[""]]
    |>> (List.ofSeq 
     >> fun machine ->
        let toCoord regex s =
            Regex.Match(s, regex)
            |> fun m ->
                {X = (int m.Groups[1].Value); Y = (int m.Groups[2].Value)}
        {
            ButtonA = toCoord "Button A: X\+([0-9]+), Y\+([0-9]+)" machine[0]
            ButtonB = toCoord "Button B: X\+([0-9]+), Y\+([0-9]+)" machine[1]
            Prize = toCoord "Prize: X=([0-9]+), Y=([0-9]+)" machine[2]
        }
    )


let part1 input =
    let solutions (machine:Input) : int seq =
        let xSolutions =
            seq{0..(Math.Min(100,machine.Prize.X / machine.ButtonA.X ))}
            |> Seq.choose (fun a -> 
                if (machine.Prize.X - a * machine.ButtonA.X) % machine.ButtonB.X = 0 
                then Some (a, (machine.Prize.X - a * machine.ButtonA.X) / machine.ButtonB.X) 
                else None 
            )
            |> Set.ofSeq
        let ySolutions =
            seq{0..(Math.Min(100,machine.Prize.Y / machine.ButtonA.Y ))}
            |> Seq.choose (fun a -> 
                if (machine.Prize.Y - a * machine.ButtonA.Y) % machine.ButtonB.Y = 0 
                then Some (a, (machine.Prize.Y - a * machine.ButtonA.Y) / machine.ButtonB.Y) 
                else None 
            )
            |> Set.ofSeq
        Set.intersect xSolutions ySolutions
        |> Seq.map (fun (a,b) -> 3 * a + b)
    input |> parseInput
    |>> solutions
    |>> (fun solutions -> if Seq.isEmpty solutions then 0 else Seq.min solutions )
    |> Seq.sum
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
