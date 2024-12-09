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

type Input = (int option) List

type Input2 = {
    FileId: int
    FileSize: int
    Free: int
}

let parseInput (input: string seq): Input =
    input
    |> Seq.head
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.fold
        (fun (state, fileId, isFile) c ->
            if isFile then
                (seq{1..c}
                |>> fun _ -> Some fileId
                |> List.ofSeq ) @ state, fileId + 1 , false
            else
                (seq{1..c}
                |>> fun _ -> None
                |> List.ofSeq ) @ state, fileId , true

        )
        ([], 0 ,true)
    |> fun (map, _, _) -> List.rev map

let parseInput2 (input: string seq): Input2 list =
    input
    |> Seq.head
    |> Seq.map (fun c -> int c - int '0')
    |> Seq.chunkBySize 2
    |> Seq.map (fun arr -> if arr.Length = 1 then (arr[0], 0) else (arr[0], arr[1]))
    |> Seq.fold
        (fun (state, fileId) (filesize, freespace) ->
            {FileId = fileId;FileSize=filesize;Free=freespace} :: state, fileId + 1
        )
        ([], 0)
    |> fun (map, _) -> List.rev map


let printmap diskmap = 
    diskmap
    |> Array.map (function | Some i ->  char (i + (int '0')) | None -> '.')
    |> String.ofArray
    |> fun s -> Console.WriteLine s

let part1 input =
    let diskmap = input |> parseInput |> Array.ofList
    seq{0..(diskmap.Length - 1)}
    |> Seq.rev
    |> Seq.iter
        (fun i ->
            match diskmap[i] with
            | Some file ->
                let freepos = 
                    diskmap
                    |> Seq.findIndex (fun slot -> slot = None)
                if freepos < i then
                    diskmap[freepos] <- Some file
                    diskmap[i] <- None
            | None -> ()
        )
    diskmap
    |> Seq.choose id
    |>> int64
    |> Seq.mapi (fun pos file -> (int64)pos * file)
    |> Seq.sum
    |> sprintf "%i"

let part2 input = 
    let diskmap = input |> parseInput2 |> Array.ofList

    //diskmap
    //|> Seq.iter
    //    (fun item -> Console.WriteLine $"{item.FileId} {item.FileSize} {item.Free}")

    seq{0..(diskmap.Length - 1)}
    |> Seq.rev
    |> Seq.iter
        (fun i ->
            //diskmap
            //|> Seq.iter
            //    (fun item -> 
            //        seq{0..item.FileSize - 1}
            //        |> Seq.iter (fun _ -> Console.Write $"{item.FileId}")
            //        if(item.Free > 0) then
            //            seq{1..item.Free}
            //            |> Seq.iter (fun _ -> Console.Write ".")
            //    )
            //Console.WriteLine ()

            let currentPos = diskmap |> Seq.findIndex (fun item -> item.FileId = i)

            let freepos = 
                diskmap
                |> Seq.tryFindIndex (fun file -> file.Free >= diskmap[currentPos].FileSize)
            match freepos with
                | Some pos when pos < currentPos ->
                    diskmap[currentPos - 1] <- 
                        {diskmap[currentPos - 1] 
                            with Free = diskmap[currentPos - 1].Free + diskmap[currentPos].FileSize + diskmap[currentPos].Free
                        }
                    let movedFile = {diskmap[currentPos] with Free = diskmap[pos].Free - diskmap[currentPos].FileSize}
                    diskmap[pos] <- {diskmap[pos] with Free = 0}
                    //shuffle right
                    if pos < currentPos - 1 then
                        seq{(pos + 2)..currentPos}
                        |> Seq.rev
                        |> Seq.iter (fun newpos -> diskmap[newpos] <- diskmap[newpos - 1])
                    diskmap[pos + 1] <- movedFile
                | _ -> ()
        )

    //diskmap
    //|> Seq.iter
    //    (fun item -> Console.WriteLine $" {item.FileId} {item.FileSize} {item.Free}")
    diskmap
    |> Seq.fold
        (fun (sum, startPos) input ->
            let checkSum = 
                seq{0..input.FileSize - 1}
                |>> int64
                |> Seq.sumBy (fun i -> (startPos + i) * (int64 input.FileId) )

            ((sum + checkSum) ,(startPos + (int64 input.Free) + (int64 input.FileSize)))
        )
        (0L, 0L)
    |> fst
    |> sprintf "%i"

module Tests =
    let private tests = 
        [
            fun () -> 
                [|"123"|] |> parseInput |>> Option.defaultValue -1 
                |> function | [0;-1;-1;1;1;1;] -> Ok () | other -> Error $"{other}"
            fun () -> 
                [|"123"|] |> parseInput2
                |> function 
                    | [{FileId = 0;FileSize=1;Free=2};{FileId = 1;FileSize=3;Free=0}] -> Ok () 
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
