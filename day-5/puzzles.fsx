open System.IO
open System
open System.Collections.Generic

let lines = File.ReadLines($"{__SOURCE_DIRECTORY__}/input-test.txt")

type Action =
    { number: int
      origin: string
      dest: string }

let emptyLineIndex =
    lines
    |> Seq.indexed
    |> Seq.filter (fun (_, l) -> l = String.Empty)
    |> Seq.map fst
    |> Seq.head

let stackLinesCount = emptyLineIndex
let firstActionIndex = emptyLineIndex + 1

let extractStacks lines =
    let stackLines = lines |> Seq.takeWhile ((<>) "")

    let keys =
        stackLines
        |> Seq.last
        |> (fun l -> l.Split(" ", StringSplitOptions.RemoveEmptyEntries))

    let stacksContent =
        let mapper l =
            [ for idx in 1..4 .. String.length l do
                  l[idx] ]

        stackLines
        |> Seq.take stackLinesCount
        |> Seq.map mapper

    [ for idx in 0 .. keys.Length - 1 do
          keys[idx],
          stacksContent
          |> Seq.map (fun a -> a[idx])
          |> Seq.filter ((<>) ' ')
          |> Seq.rev
          |> Stack ]
    |> dict

let extractActions lines =
    let mapper (l: string) =
        l.Split([| "move"; "from"; "to"; " " |], StringSplitOptions.RemoveEmptyEntries)
        |> (fun parts ->
            { number = int parts[0]
              origin = parts[1]
              dest = parts[2] })

    lines
    |> Seq.skip firstActionIndex
    |> Seq.map mapper
    |> List.ofSeq

let doActions lines revertOrder =
    let stacks = extractStacks lines

    let rec loop actions =
        match actions with
        | [] -> stacks
        | { number = number
            origin = origin
            dest = dest } :: tail ->
            let extracted =
                [ for _ in 1..number do
                      stacks[ origin ].Pop() ]

            if revertOrder then
                List.rev extracted
            else
                extracted
            |> List.iter (fun ex -> stacks[ dest ].Push(ex))

            loop tail

    let finalState = extractActions lines |> loop

    [| for k in finalState.Keys do
           finalState[ k ].Pop() |]
    |> String

let puzzle1 = doActions lines false
let puzzle2 = doActions lines true
