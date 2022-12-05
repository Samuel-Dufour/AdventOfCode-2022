open System.IO
open System
open System.Collections.Generic

let input = File.ReadLines($"{__SOURCE_DIRECTORY__}/input.txt")

type Action =
    { number: int
      origin: string
      dest: string }

let stackLines = input |> Seq.takeWhile (fun l -> l <> "")
let length = Seq.length stackLines

let keys =
    stackLines
    |> Seq.last
    |> (fun l -> l.Split(" ", StringSplitOptions.RemoveEmptyEntries))

let mapper l =
    [ for idx in 1..4 .. String.length l do
          l[idx] ]

let stacksContent =
    stackLines
    |> Seq.take (length - 1)
    |> Seq.map mapper
    |> Array.ofSeq

let rStacks =
    [ for idx in 0 .. keys.Length - 1 do
          keys[idx],
          stacksContent
          |> Seq.map (fun a -> a[idx])
          |> Seq.filter ((<>) ' ')
          |> Seq.rev
          |> Stack ]
    |> dict

let actions =
    input
    |> Seq.skip (length + 1)
    |> Seq.map (fun l -> l.Split([| "move"; "from"; "to"; " " |], StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun parts ->
        { number = int parts[0]
          origin = parts[1]
          dest = parts[2] })
    |> List.ofSeq

let doActions actions (rStacks: IDictionary<string, Stack<char>>) =
    let rec loop actions =
        match actions with
        | [] -> rStacks
        | { number = number
            origin = origin
            dest = dest } :: tail ->
            [ for _ in 1..number do
                  let extracted = rStacks[ origin ].Pop()
                  rStacks[ dest ].Push(extracted) |> ignore ]
            |> ignore

            loop tail

    loop actions

let re = doActions actions rStacks

[| for k in re.Keys do
       re[ k ].Pop() |]
|> System.String
