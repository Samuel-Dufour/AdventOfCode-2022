open System.IO
let lines = File.ReadLines($"{__SOURCE_DIRECTORY__}/input-test.txt")
let mapper seqSize line  =
    line
    |> Seq.windowed seqSize
    |> Seq.indexed
    |> Seq.map (fun (idx, vals) ->
        idx+seqSize, vals|> Seq.distinct|> Seq.length = seqSize
    )
    |> Seq.find (fun (_, v) -> v)
    |> fst

let firstPuzzle =
    lines
    |> Seq.map (mapper 4)
    |> List.ofSeq

let secondPuzzle =
    lines
    |> Seq.map (mapper 14)
    |> List.ofSeq
