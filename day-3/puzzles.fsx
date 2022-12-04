open System.IO
let input = File.ReadLines($"{__SOURCE_DIRECTORY__}/input-test.txt")

let toCompartments l =
    let middleIndex = String.length l / 2
    l[0 .. middleIndex - 1], l[middleIndex..]

let findItemsInBoth f s =
    query {
        for fst in Seq.distinct f do
            join snd in Seq.distinct s on (fst = snd)
            select fst
    }

let findItemInBoth (f, s) = findItemsInBoth f s |> Seq.head

let findItemInAll all =
    let rec loop list =
        match list with
        | fst :: snd :: tail ->
            let r = findItemsInBoth fst snd
            loop (r :: tail)
        | _ -> list |> Seq.head

    Array.map (Seq.map id) all
    |> List.ofArray
    |> loop
    |> Seq.head

let priorities =
    seq {
        for l in 'a' .. 'z' -> (l, int l - int 'a' + 1)
        for l in 'A' .. 'Z' -> (l, int l - int 'A' + 27)
    }
    |> dict

let toPriority c = priorities[c]

let firstPuzzle =
    let mapper = toCompartments >> findItemInBoth >> toPriority

    input |> Seq.map mapper |> Seq.sum

let secondPuzzle =
    let mapper = findItemInAll >> toPriority

    input
    |> Seq.chunkBySize 3
    |> Seq.map mapper
    |> Seq.sum
