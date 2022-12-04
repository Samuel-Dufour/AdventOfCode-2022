open System.IO
let input = File.ReadLines($"{__SOURCE_DIRECTORY__}/input.txt")

let elfesCalories input =
    input
    |> Seq.fold
        (fun acc n ->
            match acc with
            | [] -> [ int n ]
            | head :: tail ->
                if n = "" then
                    0 :: acc
                else
                    head + int n :: tail)
        []

let puzzle1 = input |> elfesCalories |> Seq.max

let puzzle2 =
    input
    |> elfesCalories
    |> List.sortDescending
    |> Seq.take 3
    |> Seq.sum
