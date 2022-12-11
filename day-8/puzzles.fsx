open System.IO

module Array2D =
    let fromArray (lines: 'a [] []) =
        let init ridx cidx = lines.[ridx].[cidx]
        let rowsCount = Array.length lines

        let columnsCount =
            lines
            |> Array.tryHead
            |> Option.map Array.length
            |> Option.defaultValue 0

        Array2D.init rowsCount columnsCount init

    let toList (input: 'a [,]) =
        [ for ridx in 0 .. Array2D.length1 input - 1 do
              for cidx in 0 .. Array2D.length2 input - 1 do
                  input[ridx, cidx] ]


module Puzzle1 =
    let visibleTreesCount (trees: 'a [,]) =
        let isVisible ridx cidx t =
            let isVisible trees =
                Array.forall (fun tree -> tree < t) trees

            let onLeftTrees = trees[ridx, 0 .. cidx - 1]
            let onTopTrees = trees[0 .. ridx - 1, cidx]
            let onRightTrees = trees[ridx, cidx + 1 ..]
            let onBottomTrees = trees[ridx + 1 .., cidx]

            isVisible onLeftTrees
            || isVisible onBottomTrees
            || isVisible onRightTrees
            || isVisible onTopTrees

        let res = Array2D.mapi isVisible trees

        res
        |> Array2D.toList
        |> List.filter ((=) true)
        |> List.length

module Puzzle2 =

    let maxScenicScore (trees: 'a [,]) =
        let scenicScore ridx cidx t =
            let visibleTrees trees =
                match Array.tryFindIndex (fun tree -> tree >= t) trees with
                | Some i -> i + 1
                | None -> Array.length trees

            let onLeftTrees = trees[ridx, 0 .. cidx - 1] |> Array.rev
            let onTopTrees = trees[0 .. ridx - 1, cidx] |> Array.rev
            let onRightTrees = trees[ridx, cidx + 1 ..]
            let onBottomTrees = trees[ridx + 1 .., cidx]

            let scenicScore =
                visibleTrees onLeftTrees
                * visibleTrees onTopTrees
                * visibleTrees onBottomTrees
                * visibleTrees onRightTrees

            scenicScore

        Array2D.mapi scenicScore trees
        |> Array2D.toList
        |> List.max


let trees =
    File.ReadAllLines($"{__SOURCE_DIRECTORY__}/input.txt")
    |> Array.map (Seq.map (string >> int) >> Array.ofSeq)
    |> Array2D.fromArray

Puzzle1.visibleTreesCount trees
Puzzle2.maxScenicScore trees
