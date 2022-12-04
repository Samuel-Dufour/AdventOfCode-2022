open System.IO
let input = File.ReadAllLines($"{__SOURCE_DIRECTORY__}/input.txt")

type GameChoice =
    | Rock
    | Paper
    | Scissors

type GameResult =
    | Lose
    | Draw
    | Win

module GameResult =
    let fromChar c =
        match c with
        | 'X' -> Lose
        | 'Y' -> Draw
        | 'Z' -> Win
        | _ -> failwith $"can't parse {c} as GameResult"

module GameChoice =
    let choiceValue c =
        match c with
        | Rock -> 1
        | Paper -> 2
        | Scissors -> 3

    let fromChar c =
        match c with
        | 'A'
        | 'X' -> Rock
        | 'B'
        | 'Y' -> Paper
        | 'C'
        | 'Z' -> Scissors
        | _ -> failwith $"can't parse {c} as GameChoice"

    let (|GameResultValue|) (opponent, you) =
        match opponent, you with
        | Rock, Scissors
        | Paper, Rock
        | Scissors, Paper -> 0 + choiceValue you
        | Rock, Rock
        | Paper, Paper
        | Scissors, Scissors -> 3 + choiceValue you
        | Rock, Paper
        | Paper, Scissors
        | Scissors, Rock -> 6 + choiceValue you

module Game =
    let (|GameToPlay|) (opponent, result) =
        match opponent, result with
        | Rock, Lose -> Rock, Scissors
        | Paper, Lose -> Paper, Rock
        | Scissors, Lose -> Scissors, Paper
        | _, Draw -> opponent, opponent
        | Rock, Win -> Rock, Paper
        | Paper, Win -> Paper, Scissors
        | Scissors, Win -> Scissors, Rock

    let toChoices (opponent, you) =
        GameChoice.fromChar opponent, GameChoice.fromChar you

    let toChoiceAndResult (opponent, you) =
        GameChoice.fromChar opponent, GameResult.fromChar you

    let fromResultToChoices (opponent, awaited) =
        match opponent, awaited with
        | GameToPlay g -> g

    let gameResult (opponent, you) =
        match opponent, you with
        | GameChoice.GameResultValue score -> score

open Game

let toCharChoice (l: string) =
    let choices = l.Split(' ')
    choices[0].[0], choices[1].[0]

let firstPuzzle =
    let mapper = toCharChoice >> toChoices >> gameResult

    input |> Seq.map mapper |> Seq.sum

let secondPuzzle =
    let mapper =
        toCharChoice
        >> toChoiceAndResult
        >> fromResultToChoices
        >> gameResult

    input |> Seq.map mapper |> Seq.sum
