open System.IO
open System.Text.RegularExpressions
open System

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)

    if m.Success then
        Some(List.tail [ for x in m.Groups -> x.Value ])
    else
        None

type Instruction =
    | Noop
    | Addx of int
    static member fromLine line =
        match line with
        | "noop" -> Noop
        | ParseRegex @"addx (-?\d+)" [ n ] -> Addx(int n)
        | _ -> failwith $"Can parse {line}"

type CycleInstruction =
    | DoNothing
    | AddN of int
    static member fromInstruction instruction =
        match instruction with
        | Noop -> seq { DoNothing }
        | Addx n ->
            seq {
                DoNothing
                AddN n
            }

let toInstructions lines =
    lines
    |> Seq.map Instruction.fromLine
    |> Seq.map CycleInstruction.fromInstruction
    |> Seq.collect id
    |> List.ofSeq

type CycleValues = { currentValue: int; nextValue: int }

let toSteps mapper list =
    let folder acc i =
        let { currentValue = currentValue
              nextValue = nextValue } =
            acc

        match i with
        | DoNothing ->
            let vals = { acc with currentValue = nextValue }
            vals, vals
        | AddN increment ->
            let vals = { acc with nextValue = currentValue + increment }
            vals, vals

    let (steps, _) = List.mapFold folder { currentValue = 1; nextValue = 1 } list
    steps |> Seq.mapi mapper

module Puzzle1 =
    type Step = { cycle: int; value: int }

    let toSteps =
        let mapper idx n =
            { cycle = idx + 1
              value = n.currentValue }

        toSteps mapper

    let toInterestingSteps =
        let refSteps = [ 20; 60; 100; 140; 180; 220 ]
        Seq.filter (fun i -> refSteps |> List.contains i.cycle)

    let sumProducts list =
        list |> Seq.sumBy (fun i -> i.cycle * i.value)

module Puzzle2 =
    type Step =
        { cycle: int
          lineIndex: int
          display: char
          value: int }

    let toSteps =
        let mapper idx n =
            let lineIndex = idx % 40
            let value = n.currentValue

            { cycle = idx + 1
              lineIndex = lineIndex
              value = value
              display =
                if lineIndex - 1 <= value && value <= lineIndex + 1 then
                    '#'
                else
                    '.' }

        toSteps mapper

    let toStringArray steps =
        steps
        |> Seq.map (fun i -> i.display)
        |> Seq.chunkBySize 40
        |> Seq.map (Array.ofSeq >> String)


let lines = File.ReadLines($"{__SOURCE_DIRECTORY__}/input.txt")

open Puzzle1

let firstPuzzle =
    lines
    |> toInstructions
    |> toSteps
    |> toInterestingSteps
    |> sumProducts

open Puzzle2

let secondPuzzle =
    lines
    |> toInstructions
    |> toSteps
    |> toStringArray
    |> List.ofSeq
