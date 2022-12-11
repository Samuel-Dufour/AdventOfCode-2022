open System.IO

type HeadMove =
    | Left
    | Up
    | Right
    | Down

let lines = File.ReadLines($"{__SOURCE_DIRECTORY__}/input.txt")

type RowDistance =
    | SameRow
    | NearDown
    | NearUp
    | FarDown
    | FarUp

type RowIndex =
    | RowIndex of int

    static member (-)((RowIndex hidx), (RowIndex tidx)) =
        match hidx - tidx with
        | 0 -> SameRow
        | 1 -> NearUp
        | -1 -> NearDown
        | 2 -> FarUp
        | -2 -> FarDown
        | _ -> failwith "Too far"

type ColumnDistance =
    | SameColumn
    | NearLeft
    | NearRight
    | FarLeft
    | FarRight

type ColumnIndex =
    | ColumnIndex of int

    static member (-)((ColumnIndex hidx), (ColumnIndex tidx)) =
        match hidx - tidx with
        | 0 -> SameColumn
        | 1 -> NearRight
        | -1 -> NearLeft
        | 2 -> FarRight
        | -2 -> FarLeft
        | _ -> failwith "Too far"


type CoordsDistance =
    | FarLeft
    | FarRight
    | FarUp
    | FarDown
    | FarUpLeft
    | FarUpRight
    | FarDownRight
    | FarDownLeft
    | Near

type Head = Head of Coords
and Tail = Tail of Coords

and Coords =
    { RIdx: RowIndex
      CIdx: ColumnIndex }

    static member (-)(head, tail) =
        match head.RIdx - tail.RIdx, head.CIdx - tail.CIdx with
        | SameRow, ColumnDistance.FarLeft -> FarLeft
        | SameRow, ColumnDistance.FarRight -> FarRight
        | RowDistance.FarUp, SameColumn -> FarUp
        | RowDistance.FarDown, SameColumn -> FarDown
        | RowDistance.NearUp, ColumnDistance.FarLeft
        | RowDistance.FarUp, ColumnDistance.FarLeft
        | RowDistance.FarUp, ColumnDistance.NearLeft -> FarUpLeft
        | RowDistance.NearUp, ColumnDistance.FarRight
        | RowDistance.FarUp, ColumnDistance.FarRight
        | RowDistance.FarUp, ColumnDistance.NearRight -> FarUpRight
        | RowDistance.NearDown, ColumnDistance.FarRight
        | RowDistance.FarDown, ColumnDistance.FarRight
        | RowDistance.FarDown, ColumnDistance.NearRight -> FarDownRight
        | RowDistance.NearDown, ColumnDistance.FarLeft
        | RowDistance.FarDown, ColumnDistance.FarLeft
        | RowDistance.FarDown, ColumnDistance.NearLeft -> FarDownLeft
        | _ -> Near



module ColumnIndex =

    let moveLeft (ColumnIndex cidx) = ColumnIndex(cidx - 1)
    let moveRight (ColumnIndex cidx) = ColumnIndex(cidx + 1)


module RowIndex =
    let moveUp (RowIndex ridx) = RowIndex(ridx + 1)
    let moveDown (RowIndex ridx) = RowIndex(ridx - 1)

module Coords =
    let moveLeft coords =
        { coords with CIdx = ColumnIndex.moveLeft coords.CIdx }

    let moveRight coords =
        { coords with CIdx = ColumnIndex.moveRight coords.CIdx }

    let moveUp coords =
        { coords with RIdx = RowIndex.moveUp coords.RIdx }

    let moveDown coords =
        { coords with RIdx = RowIndex.moveDown coords.RIdx }

    let moveUpLeft coords =
        { coords with
            RIdx = RowIndex.moveUp coords.RIdx
            CIdx = ColumnIndex.moveLeft coords.CIdx }

    let moveUpRight coords =
        { coords with
            RIdx = RowIndex.moveUp coords.RIdx
            CIdx = ColumnIndex.moveRight coords.CIdx }

    let moveDownLeft coords =
        { coords with
            RIdx = RowIndex.moveDown coords.RIdx
            CIdx = ColumnIndex.moveLeft coords.CIdx }

    let moveDownRight coords =
        { coords with
            RIdx = RowIndex.moveDown coords.RIdx
            CIdx = ColumnIndex.moveRight coords.CIdx }

    let move coords dir =
        match dir with
        | Left -> moveLeft coords
        | Right -> moveRight coords
        | Up -> moveUp coords
        | Down -> moveDown coords


let (|AreTouching|MoveTail|) (Head head, Tail tail) =
    match head - tail with
    | Near -> AreTouching
    | FarLeft -> MoveTail(Coords.moveLeft tail)
    | FarRight -> MoveTail(Coords.moveRight tail)
    | FarUp -> MoveTail(Coords.moveUp tail)
    | FarDown -> MoveTail(Coords.moveDown tail)
    | FarUpLeft -> MoveTail(Coords.moveUpLeft tail)
    | FarDownRight -> MoveTail(Coords.moveDownRight tail)
    | FarUpRight -> MoveTail(Coords.moveUpRight tail)
    | FarDownLeft -> MoveTail(Coords.moveDownLeft tail)

module Move =
    let toMoves =
        let toMove (line: string) =
            let parts = line.Split(' ')

            match parts.[0].[0], int parts[1] with
            | 'L', n -> List.replicate n Left
            | 'U', n -> List.replicate n Up
            | 'R', n -> List.replicate n Right
            | 'D', n -> List.replicate n Down
            | _ -> failwith "WTF ?"

        Seq.map toMove >> Seq.collect id

module Solver =
    let moveRope acc direction =
        let moveTailIfRequired tail head =
            match Head head, Tail tail with
            | AreTouching -> tail
            | MoveTail t -> t

        let head = List.head acc
        let nextHead = Coords.move head direction
        let tail = acc |> List.skip 1

        let mapFolder acc knot =
            let nextKnot = moveTailIfRequired knot acc
            nextKnot, nextKnot

        let knots, _ = List.mapFold mapFolder nextHead tail

        nextHead :: knots

    let makeMoves knotsCount directions =
        let initRopeCoords =
            List.replicate
                knotsCount
                { RIdx = RowIndex 0
                  CIdx = ColumnIndex 0 }

        let mapFolder acc direction =
            let ropeCoords = moveRope acc direction
            ropeCoords, ropeCoords

        let allMoves, _ = directions |> Seq.mapFold mapFolder initRopeCoords
        allMoves

    let solve knotsCount lines =
        lines
        |> Move.toMoves
        |> makeMoves knotsCount
        |> Seq.map Seq.last
        |> Seq.distinct
        |> Seq.length

let puzzle1 = lines |> Solver.solve 2
let puzzle2 = lines |> Solver.solve 10
