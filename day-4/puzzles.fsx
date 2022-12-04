open System.IO
let input = File.ReadLines($"{__SOURCE_DIRECTORY__}/input.txt")

type Section = Section of (int * int)

module Section =
    let toSection (s: string) =
        let boundaries = s.Split("-")
        Section(int boundaries[0], int boundaries[1])

open Section

let toSectionsPair (l: string) =
    let sections = l.Split(",")
    toSection sections[0], toSection sections[1]

let areAssignmentsContainsOneInTheOther (Section (fStart, fEnd), Section (sStart, sEnd)) =
    fStart <= sStart && fEnd >= sEnd
    || sStart <= fStart && sEnd >= fEnd

let areAssignmentsOverlapping (Section (fStart, fEnd), Section (sStart, sEnd)) =
    fStart <= sStart && sStart <= fEnd
    || fEnd <= sEnd && sEnd <= fEnd
    || sStart <= fStart && fStart <= sEnd
    || sEnd <= fEnd && fEnd <= sEnd

let firstPuzzle =
    input
    |> Seq.map toSectionsPair
    |> Seq.filter areAssignmentsContainsOneInTheOther
    |> Seq.length

let secondPuzzle =
    input
    |> Seq.map toSectionsPair
    |> Seq.filter areAssignmentsOverlapping
    |> Seq.length
