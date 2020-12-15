namespace Adventofcode

module Day15 =

    [<Literal>]
    let InputFile = "Day15Input.txt"

    let init (numbers: int list) =
        List.zip [ 1 .. numbers.Length ] numbers

    let rec cycle (numbers: (int * int) list) =
        let (lastPos, lastNumber) = List.last numbers
        let newPos = lastPos + 1
        let firstTime = List.filter (fun n -> snd n = lastNumber) numbers |> fun l -> List.length l = 1

        let newNumber =
            if firstTime then
                (newPos, 0)
            else
                let (beforePos, _) = List.findBack (fun n -> snd n = lastNumber && fst n <> lastPos) numbers
                let diff = lastPos - beforePos
                (newPos, diff)
        if fst newNumber = 2020 then
            newNumber
        else
            let numbers' = numbers @ [ newNumber ]
            cycle numbers'

    let day15() =
        let startingNumbers =
            System.IO.File.ReadAllText InputFile
            |> fun s -> s.Split(",")
            |> Array.map int
            |> List.ofArray

        let numbers = init startingNumbers
        cycle numbers
