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

    let addPosAndNumber (numbers: Map<int, int list>) p n =
        if Map.containsKey n numbers then
            let lastPos = List.last numbers.[n]
            let newPos = [ lastPos; p ]
            Map.add n newPos numbers
        else
            Map.add n [ p ] numbers

    let initPart2 (numbers: int list) =
        let posValuePairs = List.zip [ 1 .. numbers.Length ] numbers
        List.fold (fun m (p, v) -> addPosAndNumber m p v) Map.empty posValuePairs

    let rec cyclePart2 (numbers: Map<int, int list>) (lastSaid: int) =
        let positionsOfLastSaid = numbers.[lastSaid]
        let lastPos = List.last positionsOfLastSaid
        let newPos = lastPos + 1
        let firstTime = positionsOfLastSaid.Length = 1
        let newNumber = if firstTime then 0 else positionsOfLastSaid.[1] - positionsOfLastSaid.[0]
        if newPos = 30000000 then
            newNumber
        else
            let numbers' = addPosAndNumber numbers newPos newNumber
            cyclePart2 numbers' newNumber

    let day15Part2() =
        let startingNumbers =
            System.IO.File.ReadAllText InputFile
            |> fun s -> s.Split(",")
            |> Array.map int
            |> List.ofArray

        let numbers = initPart2 startingNumbers
        cyclePart2 numbers (List.last startingNumbers)
