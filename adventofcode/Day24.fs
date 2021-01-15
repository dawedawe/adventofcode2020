namespace Adventofcode

module Day24 =

    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let InputFile = "Day24Input.txt"

    type Direction =
        | East
        | SouthEast
        | SouthWest
        | West
        | NorthWest
        | NorthEast

    let parseLine line =
        let rec helper (s: string) sofar =
            match s with
            | _ when s.StartsWith("e") -> helper s.[1..] (sofar @ [ East ])
            | _ when s.StartsWith("se") -> helper s.[2..] (sofar @ [ SouthEast ])
            | _ when s.StartsWith("sw") -> helper s.[2..] (sofar @ [ SouthWest ])
            | _ when s.StartsWith("w") -> helper s.[1..] (sofar @ [ West ])
            | _ when s.StartsWith("nw") -> helper s.[2..] (sofar @ [ NorthWest ])
            | _ when s.StartsWith("ne") -> helper s.[2..] (sofar @ [ NorthEast ])
            | "" -> sofar
            | _ -> failwith "can't parse"

        helper line List.empty

    let move (x, y) direction =
        match direction with
        | East -> (x, y + 1)
        | SouthEast -> (x - 1, y + 1)
        | SouthWest -> (x - 1, y)
        | West -> (x, y - 1)
        | NorthWest -> (x + 1, y - 1)
        | NorthEast -> (x + 1, y)

    let follow = List.fold move (0, 0)

    let tile directions =
        let floor = Dictionary<(int * int), bool>()

        for d in directions do
            let p = follow d

            floor.[p] <-
                if floor.ContainsKey p then
                    not floor.[p]
                else
                    true

        floor

    let day24 () =
        let floor =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map parseLine
            |> tile

        floor.Values.Where(id).Count()

    let fillFloor (floor: Dictionary<int * int, bool>) =
        let xPositions = floor.Keys.Select(fst).ToList()
        let minX = xPositions.Min()
        let maxX = xPositions.Max()
        let yPositions = floor.Keys.Select(snd).ToList()
        let minY = yPositions.Min()
        let maxY = yPositions.Max()

        for x in minX .. maxX do
            for y in minY .. maxY do
                if not (floor.ContainsKey(x, y)) then
                    floor.[(x, y)] <- false

    let addBorders (floor: Dictionary<int * int, bool>) =
        let xPositions = floor.Keys.Select(fst).ToList()
        let minX = xPositions.Min() - 1
        let maxX = xPositions.Max() + 1
        let yPositions = floor.Keys.Select(snd).ToList()
        let minY = yPositions.Min() - 1
        let maxY = yPositions.Max() + 1

        for x in [ minX; maxX ] do
            for y in [ minY .. maxY ] do
                if not (floor.ContainsKey(x, y)) then
                    floor.[(x, y)] <- false

        for y in [ minY; maxY ] do
            for x in [ minX .. maxX ] do
                if not (floor.ContainsKey(x, y)) then
                    floor.[(x, y)] <- false

    let copyFloor (currentFloor: Dictionary<int * int, bool>) =
        let copy = Dictionary<int * int, bool>()

        for kv in currentFloor do
            copy.Add(kv.Key, kv.Value)

        copy

    let countBlackNeighbours (floor: Dictionary<int * int, bool>) p =
        let neighbourPos =
            [ move p NorthEast
              move p East
              move p SouthEast
              move p SouthWest
              move p West
              move p NorthWest ]

        seq {
            for n in neighbourPos do
                if floor.ContainsKey(n) && floor.[n] then
                    yield 1
        }
        |> Seq.sum

    let trans (floor: Dictionary<int * int, bool>) p =
        let blackNeighbours = countBlackNeighbours floor p

        match floor.[p], blackNeighbours with
        | true, n when n = 0 || n > 2 -> false
        | false, 2 -> true
        | _ -> floor.[p]

    let processDays (floor: Dictionary<int * int, bool>) days =
        let rec helper n (currentFloor: Dictionary<int * int, bool>) =
            if n = 0 then
                currentFloor
            else
                addBorders currentFloor
                let nextFloor = copyFloor currentFloor

                for p in currentFloor.Keys do
                    nextFloor.[p] <- trans currentFloor p

                helper (n - 1) nextFloor

        helper days floor

    let day24Part2 () =
        let floor =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map parseLine
            |> tile

        fillFloor floor
        let floor' = processDays floor 100
        floor'.Values.Where(id).Count()
