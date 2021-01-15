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
