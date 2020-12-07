namespace Adventofcode

module Day6 =

    [<Literal>]
    let InputFile = "Day6Input.txt"

    let parse lines =
        let rec helper (sofar: string []) (lines: string []) =
            if lines = Array.empty then
                sofar
            else
                let groupLines = Array.takeWhile (fun s -> s <> "") lines
                let group = Array.fold (+) "" groupLines
                let sofar' = Array.append sofar [| group |]
                let lines' = lines.[(groupLines.Length + 1)..]
                helper sofar' lines'
        helper Array.empty lines

    let count (s: string) =
        s.ToCharArray()
        |> Set.ofArray
        |> Set.count

    let day6() =
        System.IO.File.ReadAllLines InputFile
        |> parse
        |> Array.sumBy count

    let parsePart2 lines =
        let rec helper (sofar: string [] []) (lines: string []) =
            if lines = Array.empty then
                sofar
            else
                let groupLines = Array.takeWhile (fun s -> s <> "") lines
                let sofar' = Array.append sofar [| groupLines |]
                let lines' = lines.[(groupLines.Length + 1)..]
                helper sofar' lines'
        helper Array.empty lines

    let countPart2 (group: string []) =
        let charArrays = Array.map (fun (s: string) -> s.ToCharArray() |> Set.ofArray) group
        let i = Array.fold Set.intersect charArrays.[0] charArrays
        Set.count i

    let day6Part2() =
        System.IO.File.ReadAllLines InputFile
        |> parsePart2
        |> Array.sumBy countPart2
