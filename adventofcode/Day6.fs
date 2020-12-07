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
