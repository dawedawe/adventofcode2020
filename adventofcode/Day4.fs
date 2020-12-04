namespace Adventofcode

module Day4 =

    [<Literal>]
    let InputFile = "Day4Input.txt"

    let normalize (lines : string []) =
        lines
        |> Array.map (fun x -> x.Split(" "))
        |> Array.collect id

    let isValid (s : string []) =
        let fieldNames = s |> Array.map (fun x -> x.Substring(0, 3))
        Array.contains "byr" fieldNames
        && Array.contains "iyr" fieldNames
        && Array.contains "eyr" fieldNames
        && Array.contains "hgt" fieldNames
        && Array.contains "hcl" fieldNames
        && Array.contains "ecl" fieldNames
        && Array.contains "pid" fieldNames

    let parse lines =
        let rec helper (sofar : string [] []) (lines : string []) =
            if lines = Array.empty
            then sofar
            else
                let passportLines = Array.takeWhile (fun s -> s <> "") lines
                let passport = normalize passportLines
                let sofar' = Array.append sofar [| passport |]
                let lines' = lines.[(passportLines.Length + 1)..]
                helper sofar' lines'
        helper Array.empty lines

    let day4 () =
        let lines = System.IO.File.ReadAllLines InputFile
        parse lines
        |> Array.filter (isValid)
        |> Array.length
        