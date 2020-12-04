namespace Adventofcode

module Day4 =

    open System.Linq

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
        
    let getFieldValue (fields : string []) (fieldName : string) =
        let field = fields |> Array.find (fun s -> s.StartsWith(fieldName))
        field.Split(":").[1]

    let isValidByr (fields : string []) =
        let value = getFieldValue fields "byr"
        let r, s = System.Int32.TryParse(value)
        r && 1920 <= s && s <= 2002

    let isValidIyr (fields : string []) =
        let value = getFieldValue fields "iyr"
        let r, s = System.Int32.TryParse(value)
        r && 2010 <= s && s <= 2020

    let isValidEyr (fields : string []) =
        let value = getFieldValue fields "eyr"
        let r, s = System.Int32.TryParse(value)
        r && 2020 <= s && s <= 2030

    let isValidHgt (fields : string []) =
        let value = getFieldValue fields "hgt"
        if value.EndsWith("cm") && value.Length = 5
        then
            let r, s = System.Int32.TryParse(value.Substring(0, 3))
            r && 150 <= s && s <= 193
        else if value.EndsWith("in") && value.Length = 4
        then
            let r, s = System.Int32.TryParse(value.Substring(0, 2))
            r && 59 <= s && s <= 76
        else
            false

    let isValidHcl (fields : string []) =
        let value = getFieldValue fields "hcl"
        let pattern = @"^#[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$"
        let regex = System.Text.RegularExpressions.Regex(pattern)
        regex.IsMatch(value)

    let isValidEcl (fields : string []) =
        let value = getFieldValue fields "ecl"
        let validColors = [| "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" |]
        Array.contains value validColors

    let isValidPid (fields : string []) =
        let value = getFieldValue fields "pid"
        value.ToCharArray().Count(fun c -> System.Char.IsDigit c) = 9

    let isValidPart2 (fields : string []) =
        let fieldNames = fields |> Array.map (fun x -> x.Substring(0, 3))
        Array.contains "byr" fieldNames && isValidByr fields
        && Array.contains "iyr" fieldNames && isValidIyr fields
        && Array.contains "eyr" fieldNames && isValidEyr fields
        && Array.contains "hgt" fieldNames && isValidHgt fields
        && Array.contains "hcl" fieldNames && isValidHcl fields
        && Array.contains "ecl" fieldNames && isValidEcl fields
        && Array.contains "pid" fieldNames && isValidPid fields

    let day4Part2 () =
        let lines = System.IO.File.ReadAllLines InputFile
        parse lines
        |> Array.filter (isValidPart2)
        |> Array.length
        