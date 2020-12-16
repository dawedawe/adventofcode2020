namespace Adventofcode

module Day16 =

    [<Literal>]
    let InputFile = "Day16Input.txt"

    type Rule =
        { Name: string
          Range1: (int * int)
          Range2: (int * int) }

    let parseRule (s: string) =
        let regex =
            System.Text.RegularExpressions.Regex(@"([a-z]* [a-z]*|[a-z]*): (\d+)-(\d+) or (\d+)-(\d+)")

        let matches = regex.Matches s
        { Name = matches.[0].Groups.[1].Value
          Range1 = (int matches.[0].Groups.[2].Value, int matches.[0].Groups.[3].Value)
          Range2 = (int matches.[0].Groups.[4].Value, int matches.[0].Groups.[5].Value) }

    let rec validateField (rules: Rule []) (field: int) =
        match rules with
        | [||] -> field
        | _ ->
            let rule = rules.[0]

            let valid = (fst rule.Range1) <= field && field <= (snd rule.Range1)
                        || (fst rule.Range2) <= field && field <= (snd rule.Range2)

            if valid then 0 else validateField rules.[1..] field

    let validate (rules: Rule []) (ticket: int []) = Array.sumBy (validateField rules) ticket

    let day16 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let ruleLines = Array.takeWhile ((<>) "") lines

        let nearbyTicketsLines =
            lines.[ruleLines.Length + 5..]
            |> Array.map (fun s -> s.Split(",") |> Array.map int)

        let rules = Array.map parseRule ruleLines
        nearbyTicketsLines |> Array.sumBy (validate rules)
