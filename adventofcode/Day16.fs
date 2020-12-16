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

            let valid =
                (fst rule.Range1)
                <= field
                && field <= (snd rule.Range1)
                || (fst rule.Range2)
                   <= field
                   && field <= (snd rule.Range2)

            if valid then 0 else validateField rules.[1..] field

    let validate (rules: Rule []) (ticket: int []) = Array.sumBy (validateField rules) ticket

    let day16 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let ruleLines = Array.takeWhile ((<>) "") lines

        let nearbyTickets =
            lines.[ruleLines.Length + 5..]
            |> Array.map (fun s -> s.Split(",") |> Array.map int)

        let rules = Array.map parseRule ruleLines
        nearbyTickets |> Array.sumBy (validate rules)

    let rec validateRule (values: int []) (rule: Rule) =
        match values with
        | [||] -> true
        | _ ->
            let value = values.[0]

            let valid =
                (fst rule.Range1)
                <= value
                && value <= (snd rule.Range1)
                || (fst rule.Range2)
                   <= value
                   && value <= (snd rule.Range2)

            if valid then validateRule values.[1..] rule else false

    let findMatchingRules (rules: Rule []) (tickets: int [] []) =
        let helper (fieldValues: int []) =
            Array.filter (validateRule fieldValues) rules

        seq {
            for i in [ 0 .. tickets.[0].Length - 1 ] do
                let iValues = tickets |> Array.map (fun a -> a.[i])
                yield (i, helper iValues)
        }

    let findFieldOrder (rules: Rule []) (tickets: int [] []) =
        let rec helper (fieldsAndRules: Set<int * (Rule [])>) =

            let mutable finalMatchings =
                Set.filter (fun (_, x) -> Array.length x = 1) fieldsAndRules

            let mutable undecidedMatchings =
                Set.difference fieldsAndRules finalMatchings

            for (_, finalMatching) in finalMatchings do
                let rulesToTrim =
                    Set.filter (fun (_, rs) ->
                        (Array.tryFind (fun r -> r.Name = finalMatching.[0].Name) rs)
                        <> None) undecidedMatchings

                for (i, rs) in rulesToTrim do
                    let trimmmed =
                        Array.filter (fun r -> r.Name <> finalMatching.[0].Name) rs

                    undecidedMatchings <- undecidedMatchings.Remove(i, rs)
                    undecidedMatchings <- undecidedMatchings.Add(i, trimmmed)

            let u =
                Set.union finalMatchings undecidedMatchings

            let undecided =
                Set.filter (fun (_, x) -> Array.length x > 1) u
                |> Set.count

            if (undecided > 0) then helper u else u


        let fieldsAndMatchingRules =
            findMatchingRules rules tickets |> Set.ofSeq

        helper fieldsAndMatchingRules

    let day16Part2 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let ruleLines = Array.takeWhile ((<>) "") lines

        let myTicket =
            lines.[ruleLines.Length + 2]
            |> fun s -> s.Split(",")
            |> Array.map int

        let nearbyTickets =
            lines.[ruleLines.Length + 5..]
            |> Array.map (fun s -> s.Split(",") |> Array.map int)

        let rules = Array.map parseRule ruleLines

        let validTickets =
            nearbyTickets
            |> Array.filter (fun t -> validate rules t = 0)

        let indexes =
            findFieldOrder rules validTickets
            |> Seq.filter (fun (i, r) -> r.[0].Name.StartsWith("departure"))
            |> Seq.map fst

        let product =
            seq {
                for i in indexes do
                    yield int64 myTicket.[i]
            }
            |> Seq.fold (*) 1L

        product
