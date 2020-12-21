namespace Adventofcode

module Day19 =

    open System
    open System.Text.RegularExpressions

    [<Literal>]
    let InputFile = "Day19Input.txt"

    type RuleExpression = { Id: int; SubRules: string }

    type TermRule = { Id: int; Symbol: string }

    type Rule =
        | RuleExpression of RuleExpression
        | TermRule of TermRule

    let getRuleId =
        function
        | TermRule r -> r.Id
        | RuleExpression r -> r.Id

    let parseRuleExpression (s: string) =
        let colonIndex = s.IndexOf(":")
        let id = Int32.Parse(s.Substring(0, colonIndex))
        let options = "( " + s.[colonIndex + 2..] + " )"
        RuleExpression { Id = id; SubRules = options }

    let parseTermRule (s: string) =
        let colonIndex = s.IndexOf(":")
        let id = Int32.Parse(s.Substring(0, colonIndex))
        let symbol = string s.[colonIndex + 3]
        TermRule { Id = id; Symbol = symbol }

    let parse (line: string) =
        if line.Contains('"') then parseTermRule line else parseRuleExpression line

    let ruleToString rule =
        match rule with
        | RuleExpression re -> re.SubRules
        | TermRule te -> te.Symbol

    let getFirstRuleId (sofar: string) =
        let regex = Regex(@"\d+")
        let m = regex.Match(sofar)
        if m.Success then Some(m.Index, int m.Value) else None

    let expand (rules: Map<int, Rule>) =
        let rec helper (sofar: string) =
            let ruleToExpand = getFirstRuleId sofar
            match ruleToExpand with
            | None -> sofar
            | Some (index, id) ->
                let replacement = rules.[id] |> ruleToString
                let length = (string id).Length

                let sofar' =
                    sofar.[0..index - 1]
                    + replacement
                    + sofar.[index + length..]

                helper sofar'

        helper (rules.[0] |> ruleToString)

    let count (expandedRule: string) messages =
        let regexString =
            "^" + expandedRule.Replace(" ", "") + "$"

        let regex = Regex(regexString)
        messages
        |> Array.filter (fun m -> regex.IsMatch(m))
        |> Array.length

    let day19 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let ruleLines = Array.takeWhile ((<>) "") lines
        let messages = lines.[ruleLines.Length + 1..]

        let rules =
            Array.map (parse >> (fun r -> (getRuleId r, r))) ruleLines
            |> Map.ofArray

        let expandedRule = expand rules
        count expandedRule messages
