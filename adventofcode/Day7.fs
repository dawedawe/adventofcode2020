namespace Adventofcode

module Day7 =

    [<Literal>]
    let InputFile = "Day7Input.txt"

    type Relation =
        { Outer: string
          Inner: Option<string> }

    let parseInners (line: string) =
        let index = line.IndexOf("contain ")
        line.Substring(index + 8).Split(", ") |> Array.map ((fun s -> s.Split(" ")) >> (fun a -> a.[1] + " " + a.[2]))

    let parse (s: string) =
        let words = s.Split(' ')
        let outer = words.[0] + " " + words.[1]
        if (s.EndsWith("contain no other bags.")) then
            [| { Outer = outer
                 Inner = None } |]
        else
            parseInners s
            |> Array.map (fun inner ->
                { Outer = outer
                  Inner = Some inner })

    let rec getRelations relations inner =
        let containingRelations = Array.filter (fun r -> r.Inner = Some inner) relations
        if (Array.isEmpty containingRelations) then
            Array.empty
        else
            let containingRelations' = Array.map (fun r -> getRelations relations r.Outer) containingRelations
            let x = Array.concat containingRelations'
            Array.append containingRelations x

    let day7() =
        let lines = InputFile |> System.IO.File.ReadAllLines
        let relations = Array.collect parse lines
        let outers =
            getRelations relations "shiny gold" 
            |> Array.map (fun r -> r.Outer)
            |> Set.ofArray
        Set.count outers
