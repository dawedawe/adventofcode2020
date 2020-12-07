namespace Adventofcode

module Day7 =

    [<Literal>]
    let InputFile = "Day7Input.txt"

    type Relation =
        { Outer: string
          InnerCount: int
          Inner: Option<string> }

    let parseInners (line: string) =
        let index = line.IndexOf("contain ")
        line.Substring(index + 8).Split(", ")
        |> Array.map ((fun s -> s.Split(" ")) >> (fun a -> int a.[0],  a.[1] + " " + a.[2]))

    let parse (s: string) =
        let words = s.Split(' ')
        let outer = words.[0] + " " + words.[1]
        if (s.EndsWith("contain no other bags.")) then
            [| { Outer = outer
                 Inner = None
                 InnerCount = 0 } |]
        else
            parseInners s
            |> Array.map (fun inner ->
                { Outer = outer
                  InnerCount = fst inner
                  Inner = Some (snd inner) })

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

    let getInnerBags relations bag =
        let rec helper relations b factor =
            let containedBags = Array.filter (fun r -> r.Outer = b) relations
            if (containedBags = Array.empty)
            then 0
            else
                let deeper = Array.filter (fun r -> r.InnerCount > 0) containedBags
                let nextRelations = Array.map (fun r -> helper relations r.Inner.Value r.InnerCount) deeper
                factor + Array.sumBy (fun r -> factor * r) nextRelations
        (helper relations bag 1) - 1

    let day7Part2() =
        let lines = InputFile |> System.IO.File.ReadAllLines
        let relations = Array.collect parse lines
        let bags = getInnerBags relations "shiny gold"
        bags