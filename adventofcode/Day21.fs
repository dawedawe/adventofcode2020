namespace Adventofcode

module Day21 =

    open System.IO

    [<Literal>]
    let InputFile = "Day21Input.txt"

    type Allergen =
        { Name: string
          Ingredients: Map<string, int> }

    let rec updateAllergen allergen ingredientNames =
        match ingredientNames with
        | [||] -> allergen
        | _ ->
            let ingredientName = ingredientNames.[0]

            let ingredients =
                if allergen.Ingredients.ContainsKey ingredientName then
                    Map.add ingredientName (allergen.Ingredients.[ingredientName] + 1) allergen.Ingredients
                else
                    Map.add ingredientName 1 allergen.Ingredients

            updateAllergen
                { allergen with
                      Ingredients = ingredients }
                ingredientNames.[1..]

    let parseLine (s: string) =
        let index = s.IndexOf("(contains ")

        let ingredientNames =
            s.Substring(0, index - 1).Split(" ")
            |> Array.map (fun s -> s.Trim(' '))

        let allergenNames =
            s.Substring(index + 10).Split(",")
            |> Array.map (fun s -> s.Trim(' ', ')'))

        (allergenNames, ingredientNames)

    let rec update (allergenNames: string []) (ingredientNames: string []) (combinations: Map<string, Allergen>) =
        match allergenNames with
        | [||] -> combinations
        | _ ->
            let allergenName = allergenNames.[0]

            let allergen =
                Map.tryFind allergenName combinations
                |> Option.defaultValue
                    { Name = allergenName
                      Ingredients = Map.empty }

            let allergen' = updateAllergen allergen ingredientNames

            let combinations' =
                Map.add allergenName allergen' combinations

            update allergenNames.[1..] ingredientNames combinations'

    let parse (input: string []) =
        let rec helper lines allergens =
            match lines with
            | [||] -> allergens
            | _ ->
                let (ingredientNames, allergenNames) = parseLine lines.[0]

                let allergens' =
                    update ingredientNames allergenNames allergens

                helper lines.[1..] allergens'

        helper input Map.empty

    let allIngredients (combinations: Map<string, Allergen>) =
        seq {
            for kv in combinations do
                yield kv.Value.Ingredients |> Map.toSeq |> Seq.map fst
        }
        |> Seq.map Set.ofSeq
        |> Set.unionMany

    let suspiciousIngredients (combinations: Map<string, Allergen>) =
        seq {
            for kv in combinations do
                let max =
                    kv.Value.Ingredients
                    |> Map.toList
                    |> List.maxBy snd
                    |> snd

                let baddies =
                    kv.Value.Ingredients
                    |> Map.filter (fun _ v -> v = max)
                    |> Map.toList
                    |> List.map fst

                yield baddies
        }
        |> Seq.map Set.ofSeq
        |> Set.unionMany

    let countGoodies (goodies: Set<string>) (lines: string []) =
        seq {
            for line in lines do
                let (_, ingredients) = parseLine line

                for g in goodies do
                    if Array.contains g ingredients then
                        yield 1
        }
        |> Seq.sum

    let day21 () =
        let lines = System.IO.File.ReadAllLines(InputFile)
        let allergensMap = parse lines
        let allIngredients = allIngredients allergensMap
        let baddies = suspiciousIngredients allergensMap
        let goodies = Set.difference allIngredients baddies
        countGoodies goodies lines

    type AllergenPart2 =
        { Name: string
          Ingredients: List<string * int> }

    module AllergenPart2 =

        let toAllergenPart2 (allergen: Allergen) =
            let sortedIngredients =
                allergen.Ingredients
                |> Map.toList
                |> List.sortByDescending snd
            {
                Name = allergen.Name
                Ingredients = sortedIngredients
            }

    let rec firstObvious (allergens: AllergenPart2 list) =
        match allergens with
        | [a] -> a
        | _ -> allergens
               |> List.tryFind (fun a -> a.Ingredients.Length > 0 && snd a.Ingredients.[0] <> snd a.Ingredients.[1])
               |> Option.defaultValue allergens.[0]

    let removeIngredient ingredient (allergens: AllergenPart2 list) =
        allergens
        |> List.map (fun a -> { a with Ingredients = List.filter (fun (i, _) -> i <> ingredient) a.Ingredients  } )

    let dangerList (allergensX: AllergenPart2 list) =
        let rec helper (allergens: AllergenPart2 list) (canonicalList: (string * string) list) =
            match allergens with
            | [] -> canonicalList
            | _ -> let allergen = firstObvious allergens
                   let ingredient = fst allergen.Ingredients.Head
                   let canonicalList' = canonicalList @ [(allergen.Name, ingredient)]
                   let allergens' = allergens
                                   |> List.filter (fun a -> a.Name <> allergen.Name)
                                   |> removeIngredient ingredient
                   helper allergens' canonicalList'
        helper allergensX List.empty

    let day21Part2 () =
        let allergens =
            File.ReadAllLines(InputFile)
            |> parse
            |> Map.toList
            |> List.map (snd >> AllergenPart2.toAllergenPart2)
        dangerList allergens
        |> List.sortBy fst
        |> List.map (fun (_, i) -> i)
        |> String.concat ","