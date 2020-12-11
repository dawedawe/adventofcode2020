namespace Adventofcode

module Day11 =

    [<Literal>]
    let InputFile = "Day11Input.txt"

    let getOccupiedAdjacent x y (grid: char [] []) =
        let xBound = Array.length grid.[y] - 1
        let yBound = Array.length grid - 1
        let left = x > 0 && grid.[y].[x - 1] = '#'
        let right = x < xBound && grid.[y].[x + 1] = '#'
        let upper = y > 0 && grid.[y - 1].[x] = '#'
        let lower = y < yBound && grid.[y + 1].[x] = '#'
        let upperLeft = x > 0 && y > 0 && grid.[y - 1].[x - 1] = '#'
        let upperRight = x < xBound && y > 0 && grid.[y - 1].[x + 1] = '#'
        let lowerLeft = x > 0 && y < yBound && grid.[y + 1].[x - 1] = '#'
        let lowerRight = x < xBound && y < yBound && grid.[y + 1].[x + 1] = '#'
        Array.filter id [| left; right; upper; lower; upperLeft; upperRight; lowerLeft; lowerRight |] |> Array.length

    let applyRules x y grid =
        let occupiedAdjacent = getOccupiedAdjacent x y grid
        match (grid.[y].[x], occupiedAdjacent) with
        | ('L', 0) -> '#'
        | ('#', n) when n >= 4 -> 'L'
        | _ -> grid.[y].[x]

    let cycle (grid: char [] []) =
        let grid' = Array.map Array.copy grid
        let mutable change = false
        for y in 0 .. Array.length grid - 1 do
            for x in 0 .. Array.length grid.[y] - 1 do
                let state = applyRules x y grid
                grid'.[y].[x] <- state
                if not change then change <- grid.[y].[x] <> state
        (grid', change)

    let run grid =
        let rec helper g =
            let g', change = cycle g
            if change then helper g' else g'
        helper grid

    let day11() =
        let grid = System.IO.File.ReadAllLines InputFile |> Array.map (fun s -> s.ToCharArray())
        let grid' = run grid

        let summer =
            Array.sumBy (fun c ->
                if c = '#' then 1 else 0)
        Array.sumBy summer grid'
