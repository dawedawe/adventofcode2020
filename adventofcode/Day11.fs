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

    let cycle rulesFunc (grid: char [] []) =
        let grid' = Array.map Array.copy grid
        let mutable change = false
        for y in 0 .. Array.length grid - 1 do
            for x in 0 .. Array.length grid.[y] - 1 do
                let state = rulesFunc x y grid
                grid'.[y].[x] <- state
                if not change then change <- grid.[y].[x] <> state
        (grid', change)

    let run rulesFunc grid =
        let rec helper g =
            let g', change = cycle rulesFunc g
            if change then helper g' else g'
        helper grid

    let day11() =
        let grid = System.IO.File.ReadAllLines InputFile |> Array.map (fun s -> s.ToCharArray())
        let grid' = run applyRules grid

        let summer =
            Array.sumBy (fun c ->
                if c = '#' then 1 else 0)
        Array.sumBy summer grid'

    let isLegal x y (grid: 'a [] []) =
        let xBound = Array.length grid.[0] - 1
        let yBound = Array.length grid - 1
        0 <= x && x <= xBound && 0 <= y && y <= yBound

    let isSeatInSightOccupied (x, y) (xDiff, yDiff) (grid: char [] []) =
        let rec helper (xPos, yPos) =
            let xPos', yPos' = xPos + xDiff, yPos + yDiff
            if isLegal xPos' yPos' grid then
                if grid.[yPos'].[xPos'] = 'L' || grid.[yPos'].[xPos'] = '#' then
                    grid.[yPos'].[xPos']
                else
                    helper (xPos', yPos')
            else
                '.'
        helper (x, y) = '#'

    let getOccupiedInSight x y (grid: char [] []) =
        let left = isSeatInSightOccupied (x, y) (-1, 0) grid
        let right = isSeatInSightOccupied (x, y) (1, 0) grid
        let upper = isSeatInSightOccupied (x, y) (0, -1) grid
        let lower = isSeatInSightOccupied (x, y) (0, 1) grid
        let upperLeft = isSeatInSightOccupied (x, y) (-1, -1) grid
        let upperRight = isSeatInSightOccupied (x, y) (1, -1) grid
        let lowerLeft = isSeatInSightOccupied (x, y) (-1, 1) grid
        let lowerRight = isSeatInSightOccupied (x, y) (1, 1) grid
        Array.filter id [| left; right; upper; lower; upperLeft; upperRight; lowerLeft; lowerRight |] |> Array.length

    let applyRulesPart2 x y grid =
        let occupiedInSight = getOccupiedInSight x y grid
        match (grid.[y].[x], occupiedInSight) with
        | ('L', 0) -> '#'
        | ('#', n) when n >= 5 -> 'L'
        | _ -> grid.[y].[x]

    let day11Part2() =
        let grid = System.IO.File.ReadAllLines InputFile |> Array.map (fun s -> s.ToCharArray())
        let grid' = run applyRulesPart2 grid

        let summer =
            Array.sumBy (fun c ->
                if c = '#' then 1 else 0)
        Array.sumBy summer grid'
