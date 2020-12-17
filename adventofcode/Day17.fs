namespace Adventofcode

module Day17 =

    [<Literal>]
    let InputFile = "Day17Input.txt"

    let init lines =
        let a =
            Array.map (fun (s: string) -> s.ToCharArray()) lines

        let mutable space = Map.empty
        for x in [ 0 .. a.Length - 1 ] do
            for y in [ 0 .. a.[0].Length - 1 ] do
                space <- Map.add (x, y, 0) a.[x].[y] space
        space

    let copySpace (space: Map<(int * int * int), char>) =
        let mutable copy = Map.empty
        for item in space do
            copy <- Map.add item.Key item.Value space
        copy

    let grow (space: Map<(int * int * int), char>) =
        let mutable space' = copySpace space
        let spaceA = Map.toArray space |> Array.map fst
        let xSorted = Array.sortBy (fun (x, _, _) -> x) spaceA
        let ySorted = Array.sortBy (fun (_, y, _) -> y) spaceA
        let zSorted = Array.sortBy (fun (_, _, z) -> z) spaceA
        let (minX, _, _), (maxX, _, _) = Array.head xSorted, Array.last xSorted
        let (minX', maxX') = (minX - 1, maxX + 1)
        let (_, minY, _), (_, maxY, _) = Array.head ySorted, Array.last ySorted
        let (minY', maxY') = (minY - 1, maxY + 1)
        let (_, _, minZ), (_, _, maxZ) = Array.head zSorted, Array.last zSorted
        let (minZ', maxZ') = (minZ - 1, maxZ + 1)

        for y in [ minY' .. maxY' ] do
            for z in [ minZ' .. maxZ' ] do
                space' <- Map.add (minX', y, z) '.' space'
                space' <- Map.add (maxX', y, z) '.' space'

        for x in [ minX' .. maxX' ] do
            for z in [ minZ' .. maxZ' ] do
                space' <- Map.add (x, minY', z) '.' space'
                space' <- Map.add (x, maxY', z) '.' space'

        for x in [ minX' .. maxX' ] do
            for y in [ minY' .. maxY' ] do
                space' <- Map.add (x, y, minZ') '.' space'
                space' <- Map.add (x, y, maxZ') '.' space'

        space'

    let getNeighbourPositions (x, y, z) =
        seq {
            for xDiff in [ -1 .. 1 ] do
                for yDiff in [ -1 .. 1 ] do
                    for zDiff in [ -1 .. 1 ] do
                        if ((xDiff, yDiff, zDiff) <> (0, 0, 0)) then (x + xDiff, y + yDiff, z + zDiff)
        }

    let countActiveNeighbours (x, y, z) (space: Map<(int * int * int), char>) =
        let positions = getNeighbourPositions (x, y, z)
        seq {
            for p in positions do
                if (Map.containsKey p space && space.[p] = '#')
                then 1
        }
        |> Seq.length

    let cycle space =
        let mutable spaceGrown = grow space
        for kv in spaceGrown do
            let activeNeighbours = countActiveNeighbours kv.Key space
            match (activeNeighbours, kv.Value) with
            | (2, '#') -> spaceGrown <- Map.add kv.Key '#' spaceGrown
            | (3, '#') -> spaceGrown <- Map.add kv.Key '#' spaceGrown
            | (3, '.') -> spaceGrown <- Map.add kv.Key '#' spaceGrown
            | _ -> spaceGrown <- Map.add kv.Key '.' spaceGrown
        spaceGrown

    let rec doCycles (space: Map<(int * int * int), char>) n =
        if n = 0 then
            space
        else
            let space' = cycle space
            doCycles space' (n - 1)

    let count (space: Map<(int * int * int), char>) =
        space
        |> Map.filter (fun _ v -> v = '#')
        |> Map.count

    let day17 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let space = init lines
        doCycles space 6 |> count
