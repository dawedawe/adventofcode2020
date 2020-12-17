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

    let copySpace (space: Map<'a, 'b>) =
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

    let countActiveNeighbours (key: 'a) posFunc (space: Map<'a, char>) =
        let positions = posFunc key
        seq {
            for p in positions do
                if (Map.containsKey p space && space.[p] = '#')
                then 1
        }
        |> Seq.length

    let cycle (growFunc: Map<'a, char> -> Map<'a, char>) (posFunc: 'a -> seq<'a>) space =
        let mutable spaceGrown = growFunc space
        for kv in spaceGrown do
            let activeNeighbours =
                countActiveNeighbours kv.Key posFunc space

            match (activeNeighbours, kv.Value) with
            | (2, '#') -> spaceGrown <- Map.add kv.Key '#' spaceGrown
            | (3, '#') -> spaceGrown <- Map.add kv.Key '#' spaceGrown
            | (3, '.') -> spaceGrown <- Map.add kv.Key '#' spaceGrown
            | _ -> spaceGrown <- Map.add kv.Key '.' spaceGrown
        spaceGrown

    let rec doCycles cycleFunc (space: Map<'a, char>) n =
        if n = 0 then
            space
        else
            let space' = cycleFunc space
            doCycles cycleFunc space' (n - 1)

    let count (space: Map<'a, char>) =
        space
        |> Map.filter (fun _ v -> v = '#')
        |> Map.count

    let day17 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let space = init lines
        doCycles (cycle grow getNeighbourPositions) space 6
        |> count

    let initPart2 lines =
        let a =
            Array.map (fun (s: string) -> s.ToCharArray()) lines

        let mutable space = Map.empty
        for x in [ 0 .. a.Length - 1 ] do
            for y in [ 0 .. a.[0].Length - 1 ] do
                space <- Map.add (x, y, 0, 0) a.[x].[y] space
        space

    let growPart2 (space: Map<(int * int * int * int), char>) =
        let mutable space' = copySpace space
        let spaceA = Map.toArray space |> Array.map fst

        let xSorted =
            Array.sortBy (fun (x, _, _, _) -> x) spaceA

        let ySorted =
            Array.sortBy (fun (_, y, _, _) -> y) spaceA

        let zSorted =
            Array.sortBy (fun (_, _, z, _) -> z) spaceA

        let wSorted =
            Array.sortBy (fun (_, _, _, w) -> w) spaceA

        let (minX, _, _, _), (maxX, _, _, _) = Array.head xSorted, Array.last xSorted
        let (minX', maxX') = (minX - 1, maxX + 1)
        let (_, minY, _, _), (_, maxY, _, _) = Array.head ySorted, Array.last ySorted
        let (minY', maxY') = (minY - 1, maxY + 1)
        let (_, _, minZ, _), (_, _, maxZ, _) = Array.head zSorted, Array.last zSorted
        let (minZ', maxZ') = (minZ - 1, maxZ + 1)
        let (_, _, _, minW), (_, _, _, maxW) = Array.head wSorted, Array.last wSorted
        let (minW', maxW') = (minW - 1, maxW + 1)

        for y in [ minY' .. maxY' ] do
            for z in [ minZ' .. maxZ' ] do
                for w in [ minW' .. maxW' ] do
                    space' <- Map.add (minX', y, z, w) '.' space'
                    space' <- Map.add (maxX', y, z, w) '.' space'

        for x in [ minX' .. maxX' ] do
            for z in [ minZ' .. maxZ' ] do
                for w in [ minW' .. maxW' ] do
                    space' <- Map.add (x, minY', z, w) '.' space'
                    space' <- Map.add (x, maxY', z, w) '.' space'

        for x in [ minX' .. maxX' ] do
            for y in [ minY' .. maxY' ] do
                for w in [ minW' .. maxW' ] do
                    space' <- Map.add (x, y, minZ', w) '.' space'
                    space' <- Map.add (x, y, maxZ', w) '.' space'

        for x in [ minX' .. maxX' ] do
            for y in [ minY' .. maxY' ] do
                for z in [ minZ' .. maxZ' ] do
                    space' <- Map.add (x, y, z, minW') '.' space'
                    space' <- Map.add (x, y, z, maxW') '.' space'

        space'

    let getNeighbourPositionsPart2 (x, y, z, w) =
        seq {
            for xDiff in [ -1 .. 1 ] do
                for yDiff in [ -1 .. 1 ] do
                    for zDiff in [ -1 .. 1 ] do
                        for wDiff in [ -1 .. 1 ] do
                            if ((xDiff, yDiff, zDiff, wDiff) <> (0, 0, 0, 0))
                            then (x + xDiff, y + yDiff, z + zDiff, w + wDiff)
        }

    let day17Part2 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let space = initPart2 lines
        doCycles (cycle growPart2 getNeighbourPositionsPart2) space 6
        |> count
