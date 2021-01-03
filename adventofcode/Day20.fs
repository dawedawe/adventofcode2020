namespace Adventofcode

module Day20 =

    open System
    open System.Collections.Generic
    open System.Linq

    [<Literal>]
    let InputFile = "Day20Input.txt"

    type Tile =
        { Id: int64
          Top: string
          Right: string
          Bottom: string
          Left: string
          Image: string [] }

    module Tile =

        let revString (s: string) =
            s.ToCharArray()
            |> Array.rev
            |> Array.fold (fun a c -> a + string c) ""

        let rotate90 (t: Tile) =
            let rotatedImageLines =
                seq {
                    for i = 0 to 7 do
                        let rotLine =
                            t.Image
                            |> Array.map (fun line -> line.[i])
                            |> Array.rev
                            |> String

                        yield rotLine
                }

            { t with
                  Top = revString t.Left
                  Right = t.Top
                  Bottom = revString t.Right
                  Left = t.Bottom
                  Image = rotatedImageLines.ToArray() }

        let rotate180 = rotate90 >> rotate90

        let rotate270 = rotate90 >> rotate90 >> rotate90

        let flipH (t: Tile) =
            { t with
                  Top = t.Bottom
                  Bottom = t.Top
                  Left = revString t.Left
                  Right = revString t.Right
                  Image = Array.rev t.Image }

        let flipV (t: Tile) =
            { t with
                  Top = revString t.Top
                  Bottom = revString t.Bottom
                  Left = t.Right
                  Right = t.Left
                  Image = t.Image |> Array.map revString }

        let flipVRotate90 = flipV >> rotate90

        let flipHRotate90 = flipH >> rotate90

        let getVariants (t: Tile) =
            [| t
               flipH t
               flipV t
               rotate90 t
               rotate180 t
               rotate270 t
               flipVRotate90 t
               flipHRotate90 t |]

    let parse (lines: string []) =
        { Id = Int64.Parse(lines.[0].Replace("Tile ", "").Replace(":", ""))
          Top = lines.[1]
          Right =
              Array.map (fun (l: string) -> l.[9]) lines.[1..10]
              |> String
          Bottom = lines.[10]
          Left =
              Array.map (fun (l: string) -> l.[0]) lines.[1..10]
              |> String
          Image = lines.[2..9] |> Array.map (fun l -> l.[1..8]) }

    let parseTiles (strings: string []) =
        let rec helper (lines: string []) tiles =
            if lines.Length < 11 then
                tiles
            else
                let tile = parse lines.[0..10]
                let tiles' = Array.append tiles [| tile |]
                helper lines.[12..] tiles'

        helper strings Array.empty

    let findMatches compareF (tile: Tile) (otherTile: Tile) =
        let variants = Tile.getVariants otherTile
        let mutable foundMatch = false
        let mutable i = 0
        while not foundMatch && i < Array.length variants do
            foundMatch <- compareF tile variants.[i]
            i <- i + 1
        foundMatch

    let matchesToTheRight tile candidate = tile.Right = candidate.Left
    let matchesToTheLeft tile candidate = tile.Left = candidate.Right
    let matchesToTheTop tile candidate = tile.Top = candidate.Bottom
    let matchesToTheBottom tile candidate = tile.Bottom = candidate.Top

    let isCorner cornerPredicate (tiles: Tile []) (tile: Tile) =
        let tiles' =
            Array.filter (fun t -> t.Id <> tile.Id) tiles

        let rec helper variants =
            match variants with
            | [||] -> None
            | _ ->
                let variant = Array.head variants

                let matchesAtRight =
                    Array.map (findMatches matchesToTheRight variant) tiles'
                    |> Array.fold (||) false

                let matchesAtBottom =
                    Array.map (findMatches matchesToTheBottom variant) tiles'
                    |> Array.fold (||) false

                let matchesAtLeft =
                    Array.map (findMatches matchesToTheLeft variant) tiles'
                    |> Array.fold (||) false

                let matchesAtTop =
                    Array.map (findMatches matchesToTheTop variant) tiles'
                    |> Array.fold (||) false

                let f =
                    cornerPredicate matchesAtRight matchesAtBottom matchesAtLeft matchesAtTop

                if f then Some variant else helper variants.[1..]

        let variants = Tile.getVariants tile
        helper variants

    let isUpperLeft matchesAtRight matchesAtBottom matchesAtLeft matchesAtTop =
        matchesAtRight
        && matchesAtBottom
        && not matchesAtLeft
        && not matchesAtTop

    let getNextPosToFill (tilesCount: int) (image: Dictionary<(int * int), Tile>) =
        let maxIndex = int (Math.Sqrt(float tilesCount)) - 1

        let s =
            seq {
                for x in 0 .. maxIndex do
                    for y in 0 .. maxIndex do
                        let k1 = (x + 1, y)
                        let k2 = (x, y + 1)
                        let k3 = (x + 1, y + 1)
                        if fst k1
                           <= maxIndex
                           && snd k1 <= maxIndex
                           && not (image.ContainsKey(k1)) then
                            yield k1
                        else if fst k2
                                <= maxIndex
                                && snd k2 <= maxIndex
                                && not (image.ContainsKey(k2)) then
                            yield k2
                        else if fst k3
                                <= maxIndex
                                && snd k3 <= maxIndex
                                && not (image.ContainsKey(k3)) then
                            yield k3

            }

        s.First()

    let matchesInto (image: Dictionary<(int * int), Tile>) ((x, y): int * int) (tile: Tile) =
        let upper = (x, y - 1)
        let left = (x - 1, y)

        let match1 =
            if image.ContainsKey(upper) then image.[upper].Bottom = tile.Top else true

        let match2 =
            if image.ContainsKey(left) then image.[left].Right = tile.Left else true

        match1 && match2

    let getMatchingTiles (tiles: Tile []) (image: Dictionary<(int * int), Tile>) (nextPosToFill: int * int) =
        let consumedId = image.Values.Select(fun v -> v.Id)

        let tilesNotConsumed =
            tiles
            |> Array.filter (fun t -> not (consumedId.Contains(t.Id)))

        let tiles' =
            tilesNotConsumed |> Array.collect Tile.getVariants

        let matchingTiles =
            Array.filter (matchesInto image nextPosToFill) tiles'

        matchingTiles

    let bfs (tiles: Tile []) =
        let mutable completedImage = None

        let upperLeftCandidates =
            Array.map (isCorner isUpperLeft tiles) tiles
            |> Array.filter Option.isSome
            |> Array.map Option.get

        let queue = Queue<Dictionary<(int * int), Tile>>()

        for c in upperLeftCandidates do
            let d = Dictionary<(int * int), Tile>()
            d.[(0, 0)] <- c
            queue.Enqueue(d)

        while (queue.Count <> 0 && Option.isNone completedImage) do
            let currentImage = queue.Peek()
            if currentImage.Count = tiles.Length then
                completedImage <- Some currentImage
            else
                queue.Dequeue() |> ignore

                let nextPosToFill =
                    getNextPosToFill tiles.Length currentImage

                let matchingTiles =
                    getMatchingTiles tiles currentImage nextPosToFill

                for matchingTile in matchingTiles do
                    let newImage =
                        currentImage.ToDictionary((fun kv -> kv.Key), (fun k -> k.Value))

                    newImage.[nextPosToFill] <- matchingTile
                    queue.Enqueue(newImage)

        completedImage

    let calcCorners (image: Dictionary<(int * int), Tile>) tilesCount =
        let maxIndex = int (Math.Sqrt(float tilesCount)) - 1
        let upperLeft = image.[0, 0].Id
        let upperRight = image.[0, maxIndex].Id
        let lowerLeft = image.[maxIndex, 0].Id
        let lowerRight = image.[maxIndex, maxIndex].Id
        upperLeft * upperRight * lowerLeft * lowerRight

    let day20 () =
        let tiles =
            System.IO.File.ReadAllLines InputFile
            |> parseTiles

        let image = bfs tiles |> Option.get
        let r = calcCorners image tiles.Length
        r

    let stitch (image: Dictionary<(int * int), Tile>) tilesCount =
        let maxIndex = int (Math.Sqrt(float tilesCount)) - 1
        seq {
            for y in 0 .. maxIndex do
                let tilesInY =
                    image.Where(fun kv -> snd kv.Key = y).Select(fun kv -> kv.Value)

                for lineIndex in 0 .. 7 do
                    let line =
                        tilesInY
                        |> Seq.map (fun t -> t.Image.[lineIndex])
                        |> String.concat ("")

                    yield line
        }
        |> Seq.toArray

    let rotate90 (image: string []) =
        seq {
            for i = 0 to image.[0].Length - 1 do
                let rotLine =
                    image
                    |> Array.map (fun line -> line.[i])
                    |> Array.rev
                    |> String

                yield rotLine
        }
        |> Seq.toArray

    let rotate180 = rotate90 >> rotate90

    let rotate270 = rotate90 >> rotate90 >> rotate90

    let flipH (image: string []) = Array.rev image

    let flipV (image: string []) =
        image
        |> Array.map (fun s -> s.ToCharArray() |> Array.rev |> System.String)

    let flipVRotate90 = flipV >> rotate90

    let flipHRotate90 = flipH >> rotate90

    let getVariants (image: string []) =
        [| image
           flipH image
           flipV image
           rotate90 image
           rotate180 image
           rotate270 image
           flipVRotate90 image
           flipHRotate90 image |]

    let findIndexCandidates (s: string) =
        let monster = "#    ##    ##    ###"
        seq {
            for i in 0 .. (s.Length - monster.Length) do
                if s.[i] = '#'
                   && s.[i + 5] = '#'
                   && s.[i + 6] = '#'
                   && s.[i + 11] = '#'
                   && s.[i + 12] = '#'
                   && s.[i + 17] = '#'
                   && s.[i + 18] = '#'
                   && s.[i + 19] = '#' then
                    yield i
        }
        |> Seq.toArray

    let findNessies (image: string []) =
        let windows = Array.windowed 3 image
        seq {
            for w in windows do
                let indexCandidates = findIndexCandidates w.[1]
                for index in indexCandidates do
                    let upperOk = w.[0].[index + 18] = '#'

                    let lowerOk =
                        w.[2].[index + 1] = '#'
                        && w.[2].[index + 4] = '#'
                        && w.[2].[index + 7] = '#'
                        && w.[2].[index + 10] = '#'
                        && w.[2].[index + 13] = '#'
                        && w.[2].[index + 16] = '#'

                    if upperOk && lowerOk then yield 1
        }
        |> Seq.sum

    let countNessies (image: string []) =
        getVariants image
        |> Array.map findNessies
        |> Array.find (fun c -> c > 0)

    let day20Part2 () =
        let tiles =
            System.IO.File.ReadAllLines InputFile
            |> parseTiles

        let image = bfs tiles |> Option.get
        let r = calcCorners image tiles.Length
        let stichedImage = stitch image tiles.Length
        let nessieCount = countNessies stichedImage
        let c = nessieCount * 15

        let sum =
            stichedImage
            |> Array.sumBy (fun (l: string) -> l.ToCharArray().Count(fun c -> c = '#'))

        sum - c
