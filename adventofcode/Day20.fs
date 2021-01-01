namespace Adventofcode

module Day20 =

    open System

    [<Literal>]
    let InputFile = "Day20Input.txt"

    type Tile =
        { Id: int64
          Top: string
          Right: string
          Bottom: string
          Left: string }

    module Tile =

        let revString (s: string) =
            s.ToCharArray()
            |> Array.rev
            |> Array.fold (fun a c -> a + string c) ""

        let rotate90 (t: Tile) =
            { t with
                  Top = revString t.Left
                  Right = t.Top
                  Bottom = revString t.Right
                  Left = t.Bottom }

        let rotate180 = rotate90 >> rotate90

        let rotate270 = rotate90 >> rotate90 >> rotate90

        let flipH (t: Tile) =
            { t with
                  Top = t.Bottom
                  Bottom = t.Top
                  Left = revString t.Left
                  Right = revString t.Right }

        let flipV (t: Tile) =
            { t with
                  Top = revString t.Top
                  Bottom = revString t.Bottom
                  Left = t.Right
                  Right = t.Left }

        let flipVRotate90 = flipV >> rotate90
        let flipVRotate180 = flipV >> rotate90 >> rotate90

        let flipVRotate270 =
            flipV >> rotate90 >> rotate90 >> rotate90

        let flipHRotate90 = flipH >> rotate90
        let flipHRotate180 = flipH >> rotate90 >> rotate90

        let flipHRotate270 =
            flipH >> rotate90 >> rotate90 >> rotate90

        let getVariants (t: Tile) =
            [| t
               rotate90 t
               rotate180 t
               rotate270 t
               flipH t
               flipV t
               flipVRotate90 t
               flipVRotate180 t
               flipVRotate270 t
               flipHRotate90 t
               flipHRotate180 t
               flipHRotate270 t |]

    let parse (lines: string []) =
        { Id = Int64.Parse(lines.[0].Replace("Tile ", "").Replace(":", ""))
          Top = lines.[1]
          Right =
              Array.map (fun (l: string) -> string l.[9]) lines.[1..10]
              |> Array.fold (+) ""
          Bottom = lines.[10]
          Left =
              Array.map (fun (l: string) -> string l.[0]) lines.[1..10]
              |> Array.fold (+) "" }

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
            | [||] -> false
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

                if f then true else helper variants.[1..]

        let variants = Tile.getVariants tile
        helper variants

    let isUpperLeft matchesAtRight matchesAtBottom matchesAtLeft matchesAtTop =
        matchesAtRight
        && matchesAtBottom
        && not matchesAtLeft
        && not matchesAtTop

    let isUpperRight matchesAtRight matchesAtBottom matchesAtLeft matchesAtTop =
        not matchesAtRight
        && matchesAtBottom
        && matchesAtLeft
        && not matchesAtTop

    let isLowerRight matchesAtRight matchesAtBottom matchesAtLeft matchesAtTop =
        not matchesAtRight
        && not matchesAtBottom
        && matchesAtLeft
        && matchesAtTop

    let isLowerLeft matchesAtRight matchesAtBottom matchesAtLeft matchesAtTop =
        matchesAtRight
        && not matchesAtBottom
        && not matchesAtLeft
        && matchesAtTop

    let day20 () =
        let tiles =
            System.IO.File.ReadAllLines InputFile
            |> parseTiles

        let upperLeftCandidates =
            Array.filter (isCorner isUpperLeft tiles) tiles

        let upperRightCandidates =
            Array.filter (isCorner isUpperRight tiles) tiles

        let lowerRightCandidates =
            Array.filter (isCorner isLowerRight tiles) tiles

        let lowerLeftCandidates =
            Array.filter (isCorner isLowerLeft tiles) tiles

        Array.concat [| upperLeftCandidates
                        upperRightCandidates
                        lowerRightCandidates
                        lowerLeftCandidates |]
        |> Set.ofArray
        |> Set.map (fun t -> t.Id)
        |> Set.fold (*) 1L
