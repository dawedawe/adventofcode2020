namespace Adventofcode

module Day10 =

    open System

    [<Literal>]
    let InputFile = "Day10Input.txt"

    let calcDiffs (jolts: int []) =
        let rec helper oneDiffs twoDiffs threeDiffs js =
            if Array.length js = 1
            then (oneDiffs, twoDiffs, threeDiffs)
            else
                let diff = js.[1] - js.[0]
                let (oneDiffs', twoDiffs', threeDiffs') =
                    match diff with
                    | 1 -> (oneDiffs + 1, twoDiffs, threeDiffs)
                    | 2 -> (oneDiffs, twoDiffs + 1, threeDiffs)
                    | 3 -> (oneDiffs, twoDiffs, threeDiffs + 1)
                    | _ -> failwith "unsupported diff"
                helper oneDiffs' twoDiffs' threeDiffs' js.[1..]
        helper 0 0 0 jolts

    let day10() =
        let adapters =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map int
            |> Array.sort
        let device = Array.last adapters + 3
        let jolts = Array.concat [| [| 0 |]; adapters; [| device |] |]
        let ones, _, threes = calcDiffs jolts
        ones * threes

    let calcSequences (jolts: int list) =
        let rec helper threes fours fives (js: int list) =
            if List.isEmpty js
            then (threes, fours, fives)
            else
                if js.[0 .. 4] = [js.[0] .. js.[0] + 4]
                then helper threes fours (fives + 1) js.[5..]
                else if js.[0 .. 3] = [js.[0] .. js.[0] + 3]
                then helper threes (fours + 1) fives js.[4..]
                else if js.[0 .. 2] = [js.[0] .. js.[0] + 2]
                then helper (threes + 1) fours fives js.[3..]
                else helper threes fours fives js.[1..]
        helper 0 0 0 jolts

    let day10Part2() =
        let adapters =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map int
            |> Array.sort
        let device = Array.last adapters + 3
        let jolts = Array.concat [| [| 0 |]; adapters; [| device |] |] |> List.ofArray
        let (threes, fours, fives) = calcSequences jolts
        Math.Pow(7.0, float fives) * Math.Pow(4.0, float fours) * Math.Pow(2.0, float threes)
        |> int64
