namespace Adventofcode

module Day10 =

    [<Literal>]
    let InputFile = "Day10Input.txt"

    let calcDiffs (jolts: int []) =
        let rec helper oneDiffs threeDiffs js =
            if Array.length js = 1
            then (oneDiffs, threeDiffs)
            else
                let diff = js.[1] - js.[0]
                let (oneDiffs', threeDiffs') =
                    if diff = 1
                    then (oneDiffs + 1, threeDiffs)
                    else if diff = 3
                    then (oneDiffs, threeDiffs + 1)
                    else (oneDiffs, threeDiffs)
                helper oneDiffs' threeDiffs' js.[1..]
        let ones, threes = helper 0 0 jolts
        ones * threes

    let day10() =
        let adapters =
            InputFile
            |> System.IO.File.ReadAllLines
            |> Array.map int
            |> Array.sort
        let device = Array.last adapters + 3
        let jolts = Array.concat [| [| 0 |]; adapters; [| device |] |]
        calcDiffs jolts
