namespace Adventofcode

module Day9 =

    [<Literal>]
    let InputFile = "Day9Input.txt"

    let findPair (preamble: int64 []) (x: int64) =
        let rec helper pairs =
            match pairs with
            | [||] -> None
            | _ ->
                let (a, b) = pairs.[0]
                if (a <> b && a + b = x) then Some(a, b) else helper pairs.[1..]
        helper (Array.allPairs preamble preamble)

    let findNumber preambleLength (numbers: int64 []) =
        let rec helper i =
            let r = findPair numbers.[i..i + preambleLength - 1] numbers.[i + preambleLength]
            if Option.isSome r then helper (i + 1) else numbers.[i + preambleLength]
        helper 0


    let day9() =
        System.IO.File.ReadAllLines InputFile
        |> Array.map int64
        |> findNumber 25
