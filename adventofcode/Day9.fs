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

    let findChunk (x: int64) (numbers: int64 []) (size: int) =
        let rec helper i =
            if i + size > numbers.Length then
                Array.empty
            else
                let chunk = numbers.[i..i + size - 1]
                let sum = Array.sum chunk
                if sum = x then chunk else helper (i + 1)
        helper 0

    let findWeakness (numbers: int64 []) x =
        let rec helper size =
            let r = findChunk x numbers size
            if Array.isEmpty r then helper (size + 1) else r

        let chunk = helper 2 |> Array.sort
        Array.head chunk + Array.last chunk

    let day9Part2() =
        let numbers = System.IO.File.ReadAllLines InputFile |> Array.map int64
        let x = findNumber 25 numbers
        findWeakness numbers x
