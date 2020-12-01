namespace Adventofcode

module Day1 =

    [<Literal>]
    let InputFile = "Day1Input.txt"

    let findEntries entries =
        let rec helper (numbers : int []) =
            if (numbers.Length < 2)
            then failwith "no pair left"
            let n1 = numbers.[0]
            let missing = 2020 - n1
            if Array.contains missing numbers.[1..]
            then (n1, missing)
            else helper numbers.[1..]
        helper entries

    let day1 () =
        let n1, n2 = System.IO.File.ReadAllLines InputFile
                     |> Array.map int
                     |> findEntries
        n1 * n2

    let findEntriesPart2 (entries : int []) =
        let l = entries.Length
        let mutable r = None
        for i in 0 .. l - 3 do
            for j in (i + 1) .. l - 2 do
                for k in (j + 1) .. l - 1 do
                    let sum = entries.[i] + entries.[j] + entries.[k]
                    if sum = 2020
                    then r <- Some (entries.[i], entries.[j], entries.[k])
        if (Option.isNone r)
        then failwith "no triple found"
        r.Value

    let day1Part2 () =
        let a, b, c = System.IO.File.ReadAllLines InputFile
                      |> Array.map int
                      |> findEntriesPart2
        a * b * c
        