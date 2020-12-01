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