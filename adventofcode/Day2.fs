namespace Adventofcode

module Day2 =

    [<Literal>]
    let InputFile = "Day2Input.txt"

    let parseLine s =
        let pattern = @"(\d+)-(\d+) ([a-z]): ([a-z]*)"
        let regex = System.Text.RegularExpressions.Regex(pattern)
        let matches = regex.Match(s)
        let min = int matches.Groups.[1].Value
        let max = int matches.Groups.[2].Value
        let c = matches.Groups.[3].Value.[0]
        let word = matches.Groups.[4].Value
        (min, max, c, word)

    let isValid s =
        let (min, max, c, word) = parseLine s
        word.ToCharArray()
        |> Array.sumBy (fun x -> if x = c then 1 else 0)
        |> fun count -> min <= count && count <= max

    let day2 () =
        System.IO.File.ReadAllLines InputFile
        |> Array.filter isValid
        |> Array.length
        
    let isValidPart2 s =
        let (pos1, pos2, c, word) = parseLine s
        let charArray = word.ToCharArray()
        let pos1ContainsC = if charArray.[pos1 - 1] = c then 1 else 0
        let pos2ContainsC = if charArray.[pos2 - 1] = c then 1 else 0
        (pos1ContainsC ^^^ pos2ContainsC) = 1

    let day2Part2 () =
        System.IO.File.ReadAllLines InputFile
        |> Array.filter isValidPart2
        |> Array.length
        