namespace Adventofcode

module Day3 =

    [<Literal>]
    let InputFile = "Day3Input.txt"

    let moveToBottom (lines : string []) =
        let rec helper x y trees =
            if y >= Array.length lines - 1
            then trees
            else
                let x' = (x + 3) % lines.[0].Length
                let y' = y + 1
                let trees' = if lines.[y'].[x'] = '#' then trees + 1 else trees
                helper x' y' trees'
        helper 0 0 0

    let day3 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let trees = moveToBottom lines
        trees
