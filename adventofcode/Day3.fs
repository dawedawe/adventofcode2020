namespace Adventofcode

module Day3 =

    [<Literal>]
    let InputFile = "Day3Input.txt"

    let moveToBottom (lines : string []) (right, down) =
        let rec helper x y trees =
            if y >= Array.length lines - 1
            then trees
            else
                let x' = (x + right) % lines.[0].Length
                let y' = y + down
                let trees' = if lines.[y'].[x'] = '#' then trees + 1L else trees
                helper x' y' trees'
        helper 0 0 0L

    let day3 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let trees = moveToBottom lines (3, 1)
        trees

    let day3Part2 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let slopes = [ (1, 1); (3, 1); (5, 1); (7, 1); (1, 2) ]
        let treeCounts = List.map (moveToBottom lines) slopes
        List.fold (*) 1L treeCounts