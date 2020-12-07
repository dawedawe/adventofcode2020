namespace Adventofcode

module Day5 =

    [<Literal>]
    let InputFile = "Day5Input.txt"

    let getHalf lowerBound upperBound half =
        match half with
        | x when x = 'F' || x = 'L' -> lowerBound, lowerBound + ((upperBound - lowerBound) / 2)
        | x when x = 'B' || x = 'R' -> lowerBound + ((upperBound - lowerBound) / 2) + 1, upperBound
        | _ -> failwith "unsupported partitioning"

    let calculateSeatId (s : string) =
        let partitions = s.ToCharArray()
        let rec helper parts lower upper =
            match parts with
            | [||] -> lower
            | ps -> let lower', upper' = getHalf lower upper ps.[0]
                    helper ps.[1..] lower' upper'
        let row = helper partitions.[0 .. 6] 0 127
        let col = helper partitions.[7 ..] 0 7
        row * 8 + col

    let day5 () =
        System.IO.File.ReadAllLines InputFile
        |> Array.map calculateSeatId
        |> Array.max

    let day5Part2 () =
        let seatIds = System.IO.File.ReadAllLines InputFile
                      |> Array.map calculateSeatId
                      |> Array.sort
        let min, max = seatIds.[0], Array.last seatIds
        seq [min .. max]
        |> Seq.filter (fun id -> not (Array.contains id seatIds))
        |> Seq.head
        