namespace Adventofcode

module Day13 =

    open System.Linq

    [<Literal>]
    let InputFile = "Day13Input.txt"

    let f timestamp bus = bus - (timestamp % bus)

    let day13() =
        let lines = System.IO.File.ReadAllLines InputFile
        let timestamp = int lines.[0]

        let busses =
            lines.[1]
            |> fun (s: string) -> s.Split(",")
            |> Array.filter (fun c -> c <> "x")
            |> Array.map (int >> (fun b -> b, (f timestamp b)))
            |> Array.sortBy snd
        fst busses.[0] * snd busses.[0]

    let f2 (t: int64) (busId: int64) (busIndex: int64) =
        let tOfBus = t + busIndex
        (tOfBus % busId) = 0L

    let day13Part2() =
        let input = System.IO.File.ReadAllLines InputFile
                    |> Array.item 1
                    |> fun (s: string) -> s.Split(",")
                    |> List.ofArray
        let mutable busses =
            List.zip input [ 0L .. (int64 input.Length - 1L) ]
            |> List.filter (fun x -> fst x <> "x")
            |> List.map (fun (bus, index) -> (int64 bus, index))
        let mutable timestamp = 0L
        let mutable step = 1L

        while busses.Length > 0 do
            if (f2 timestamp (fst busses.[0]) (snd busses.[0]))
            then step <- step * fst busses.[0]
                 busses <- busses.[1..]
            else timestamp <- timestamp + step
        timestamp
