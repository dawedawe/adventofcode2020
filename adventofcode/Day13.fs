namespace Adventofcode

module Day13 =

    [<Literal>]
    let InputFile = "Day13Input.txt"

    let f timestamp bus = bus - (timestamp % bus)

    let day13() =
        let lines = System.IO.File.ReadAllLines InputFile
        let timestamp = int lines.[0]
        let busses = lines.[1]
                     |> fun (s: string) -> s.Split(",")
                     |> Array.filter (fun c -> c <> "x")
                     |> Array.map (int >> (fun b -> b, (f timestamp b)))
                     |> Array.sortBy snd
        fst busses.[0] * snd busses.[0]