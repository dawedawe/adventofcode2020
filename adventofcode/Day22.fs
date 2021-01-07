namespace Adventofcode

module Day22 =

    [<Literal>]
    let InputFile = "Day22Input.txt"

    let parse (lines: string []) =
        let player1 =
            lines |> Array.takeWhile (fun s -> s <> "")

        let player2 = lines.[player1.Length + 1..]

        let deck1 =
            player1.[1..] |> Array.map int |> List.ofArray

        let deck2 =
            player2.[1..] |> Array.map int |> List.ofArray

        (deck1, deck2)

    let play (deck1: int list) (deck2: int list) =
        let (top1, top2) = (deck1.[0], deck2.[0])

        if top1 > top2 then
            (deck1.[1..] @ [ top1; top2 ], deck2.[1..])
        else
            (deck1.[1..], deck2.[1..] @ [ top2; top1 ])

    let rec game ((deck1, deck2): int list * int list) =
        match (deck1, deck2) with
        | _, [] -> deck1
        | [], _ -> deck2
        | _, _ -> play deck1 deck2 |> game

    let day22 () =
        let lines = System.IO.File.ReadAllLines InputFile
        let (deck1, deck2) = parse lines
        let winner = game (deck1, deck2)
        let indexes = [1 .. winner.Length]
        winner |> List.rev |> List.zip indexes |> List.sumBy (fun (a, b) -> a * b)
