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

    let decideOnTop (top1: int) (top2: int) (deck1Rest: int list) (deck2Rest: int list) =
        if top1 > top2 then
            (deck1Rest @ [ top1; top2 ], deck2Rest)
        else
            (deck1Rest, deck2Rest @ [ top2; top1 ])

    let rec game ((deck1, deck2): int list * int list) =
        match (deck1, deck2) with
        | _, [] -> deck1
        | [], _ -> deck2
        | _, _ ->
            decideOnTop deck1.[0] deck2.[0] deck1.[1..] deck2.[1..]
            |> game

    let deckScore deck =
        deck
        |> List.rev
        |> List.zip [ 1 .. deck.Length ]
        |> List.sumBy (fun (a, b) -> a * b)

    let day22 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> parse
        |> game
        |> deckScore

    let rec gamePart2 (decks: int list * int list) =
        let mutable states = Set.empty<int list * int list>

        let rec playRounds ((deck1, deck2): int list * int list) =
            if states.Contains(deck1, deck2) then
                (1, deck1)
            else
                states <- Set.add (deck1, deck2) states

                match (deck1, deck2) with
                | _, [] -> (1, deck1)
                | [], _ -> (2, deck2)
                | _, _ ->
                    let (top1, top2) = (deck1.[0], deck2.[0])
                    let (deck1', deck2') = (deck1.[1..], deck2.[1..])

                    if deck1'.Length >= top1 && deck2'.Length >= top2 then
                        let (winner, _) =
                            gamePart2 (List.take top1 deck1', List.take top2 deck2')

                        if winner = 1 then
                            playRounds (deck1' @ [ top1; top2 ], deck2')
                        else
                            playRounds (deck1', deck2' @ [ top2; top1 ])
                    else
                        decideOnTop top1 top2 deck1' deck2' |> playRounds

        playRounds decks

    let day22Part2 () =
        InputFile
        |> System.IO.File.ReadAllLines
        |> parse
        |> gamePart2
        |> snd
        |> deckScore
