namespace Adventofcode

module Day8 =

    [<Literal>]
    let InputFile = "Day8Input.txt"

    type Instruction =
        | Acc of int
        | Jmp of int
        | Nop

    let parse (line: string) =
        let words = line.Split(" ")
        let arg = int words.[1]
        match words.[0] with
        | "acc" -> Acc arg
        | "jmp" -> Jmp arg
        | "nop" -> Nop
        | _ -> failwith "unsupported instruction"

    type State =
        { InstPointer: int
          Accumulator: int }

    let init() =
        { InstPointer = 0
          Accumulator = 0 }

    let acc state arg =
        { InstPointer = state.InstPointer + 1
          Accumulator = state.Accumulator + arg }

    let jmp state arg =
        { state with InstPointer = state.InstPointer + arg }

    let nop state =
        { state with InstPointer = state.InstPointer + 1 }

    let run (code: Instruction []) =
        let rec helper (state: State) (executed: Set<int>) =
            if (executed.Contains state.InstPointer) then
                state.Accumulator
            else
                let executed' = Set.add state.InstPointer executed

                let state' =
                    match code.[state.InstPointer] with
                    | Acc a -> acc state a
                    | Jmp a -> jmp state a
                    | Nop -> nop state
                helper state' executed'

        let initialState = init()
        helper initialState Set.empty

    let day8() =
        let code = System.IO.File.ReadAllLines InputFile |> Array.map parse
        run code
