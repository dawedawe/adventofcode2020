namespace Adventofcode

module Day12 =

    [<Literal>]
    let InputFile = "Day12Input.txt"

    type Action =
        | N of int
        | S of int
        | E of int
        | W of int
        | L of int
        | R of int
        | F of int

    type Direction =
        | North
        | South
        | East
        | West

    type Position =
        { NS: int
          WE: int
          Dir: Direction }

    let parse (s: string) =
        let a = s.[0]
        let v = int s.[1..]
        match a with
        | 'N' -> N v
        | 'S' -> S v
        | 'E' -> E v
        | 'W' -> W v
        | 'L' -> L v
        | 'R' -> R v
        | 'F' -> F v
        | _ -> failwith "can't parse"

    let initPosition =
        { NS = 0
          WE = 0
          Dir = East }

    let forward (pos: Position) (v: int) =
        match pos.Dir with
        | North -> { pos with NS = pos.NS + v }
        | South -> { pos with NS = pos.NS - v }
        | East -> { pos with WE = pos.WE + v }
        | West -> { pos with WE = pos.WE - v }

    let turn (act: Action) (pos: Position) =
        let d v = act
        match (pos.Dir, act) with
        | (North, R 90) -> { pos with Dir = East }
        | (North, R 180) -> { pos with Dir = South }
        | (North, R 270) -> { pos with Dir = West }
        | (North, L 90) -> { pos with Dir = West }
        | (North, L 180) -> { pos with Dir = South }
        | (North, L 270) -> { pos with Dir = East }
        | (East, R 90) -> { pos with Dir = South }
        | (East, R 180) -> { pos with Dir = West }
        | (East, R 270) -> { pos with Dir = North }
        | (East, L 90) -> { pos with Dir = North }
        | (East, L 180) -> { pos with Dir = West }
        | (East, L 270) -> { pos with Dir = South }
        | (South, R 90) -> { pos with Dir = West }
        | (South, R 180) -> { pos with Dir = North }
        | (South, R 270) -> { pos with Dir = East }
        | (South, L 90) -> { pos with Dir = East }
        | (South, L 180) -> { pos with Dir = North }
        | (South, L 270) -> { pos with Dir = West }
        | (West, R 90) -> { pos with Dir = North }
        | (West, R 180) -> { pos with Dir = East }
        | (West, R 270) -> { pos with Dir = South }
        | (West, L 90) -> { pos with Dir = South }
        | (West, L 180) -> { pos with Dir = East }
        | (West, L 270) -> { pos with Dir = North }
        | _ -> failwith "unsupported turn"        

    let applyAction act pos =
        match act with
        | N v -> { pos with NS = pos.NS + v }
        | S v -> { pos with NS = pos.NS - v }
        | E v -> { pos with WE = pos.WE + v }
        | W v -> { pos with WE = pos.WE - v }
        | F v -> forward pos v
        | L v -> turn act pos
        | R v -> turn act pos

    let rec run (actions: Action []) (pos: Position) =
        match actions with
        | [||] -> pos
        | _ -> let pos' = applyAction actions.[0] pos
               run actions.[1..] pos'

    let day12() =
        let actions = System.IO.File.ReadAllLines InputFile
                      |> Array.map parse
        let pos = run actions initPosition
        System.Math.Abs pos.NS + System.Math.Abs pos.WE
