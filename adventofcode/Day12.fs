namespace Adventofcode

module Day12 =

    open System

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
        | _ ->
            let pos' = applyAction actions.[0] pos
            run actions.[1..] pos'

    let calcManhattan pos =
        Math.Abs pos.NS + Math.Abs pos.WE

    let day12() =
        let actions = System.IO.File.ReadAllLines InputFile |> Array.map parse
        run actions initPosition |> calcManhattan

    let initWayPoint =
        { NS = 1
          WE = 10
          Dir = North }

    let forwardPart2 ship (wayPoint: Position) v =
        let nsMove = wayPoint.NS * v
        let weMove = wayPoint.WE * v

        let ship' =
            { ship with
                  NS = ship.NS + nsMove
                  WE = ship.WE + weMove }
        (ship', wayPoint)

    let calcQuadrant (wayPoint: Position) =
        match (wayPoint.WE, wayPoint.NS) with
        | (x, y) when x >= 0 && y >= 0 -> 1
        | (x, y) when x < 0 && y >= 0 -> 2
        | (x, y) when x < 0 && y < 0 -> 3
        | (x, y) when x >= 0 && y < 0 -> 4
        | _ -> failwith "unsupported quadrant"

    let rec rotate act wayPoint =
        let q = calcQuadrant wayPoint
        let we = Math.Abs wayPoint.WE
        let ns = Math.Abs wayPoint.NS
        match (act, q) with
        | (R 90, 1) ->
            { wayPoint with
                  NS = -1 * we
                  WE = ns }
        | (R 90, 2) ->
            { wayPoint with
                  NS = we
                  WE = ns }
        | (R 90, 3) ->
            { wayPoint with
                  NS = we
                  WE = -1 * ns }
        | (R 90, 4) ->
            { wayPoint with
                  NS = -1 * we
                  WE = -1 * ns }
        | (R 180, 1) ->
            { wayPoint with
                  NS = -1 * ns
                  WE = -1 * we }
        | (R 180, 2) ->
            { wayPoint with
                  NS = -1 * ns
                  WE = we }
        | (R 180, 3) ->
            { wayPoint with
                  NS = ns
                  WE = we }
        | (R 180, 4) ->
            { wayPoint with
                  NS = ns
                  WE = -1 * we }
        | (R 270, 1) ->
            { wayPoint with
                  NS = we
                  WE = -1 * ns }
        | (R 270, 2) ->
            { wayPoint with
                  NS = -1 * we
                  WE = -1 * ns }
        | (R 270, 3) ->
            { wayPoint with
                  NS = -1 * we
                  WE = ns }
        | (R 270, 4) ->
            { wayPoint with
                  NS = we
                  WE = ns }
        | (L 90, _) -> rotate (R 270) wayPoint
        | (L 180, _) -> rotate (R 180) wayPoint
        | (L 270, _) -> rotate (R 90) wayPoint
        | _ -> failwith "unsupported turn"

    let applyActionPart2 act ship wayPoint =
        match act with
        | N v -> (ship, { wayPoint with NS = wayPoint.NS + v })
        | S v -> (ship, { wayPoint with NS = wayPoint.NS - v })
        | E v -> (ship, { wayPoint with WE = wayPoint.WE + v })
        | W v -> (ship, { wayPoint with WE = wayPoint.WE - v })
        | F v -> forwardPart2 ship wayPoint v
        | L _ -> (ship, rotate act wayPoint)
        | R _ -> (ship, rotate act wayPoint)

    let rec runPart2 (actions: Action []) (ship: Position) (wayPoint: Position) =
        match actions with
        | [||] -> ship
        | _ ->
            let (ship', wayPoint') = applyActionPart2 actions.[0] ship wayPoint
            runPart2 actions.[1..] ship' wayPoint'

    let day12Part2() =
        let actions = System.IO.File.ReadAllLines InputFile |> Array.map parse
        let wayPoint = initWayPoint
        runPart2 actions initPosition wayPoint |> calcManhattan
