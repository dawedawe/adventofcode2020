namespace Adventofcode

module Day14 =

    [<Literal>]
    let InputFile = "Day14Input.txt"

    type Instruction =
        | SetMask of string
        | WriteValue of (int64 * int64)

    let parse (s: string) =
        if s.StartsWith("mask") then
            SetMask s.[7..]
        else
            let regex = System.Text.RegularExpressions.Regex(@"mem\[(\d*)\] = (\d*)")
            let matches = regex.Matches(s)
            let addr = int64 matches.[0].Groups.[1].Value
            let value = int64 matches.[0].Groups.[2].Value
            WriteValue(addr, value)

    let getBits value =
        let bits = Array.create 64 0L
        for i in 63 .. -1 .. 0 do
            bits.[i] <- (value >>> (63 - i)) &&& 1L
        bits.[28..]

    let applyMask (mask: string) value =
        let bits = getBits value
        let mutable value' = 0L
        for i in 0 .. (mask.Length - 1)do
            let bitValue = if mask.[i] = 'X'
                           then bits.[i]
                           else System.Int64.Parse(string mask.[i])
            value' <- value' ||| (bitValue <<< (mask.Length - 1 - i))
        value'

    let exec mask space instruction =
        match instruction with
        | SetMask m -> (m, space)
        | WriteValue(addr, value) ->
            let value' = applyMask mask value
            let space' = Map.add addr value' space
            (mask, space')

    let run instructions =
        let rec runHelper prog mask space =
            match prog with
            | [||] -> space
            | _ ->
                let (mask', space') = exec mask space prog.[0]
                runHelper prog.[1..] mask' space'
        runHelper instructions "" Map.empty

    let day14() =
        System.IO.File.ReadAllLines InputFile
        |> Array.map parse
        |> run
        |> Map.toArray
        |> Array.sumBy snd
