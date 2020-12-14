namespace Adventofcode

module Day14 =

    open System
    open System.Collections.Generic
    open System.Linq

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
        for i in 0 .. (mask.Length - 1) do
            let bitValue =
                if mask.[i] = 'X' then bits.[i] else System.Int64.Parse(string mask.[i])
            value' <- value' ||| (bitValue <<< (mask.Length - 1 - i))
        value'

    let exec mask (space: Dictionary<int64, int64>) instruction =
        match instruction with
        | SetMask m -> (m, space)
        | WriteValue(addr, value) ->
            let value' = applyMask mask value
            space.[addr] <- value'
            (mask, space)

    let run execFunc instructions =
        let rec runHelper prog mask space =
            match prog with
            | [||] -> space
            | _ ->
                let (mask', space') = execFunc mask space prog.[0]
                runHelper prog.[1..] mask' space'
        runHelper instructions "" (Dictionary<int64, int64>())

    let day14() =
        System.IO.File.ReadAllLines InputFile
        |> Array.map parse
        |> run exec
        |> fun d -> d.Values.Sum()

    let getFloatingAddress (mask: string) (addr: int64) =
        let addrBits = getBits addr
        let zipped = Array.zip addrBits (mask.ToCharArray())

        let floatMask =
            Array.map (fun (a, m) ->
                if m = '0' then string a
                else if m = '1' then "1"
                else "X") zipped
        floatMask

    let getBitCombinations bitCount =
        let combinations = System.Math.Pow(2.0, float bitCount) |> int
        seq {
            for i in 0 .. combinations - 1 do
                let bits = getBits (int64 i)
                yield bits.[(bits.Length - bitCount)..]
        }

    let toNumber (a: int64 []) =
        let mutable value = 0L
        for i in (a.Length - 1) .. (-1) .. 0 do
            let incr = Math.Pow(2.0, float i) |> int64
            value <- value + a.[i] * incr
        value

    let distributeBits (floatingAddress: string []) (bitsToMap: int64 []) =
        let addr = Array.copy floatingAddress
        let mutable i = 0
        for _ in bitsToMap do
            let index = Array.IndexOf(addr, "X")
            addr.[index] <- string bitsToMap.[i]
            i <- i + 1
        addr |> Array.map int64

    let getAddresses (floatingAddress: string []) =
        let floats =
            Array.sumBy (fun x ->
                if x = "X" then 1 else 0) floatingAddress

        let bitsToMap = getBitCombinations floats |> Seq.toArray
        seq {
            for c in 0 .. (bitsToMap.Length - 1) do
                let realAddr = distributeBits floatingAddress bitsToMap.[c]
                yield (toNumber realAddr)
        }

    let execPart2 mask (space: Dictionary<int64, int64>) instruction =
        match instruction with
        | SetMask m -> (m, space)
        | WriteValue(addr, value) ->
            let floatingAddress = getFloatingAddress mask addr
            let addresses = getAddresses floatingAddress
            for a in addresses do
                space.[a] <- value
            (mask, space)

    let day14Part2() =
        System.IO.File.ReadAllLines InputFile
        |> Array.map parse
        |> run execPart2
        |> fun d -> d.Values.Sum()
