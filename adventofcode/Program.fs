// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open Adventofcode

[<EntryPoint>]
let main argv =
    let r = Day1.day1Part2 ()
    printfn "%A" r
    0 // return an integer exit code