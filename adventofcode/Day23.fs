namespace Adventofcode

module Day23 =

    open System
    open System.Collections.Generic

    let input = "364289715"

    let getTakeOut (circle: LinkedList<int>) =
        let current = circle.First
        let t1 = current.Next
        let t2 = t1.Next
        let t3 = t2.Next
        [ t1; t2; t3 ]

    let maxWithoutTakeOut (takeOut: int list) (circle: LinkedList<int>) =
        let mutable max = circle.Count
        for t in (List.sortDescending takeOut) do
            if t = max then max <- max - 1
        max

    let findDestination (takeOut: int list) (circle: LinkedList<int>) =
        let destLabel = circle.First.Value - 1
        
        if destLabel > 0 && not (List.contains destLabel takeOut) then
            destLabel
        else
            let firstAlternatives = [(destLabel - 1) .. (-1) .. 1]
            let firstAlt = List.tryFind (fun a -> not (List.contains a takeOut)) firstAlternatives
            if Option.isSome firstAlt
            then firstAlt.Value
            else maxWithoutTakeOut takeOut circle

    let move (circle: LinkedList<int>) =
        let takeOutNodes = getTakeOut circle
        let takeOutValues = takeOutNodes |> List.map (fun n -> n.Value)
        let destination = findDestination takeOutValues circle
        circle.Remove(takeOutNodes.[0]) |> ignore
        circle.Remove(takeOutNodes.[1]) |> ignore
        circle.Remove(takeOutNodes.[2]) |> ignore
        let destinationNode = circle.Find(destination)
        let t0Node = circle.AddAfter(destinationNode, takeOutNodes.[0].Value)
        let t1Node = circle.AddAfter(t0Node, takeOutNodes.[1].Value)
        circle.AddAfter(t1Node, takeOutNodes.[2].Value) |> ignore
        let first = circle.First.Value
        circle.RemoveFirst()
        circle.AddLast(first) |> ignore

    let rec play n (circle: LinkedList<int>) =
        if n = 0 then
            circle
        else
            if n % 1000 = 0 then printfn "%d" n
            move circle
            play (n - 1) circle

    let day23 () =
        let circle =
            seq {
                for c in input do
                    yield Int32.Parse(string c)
            }
            |> LinkedList

        play 100 circle |> ignore
        let circle' = circle |> List.ofSeq
        let index1 = List.findIndex ((=) 1) circle'
        circle'.[index1 + 1..] @ circle'.[0..index1 - 1]

    let day23Part2 () =
        let smallCircle =
            seq {
                for c in input do
                    yield Int32.Parse(string c)
            }
            |> Seq.toList
        let circleToAppend = [(List.max smallCircle + 1) .. 1000000]
        let circle = smallCircle @ circleToAppend |> LinkedList
        play 10000000 circle |> ignore
        let node1 = circle.Find(1)
        let next = int64 node1.Next.Value
        let nextNext = int64 node1.Next.Next.Value
        printfn "%d %d" next nextNext
        next * nextNext
