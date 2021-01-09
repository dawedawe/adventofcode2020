namespace Adventofcode

module Day23 =

    open System

    let input = "364289715"

    let splitCircle (currentIndex: int) (circle: int list) =
        let takeOut =
            [ circle.[(currentIndex + 1) % circle.Length]
              circle.[(currentIndex + 2) % circle.Length]
              circle.[(currentIndex + 3) % circle.Length] ]

        let circle' =
            List.filter (fun i -> not (List.contains i takeOut)) circle

        (circle', takeOut)

    let findDestination (current: int) (circle: int list) =
        let destLabel = current - 1

        if List.contains destLabel circle then
            destLabel
        else
            let firstAlternatives =
                seq { (destLabel - 1) .. (-1) .. 1 } |> Seq.toList

            let s =
                seq {
                    for a in firstAlternatives do
                        if List.contains a circle then yield a
                }

            if not (Seq.isEmpty s) then
                Seq.head s
            else
                List.max circle

    let move (currentIndex: int) (circle: int list) =
        let (circle', takeOut) = splitCircle currentIndex circle

        let destination =
            findDestination circle.[currentIndex] circle'

        let destinationIndex = List.findIndex ((=) destination) circle'

        let circle'' =
            circle'.[0..destinationIndex]
            @ takeOut @ circle'.[destinationIndex + 1..]

        let currentIndex' =
            (List.findIndex ((=) circle.[currentIndex]) circle''
             + 1) % circle''.Length

        (circle'', currentIndex')

    let rec play n currentIndex circle =
        if n = 0 then
            circle
        else
            let circle', currentIndex' = move currentIndex circle
            play (n - 1) currentIndex' circle'

    let day23 () =
        let circle =
            seq {
                for c in input do
                    yield Int32.Parse(string c)
            }
            |> Seq.toList

        let circle' = play 100 0 circle
        let index1 = List.findIndex ((=) 1) circle'
        circle'.[index1 + 1..] @ circle'.[0..index1 - 1]
