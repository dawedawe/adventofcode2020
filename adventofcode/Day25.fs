namespace Adventofcode

module Day25 =

    let pubKey1 = 11239946L
    let pubKey2 = 10464955L
    let subjectNumber = 7L

    let transformTillPubKey pubKey subNr =
        let mutable value = 1L

        seq {
            for i in Seq.initInfinite id do
                value <- (value * subNr) % 20201227L
                if value = pubKey then yield (i + 1)
        }
        |> Seq.head

    let transform loopSize subNr =
        let rec helper value i =
            if i = 0 then
                value
            else
                helper ((value * subNr) % 20201227L) (i - 1)

        helper 1L loopSize

    let day25 () =
        let loopSize1 =
            transformTillPubKey pubKey1 subjectNumber

        let loopSize2 =
            transformTillPubKey pubKey2 subjectNumber

        let secret1 = transform loopSize1 pubKey2
        let secret2 = transform loopSize2 pubKey1
        secret1, secret2
