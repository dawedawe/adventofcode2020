namespace Adventofcode

module Day18 =

    open System
    open System.Collections.Generic

    [<Literal>]
    let InputFile = "Day18Input.txt"

    type Token =
        | Plus
        | Mul
        | X of int64
        | LeftBracket
        | RightBracket

    let parse (line: string) =
        let rec parseHelper (rest: char list) (sofar: Token list) =
            match rest with
            | [] -> sofar
            | ' ' :: r -> parseHelper r sofar
            | '+' :: r -> parseHelper r (sofar @ [ Plus ])
            | '*' :: r -> parseHelper r (sofar @ [ Mul ])
            | '(' :: r -> parseHelper r (sofar @ [ LeftBracket ])
            | ')' :: r -> parseHelper r (sofar @ [ RightBracket ])
            | x :: r when Char.IsDigit x -> parseHelper r (sofar @ [ X(Int64.Parse(string x)) ])
            | _ -> failwith "unsupported input"

        let l = line.ToCharArray() |> List.ofArray
        parseHelper l List.empty

    let popBracketContent (stack: Stack<Token>) =
        let mutable tokens = List.empty

        let rec helper () =
            let t = stack.Pop()
            match t with
            | RightBracket -> helper ()
            | LeftBracket -> tokens
            | _ ->
                tokens <- [ t ] @ tokens
                helper ()

        helper ()

    let popWholeStack (stack: Stack<Token>) =
        let mutable tokens = List.empty

        let rec helper () =
            let t = stack.Pop()
            match t with
            | RightBracket -> failwith "bad stack"
            | LeftBracket -> failwith "bad stack"
            | _ ->
                tokens <- [ t ] @ tokens
                if stack.Count > 0 then helper () else tokens

        helper ()

    let rec evalTokens (tokens: Token list) =
        match tokens with
        | (X x1) :: op :: (X x2) :: rest ->
            let f = if op = Plus then (+) else (*)
            let result = f x1 x2
            evalTokens ([ X result ] @ tokens.[3..])
        | [ X x ] -> X x
        | _ -> failwith "bad state"

    let evaluateStacks (evalStack: Stack<Token>) (parseStack: Stack<Token>) =
        let tokens =
            if parseStack.Peek() = RightBracket then popBracketContent parseStack else popWholeStack parseStack

        let result = evalTokens tokens
        if parseStack.Count > 0 then parseStack.Push(result) else evalStack.Push(result)

    let evaluateStack (evalStack: Stack<Token>) = popWholeStack evalStack |> evalTokens

    let eval (tokens: Token list) =
        let evalStack = Stack<Token>()
        let parseStack = Stack<Token>()

        let rec evalHelper rest =
            match rest with
            | [] when parseStack.Count = 0 ->
                match (evaluateStack evalStack) with
                | X x -> x
                | _ -> failwith "bad state"
            | (X x) :: r ->
                if (parseStack.Count > 0) then
                    parseStack.Push(X x)
                    evalHelper r
                else
                    evalStack.Push(X x)
                    evalHelper r
            | op :: r when op = Mul || op = Plus ->
                if (parseStack.Count > 0) then parseStack.Push(op) else evalStack.Push(op)
                evalHelper r
            | LeftBracket :: r ->
                parseStack.Push(LeftBracket)
                evalHelper r
            | RightBracket :: r ->
                parseStack.Push(RightBracket)
                evaluateStacks evalStack parseStack
                evalHelper r
            | _ -> failwith "bad state"

        evalHelper tokens

    let day18 () =
        System.IO.File.ReadAllLines InputFile
        |> Array.map parse
        |> Array.sumBy eval
