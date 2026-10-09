namespace PRo3D.ImageMapping

open FParsec

module BandExpression = 

    type BandExpression = 
        | Band of int       // user-facing, 1-based (B6 = sixth band)
        | Const of float
        | Neg of BandExpression
        | Add of BandExpression * BandExpression
        | Sub of BandExpression * BandExpression
        | Mul of BandExpression * BandExpression
        | Div of BandExpression * BandExpression

    let private ws = spaces
    let private sym s = pstring s >>. ws

    let private parseBand: Parser<BandExpression, unit> = 
        ( pchar 'B' <|> pchar 'b' ) >>. pint32 .>> ws |>> Band <?> "band (e.g. B6)"

    let private parseNumber: Parser<BandExpression, unit> = 
        pfloat .>> ws |>> Const <?> "number"

    // handles precedence, associativity, and building the tree
    let private opp = OperatorPrecedenceParser<BandExpression, unit, unit>()

    // gets the pasrer, this will look up the terms and operators when it actually runs
    let private expr = opp.ExpressionParser

    // The Terms (atoms)
    // fails on anything that does not start with a band, number, or opening paren
    opp.TermParser <- parseBand <|> parseNumber <|> between (sym "(") (sym ")") expr

    opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun a b -> Add(a, b)))
    opp.AddOperator(InfixOperator("-", ws, 1, Associativity.Left, fun a b -> Sub(a, b)))
    opp.AddOperator(InfixOperator("*", ws, 2, Associativity.Left, fun a b -> Mul(a, b)))
    opp.AddOperator(InfixOperator("/", ws, 2, Associativity.Left, fun a b -> Div(a, b)))

    // eof required to ensure that the entire input is consumed
    let private fullExpr = ws >>. expr .>> eof

    let parse (input : string) : Result<BandExpression, string> =
        match run fullExpr input with
        | Success(result, _, _) -> Result.Ok result
        | Failure(errorMsg, _, _) -> Result.Error errorMsg

    // walks the expression tree and collects every band number that appears in it
    let rec referenceBands expr =
        match expr with
        | Band n -> Set.singleton n // leaf: this band
        | Const _ -> Set.empty      // a number uses no band
        | Neg e -> referenceBands e // look at the child
        | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) -> 
            Set.union (referenceBands a) (referenceBands b) // both sides combined 

    let rec print expr =
        match expr with
        | Band n -> sprintf "B%d" n
        | Const c -> string c
        | Neg e -> sprintf "-%s" (print e)
        | Add (a, b) -> sprintf "(%s + %s)" (print a) (print b)
        | Sub (a, b) -> sprintf "(%s - %s)" (print a) (print b)
        | Mul (a, b) -> sprintf "(%s * %s)" (print a) (print b)
        | Div (a, b) -> sprintf "(%s / %s)" (print a) (print b)

    // lower (reduce abstraction) the AST
    let toRatio (availableBandIndices : Set<int>) (expr : BandExpression)
        : Result<int * Option<int>, string> =

        let resolve n = 
            let idx = n - 1 // convert to 0-based
            if availableBandIndices.Contains idx then
                Result.Ok idx
            else
                Result.Error (sprintf "Band B%d is not available." n)

        match expr with 
        | Band n -> resolve n |> Result.map (fun idx -> (idx, None))
        | Div (Band n, Band m) -> 
            match resolve n, resolve m with
            | Result.Ok idxN, Result.Ok idxM -> Result.Ok (idxN, Some idxM)
            | Result.Error e, _ | _, Result.Error e -> Result.Error e
        | _ -> Result.Error "Only 'Bx' or 'Bx/By' is supported in this mode"

    /// Checks that every band used in the expression is loaded.
    /// availableBandIndices are 0-based (Image.bandIndex); the expression uses 1-based numbers.
    let validateBands (availableBandIndices : Set<int>) (expr : BandExpression)
        : Result<BandExpression, string> =

        let missing =
            referenceBands expr
            |> Set.filter (fun n -> not (availableBandIndices.Contains (n - 1)))

        if Set.isEmpty missing then
            Result.Ok expr
        else
            let missingText =
                missing
                |> Seq.map (sprintf "B%d")
                |> String.concat ", "

            let availableText =
                if Set.isEmpty availableBandIndices then
                    "no bands are loaded"
                else
                    sprintf "available: B%d–B%d"
                        (Set.minElement availableBandIndices + 1)
                        (Set.maxElement availableBandIndices + 1)

            Result.Error (sprintf "Band(s) not loaded: %s (%s)" missingText availableText)