namespace PRo3D.ImageMapping

open FParsec

module BandExpression = 

    type ResolvedWavelength = { requested: float; bandNumber: int; actual: float }

    type BandExpression = 
        | Band of int       // user-facing, 1-based (B6 = sixth band)
        | Wavelength of float 
        | Name of string
        | Const of float
        | Neg of BandExpression
        | Add of BandExpression * BandExpression
        | Sub of BandExpression * BandExpression
        | Mul of BandExpression * BandExpression
        | Div of BandExpression * BandExpression

    let private ws = spaces
    let private sym s = pstring s >>. ws

    let private isIdChar c = isLetter c || isDigit c || c = '_'

    let private parseBand: Parser<BandExpression, unit> =
        attempt (anyOf "Bb" >>. pint32 .>> notFollowedBy (satisfy isIdChar))
        .>> ws |>> Band <?> "band (e.g. B6)"

    let private parseWavelength: Parser<BandExpression, unit> =
        attempt (anyOf "Ww" >>. pfloat .>> notFollowedBy (satisfy isIdChar))
        .>> ws |>> Wavelength <?> "wavelength (e.g. W550)"

    let private parseName: Parser<BandExpression, unit> =
        many1Satisfy2L isLetter isIdChar "band name" .>> ws |>> Name

    // makes ratio computations as B4/2 possible
    let private parseNumber: Parser<BandExpression, unit> = 
        pfloat .>> ws |>> Const <?> "number"

    // handles precedence, associativity, and building the tree
    let private opp = OperatorPrecedenceParser<BandExpression, unit, unit>()

    // gets the pasrer, this will look up the terms and operators when it actually runs
    let private expr = opp.ExpressionParser

    // The Terms (atoms)
    opp.TermParser <- parseBand <|> parseWavelength <|> parseName <|> parseNumber <|> between (sym "(") (sym ")") expr

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


    /// candidates: (0-based logical bandIndex, wavelength)
    let resolveWavelengths (candidates : (int * float) list) (expr : BandExpression)
        : Result<BandExpression * ResolvedWavelength list, string> =

        let candidates =
            candidates |> List.filter (fun (_, wl) -> System.Double.IsFinite wl)

        let nearest (w : float) =
            candidates
            |> List.minBy (fun (idx, wl) -> abs (wl - w), wl, idx)

        let rec go e : BandExpression * ResolvedWavelength list =
            match e with
            | Wavelength w ->
                let idx, wl = nearest w
                let bandNumber = idx + 1 // 0-based -> 1-based
                Band bandNumber, [ { requested = w; bandNumber = bandNumber; actual = wl } ]
            | Band _ | Const _ | Name _ -> e, []
            | Neg a ->
                let a', ra = go a
                Neg a', ra
            | Add (a, b) -> bin Add a b
            | Sub (a, b) -> bin Sub a b
            | Mul (a, b) -> bin Mul a b
            | Div (a, b) -> bin Div a b

        and bin ctor a b =
            let a', ra = go a
            let b', rb = go b
            ctor (a', b'), ra @ rb

        let rec hasWavelength e =
            match e with
            | Wavelength _ -> true
            | Band _ | Const _ | Name _ -> false
            | Neg a -> hasWavelength a
            | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) -> hasWavelength a || hasWavelength b

        if not (hasWavelength expr) then
            Result.Ok (expr, [])
        elif List.isEmpty candidates then
            Result.Error "No wavelength information is available for the loaded bands."
        else
            Result.Ok (go expr) 

    // walks the expression tree and collects every band number that appears in it
    let rec referenceBands expr =
        match expr with
        | Band n -> Set.singleton n // leaf: this band
        | Wavelength _ -> Set.empty // leaf: this wavelength
        | Const _ -> Set.empty      // a number uses no band
        | Name _ -> Set.empty
        | Neg e -> referenceBands e // look at the child
        | Add (a, b) | Sub (a, b) | Mul (a, b) | Div (a, b) -> 
            Set.union (referenceBands a) (referenceBands b) // both sides combined 

    /// names: (0-based bandIndex, custom name). Case-insensitive.
    let resolveNames (names : (int * string) list) (expr : BandExpression) : Result<BandExpression, string> =
        let rec go e =
            match e with
            | Name n ->
                match names |> List.tryFind (fun (_, nm) -> System.String.Equals(nm, n, System.StringComparison.OrdinalIgnoreCase)) with
                | Some (idx, _) -> Result.Ok (Band (idx + 1))
                | None -> Result.Error (sprintf "Unknown band name '%s'." n)
            | Band _ | Wavelength _ | Const _ -> Result.Ok e
            | Neg a -> go a |> Result.map Neg
            | Add (a, b) -> bin Add a b
            | Sub (a, b) -> bin Sub a b
            | Mul (a, b) -> bin Mul a b
            | Div (a, b) -> bin Div a b
        and bin ctor a b =
            go a |> Result.bind (fun a' -> go b |> Result.map (fun b' -> ctor (a', b')))
        go expr

    /// Run BEFORE resolveNames/resolveWavelengths: a renamed band must not be addressed as Bx any more.
    let rejectRenamedBands (names : (int * string) list) (expr : BandExpression) : Result<BandExpression, string> =
        referenceBands expr
        |> Set.toList
        |> List.tryPick (fun n -> names |> List.tryFind (fun (i, _) -> i = n - 1))
        |> function
            | Some (i, nm) -> Result.Error (sprintf "Band B%d has been renamed to '%s'; use that name." (i + 1) nm)
            | None -> Result.Ok expr

    let rec printWith (label : int -> string) expr =
        match expr with
        | Band n -> label (n - 1)
        | Wavelength w -> sprintf "W%g" w
        | Name n -> n
        | Const c -> string c
        | Neg e -> sprintf "-%s" (printWith label e)
        | Add (a, b) -> sprintf "(%s + %s)" (printWith label a) (printWith label b)
        | Sub (a, b) -> sprintf "(%s - %s)" (printWith label a) (printWith label b)
        | Mul (a, b) -> sprintf "(%s * %s)" (printWith label a) (printWith label b)
        | Div (a, b) -> sprintf "(%s / %s)" (printWith label a) (printWith label b)

    /// like printWith, without the outermost parentheses
    let printTop (label : int -> string) expr =
        let s = printWith label expr
        match expr with
        | Add _ | Sub _ | Mul _ | Div _ -> s.Substring(1, s.Length - 2)
        | _ -> s

    let rec print expr =
        match expr with
        | Band n -> sprintf "B%d" n
        | Wavelength w -> sprintf "W%f" w
        | Name n -> n
        | Const c -> string c
        | Neg e -> sprintf "-%s" (print e)
        | Add (a, b) -> sprintf "(%s + %s)" (print a) (print b)
        | Sub (a, b) -> sprintf "(%s - %s)" (print a) (print b)
        | Mul (a, b) -> sprintf "(%s * %s)" (print a) (print b)
        | Div (a, b) -> sprintf "(%s / %s)" (print a) (print b)

    // lower (reduce abstraction) the AST
    // used for syncing text with checkboxes ( only if Bandx/Bandy or Const)
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


    // evaluator. Builds a closure per pixel once, so the tree isn't rewalked per pixel
    let compile (getBand : int -> float[]) (minDenominator : float) (expr : BandExpression) 
        : int -> float =
        let rec go e : int -> float =
            match e with
            | Band n    -> let v = getBand (n - 1) in fun i -> v.[i]
            | Wavelength _ -> failwith "unresolved wavelength"
            | Name _ -> failwith "unresolved band name"
            | Const c   -> fun _ -> c
            | Neg a     -> let fa = go a in fun i -> -(fa i)
            | Add (a, b) -> let fa, fb = go a, go b in fun i -> fa i + fb i
            | Sub (a, b) -> let fa, fb = go a, go b in fun i -> fa i - fb i
            | Mul (a, b) -> let fa, fb = go a, go b in fun i -> fa i * fb i
            | Div (a, b) -> 
                let fa, fb = go a, go b
                fun i -> 
                    let d = fb i
                    if System.Double.IsFinite d && abs d >= minDenominator then fa i / d
                    else nan
        go expr