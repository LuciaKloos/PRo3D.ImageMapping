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
    let private sym s = pstring >>. ws

    let private parseBand: Parser<BandExpression, unit> = 
        ( pchar 'B' <|> pchar 'b' ) >>. pint32 .>> ws |>> Band <?> "band (e.g. B6)"

    let private parseNumber: Parser<BandExpression, unit> = 
        pfloat .>> ws |>> Const <?> "number"

    // handles precedence, associativity, and building the tree
    let private opp = OperatorPredecenceParser<BandExpression, unit, unit>()

    // gets the pasrer, this will look up the terms and operators when it actually runs
    let private expr = opp.ExpressionParser

    // The Terms (atoms)
    // fails on anything that does not start with a band, number, or opening paren
    opp.TermParser <- parseBand <|> parseNumber <|> between (sym "(") (sym ")") expr

    opp.AddOperator(InfixOperator("+", ws, 1, Associativity.Left, fun a b -> Add(a, b)))