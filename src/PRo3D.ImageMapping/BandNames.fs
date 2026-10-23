namespace PRo3D.ImageMapping

module BandNames = 
    let private identifier = System.Text.RegularExpressions.Regex(@"^[A-Za-z][A-Za-z0-9_]*$")
    let private reserved   = System.Text.RegularExpressions.Regex(@"^[BbWw]\d+$")

    let tryFind (names : list<int * string>) idx =
        names |> List.tryFind (fun (i, _) -> i = idx) |> Option.map snd

    /// label in the band list
    let label names idx = 
        tryFind names idx |> Option.defaultValue (sprintf "Band %d" (idx + 1))

    /// label inside expressions
    let exprLabel names idx =
        tryFind names idx |> Option.defaultValue (sprintf "B%d" (idx + 1))

    let reset idx (names : list<int * string>) =
        names |> List.filter (fun (i, _) -> i <> idx)

    let tryRename (names : list<int * string>) idx (raw : string) =
        let name    = raw.Trim().Replace(' ', '_')
        let without = reset idx names
        if name = "" then Result.Ok without
        elif not (identifier.IsMatch name) then
            Result.Error "Names must start with a letter and contain only letters, digits and '_'."
        elif reserved.IsMatch name then
            Result.Error "Names like B6 or W550 are reserved for bands and wavelengths."
        elif without |> List.exists (fun (_, n) -> System.String.Equals(n, name, System.StringComparison.OrdinalIgnoreCase)) then
            Result.Error "This name is already used by another band."
        else Result.Ok ((idx, name) :: without)
