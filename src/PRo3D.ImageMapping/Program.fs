open PRo3D.ImageMapping

open Aardium
open Aardvark.UI
open System
open System.IO
open System.Globalization
open Suave
open Aardvark.Base
open PRo3D.SPICE

let private tryGetCommandLineValue (name : string) (argv : string[]) =
    let equalsPrefix = name + "="

    let rec loop i =
        if i >= argv.Length then
            None
        else
            let arg = argv.[i]

            if arg.Equals(name, StringComparison.OrdinalIgnoreCase) then
                if i + 1 < argv.Length then
                    Some argv.[i + 1]
                else
                    failwithf "Missing value after command-line argument '%s'." name

            elif arg.StartsWith(equalsPrefix, StringComparison.OrdinalIgnoreCase) then
                Some(arg.Substring(equalsPrefix.Length))

            else
                loop (i + 1)

    loop 0

// reads a float option; falls back to the default when the option is absent
let private getFloatOrDefault (name : string) (defaultValue : float) (argv : string[]) =
    match tryGetCommandLineValue name argv with
    | None ->
        defaultValue
    | Some text ->
        match Double.TryParse(text, NumberStyles.Float, CultureInfo.InvariantCulture) with
        | true, value -> value
        | _ -> failwithf "Invalid number '%s' for command-line argument '%s'." text name

let private getSpiceFileName (argv : string[]) =
    match tryGetCommandLineValue "--spice" argv with
    | Some path when not (String.IsNullOrWhiteSpace path) ->
        Path.GetFullPath path

    | _ ->
        failwith
            "Missing SPICE meta-kernel path. Start the program with: --spice \"C:\\path\\to\\spice\\kernels\\mk\\hera_ops.tm\""

// test mode is active only when --test-image is given
let private tryGetTestConfig (argv : string[]) : Option<TestConfig> =
    match tryGetCommandLineValue "--test-image" argv with
    | None ->
        None

    | Some path when String.IsNullOrWhiteSpace path ->
        failwith "Empty value for --test-image."

    | Some path ->
        let fullPath = Path.GetFullPath path

        if not (File.Exists fullPath) then
            failwithf "Test image not found: %s" fullPath

        // defaults match HighlightAdjustment.init / ShadowAdjustment.init
        Some {
            imagePath       = fullPath
            highlightAmount = getFloatOrDefault "--highlight-amount" 0.0 argv
            highlightTone   = getFloatOrDefault "--highlight-tone"   0.5 argv
            highlightRadius = getFloatOrDefault "--highlight-radius" 30.0 argv
            shadowAmount    = getFloatOrDefault "--shadow-amount"    0.0 argv
            shadowTone      = getFloatOrDefault "--shadow-tone"      0.5 argv
            shadowRadius    = getFloatOrDefault "--shadow-radius"    30.0 argv
            outputPath      =
                tryGetCommandLineValue "--output" argv
                |> Option.map Path.GetFullPath
        }

// check mode is active only when --check-case is given; needs --check-input and --check-output
let private tryGetCheckMode (argv : string[]) =
    match tryGetCommandLineValue "--check-case" argv with
    | None ->
        None

    | Some caseName ->
        let case =
            match ShadowsHighlightsCheck.tryParseCase caseName with
            | Some case -> case
            | None ->
                failwithf "Unknown --check-case '%s'. Use identity, shadows-only or highlights-only." caseName

        let getExistingPath name =
            match tryGetCommandLineValue name argv with
            | Some path when not (String.IsNullOrWhiteSpace path) ->
                let fullPath = Path.GetFullPath path
                if not (File.Exists fullPath) then failwithf "File not found for %s: %s" name fullPath
                fullPath
            | _ ->
                failwithf "--check-case requires %s." name

        Some (case, getExistingPath "--check-input", getExistingPath "--check-output")

// renders the adjusted test image offscreen and saves it, without server or Aardium
let private runHeadlessExport (config : TestConfig) (outputPath : string) =
    Log.warn "Headless export: %s -> %s" config.imagePath outputPath
    use app = new Aardvark.Application.Slim.OpenGlApplication()
    Image.saveAdjustedImage app.Runtime config outputPath
    Log.warn "Saved %s" outputPath

[<EntryPoint>]
let main args =
    Aardvark.Init()
    Aardium.init()

    let testConfig = tryGetTestConfig args
    match tryGetCheckMode args with
    | Some (case, inputPath, outputPath) ->
        ShadowsHighlightsCheck.run case inputPath outputPath

    | None ->

    if args |> Array.exists (fun a -> a.Equals("--synthetic-tests", StringComparison.OrdinalIgnoreCase)) then
        let outputDir =
            tryGetCommandLineValue "--synthetic-output" args
            |> Option.defaultValue "tests/shadows-highlights/output/synthetic"
            |> Path.GetFullPath

        use app = new Aardvark.Application.Slim.OpenGlApplication()
        SyntheticTests.run app.Runtime outputDir

    else

    // resolve relative paths before SPICE.init, which changes the working directory
    let testConfig = tryGetTestConfig args

    let spiceFileName = getSpiceFileName args
    use _ = SPICE.init spiceFileName

    match testConfig with
    | Some ({ outputPath = Some outputPath } as t) ->
        runHeadlessExport t outputPath
        0

    | _ ->

    match testConfig with
    | Some t -> Log.warn "Test mode: loading %s" t.imagePath
    | None -> ()

    Aardium.init()

    // create the opengl application for rendering
    let app = new Aardvark.Application.Slim.OpenGlApplication()
    // create the media application
    let mediaApp = App.app app.Runtime testConfig

    WebPart.startServerLocalhost 4321 [
        MutableApp.toWebPart' app.Runtime false (App.start mediaApp)
    ] |> ignore

    Aardium.run {
        title "PRo3D.ImageMapping Tool"
        width 1024
        height 768
        debug true
        url "http://localhost:4321/"
    }

    0