open Expecto
open PRo3D.Core

let tests =
    test "A simple test" {
        let instrumentImages = 
            InstrumentMetadata.discoverInstrumentFolder @"C:\pro3ddata\HERA\Workshop2\EOX_PRo3D-GIS_Data\TIFF\Mars-Swing-By\Mars-Swing-By\HSH-1B\1B"
            |> Seq.toArray
        Expect.isNonEmpty instrumentImages "Instrument images should not be empty"
    }

[<EntryPoint>]
let main args =
    runTestsWithCLIArgs [] args tests