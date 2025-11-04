module TransformationTests

open System.Globalization

#nowarn "9"

open System
open System.IO

open Expecto

open FSharp.NativeInterop

open Aardvark.Base

open PRo3D.Extensions
open PRo3D.Extensions.FSharp

open PRo3D.Core
open PRo3D.SPICE

let logDir = Path.Combine(".", "logs")
let spiceRoot = Path.combine [ ".."; ".."; ".."; ".."; ".."]
let spiceFileName = Path.GetFullPath(Path.combine [ spiceRoot; "./spice/kernels/mk/hera_ops.tm"])

do Aardvark.Base.Aardvark.UnpackNativeDependencies(typeof<CooTransformation.RelState>.Assembly)

let init () =
    if not (Directory.Exists(logDir)) then 
        Directory.CreateDirectory(logDir) |> ignore

    let r = CooTransformation.Init(true, Path.Combine(logDir, "CooTrafo.log"), 4, 4)
    if r <> 0 then failwith "init failed."
    { new IDisposable with member x.Dispose() = CooTransformation.DeInit()}


let transformationTests () = 
    testSequenced <| testList "init" [
        test "InitDeInit" {
            let i = init()
            i.Dispose()
        }

        use _ = init()
        System.Environment.CurrentDirectory <- Path.GetDirectoryName(spiceFileName)
        let init = CooTransformation.AddSpiceKernel(spiceFileName)
        Expect.equal 0 init "spice adding"

        let images =
            [
                @"C:\pro3ddata\HERA\Workshop2\EOX_PRo3D-GIS_Data\TIFF\Mars-Swing-By\Mars-Swing-By\AFC1-1B\1B\AF1_0CO6TA_250311T025412_1B_AFC1.tif"
                @"C:\pro3ddata\HERA\Workshop2\EOX_PRo3D-GIS_Data\TIFF\Mars-Swing-By\Mars-Swing-By\HSH-1B\1B\HSH_0CR7B2_250312T062000_1B_Stacked.tif"
                @"C:\pro3ddata\HERA\20250314\HSH_converted\HSH_converted\HSH_0CRQSV_250312T115349_1A.tif"
            ]

        for imgPath in images do
            test $"Target vectors align with metadata {imgPath}" {
                let (mbi,tif) = InstrumentMetadata.tryParseMetadataForImagePath imgPath
                Expect.isSome mbi "MBI metadata cannot be parsed"
                Expect.isSome tif "TIF metacata cannot be parsed"
                let mbi = mbi |> Option.get
                let tif = tif |> Option.get

                let posEarth = CooTransformation.getRelState "EARTH" "SUN" "HERA" mbi.obs_date "ECLIPJ2000" |> Option.get
                let metaVsComputed = Vec.distance posEarth.pos (mbi.earthPos * 1000.0)

                let posEarth2 = CooTransformation.getRelState "EARTH" "SUN" "HERA" mbi.obs_date "J2000" |> Option.get
                let metaVsComputed2 = Vec.distance posEarth2.pos (mbi.earthPos * 1000.0)

                let posEarth3 = CooTransformation.getRelState "EARTH" "SUN" "HERA" mbi.obs_date "HERA_AFC-1" |> Option.get
                let metaVsComputed3 = Vec.distance posEarth3.pos (mbi.earthPos * 1000.0)

                let posEarth4 = CooTransformation.getRelState "EARTH" "SUN" "HERA" mbi.obs_date "HERA_SPACECRAFT" |> Option.get
                let metaVsComputed4 = Vec.distance posEarth4.pos (mbi.earthPos * 1000.0)

                let posSun = CooTransformation.getRelState "SUN" "SUN" "HERA" mbi.obs_date "J2000" |> Option.get
                let sunDif = Vec.distance posSun.pos (mbi.sunPos * 1000.0)
            
                //let cameraView = InstrumentProjection.getLookAt "HERA" "HERA" "HERA_AFC-1" "SUN" mbi.obs_date

                printfn "%A" metaVsComputed
                ()
            }
    ]
