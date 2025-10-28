module SPICE

open System
open System.IO

open Aardvark.Base

open PRo3D.Extensions
open PRo3D.Extensions.FSharp

let init() =
    let d = 
        let logPath = Path.Combine(".", "logs", "CooTrafo.Log")
        Log.line "log path for coo trafo: %s" logPath
        let r = CooTransformation.Init(true, logPath, 10, 10)
        if r <> 0 then failwith "could not initialize CooTransformation lib."
        { new IDisposable with member x.Dispose() = CooTransformation.DeInit() }


    let spiceFileName = Path.GetFullPath(Path.combine [ ".."; ".."; ".."; ".."; "./spice/kernels/mk/hera_ops.tm"])
    let oldDir = System.Environment.CurrentDirectory
    System.Environment.CurrentDirectory <- Path.GetFullPath(Path.GetDirectoryName(spiceFileName))

    if not (File.Exists spiceFileName) then
        failwith "spice kernel does not exist."

    let r = CooTransformation.AddSpiceKernel(Path.GetFullPath(spiceFileName))
    if r <> 0 then failwith "could not add spice kernel"

    d