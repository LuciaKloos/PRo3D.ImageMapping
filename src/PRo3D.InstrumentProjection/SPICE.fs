#if INTERACTIVE
#r "nuget: PRo3D.SPICE"
#else
#endif

namespace PRo3D.SPICE

module SPICE =

    open System
    open System.IO

    open Aardvark.Base

    open PRo3D.Extensions
    open PRo3D.Extensions.FSharp

    let init(spiceFileName : string) =
        let d = 
            let logPath = Path.Combine(".", "logs", "CooTrafo.Log")
            Log.line "log path for coo trafo: %s" logPath
            let r = CooTransformation.Init(true, logPath, 10, 10)
            if r <> 0 then failwith "could not initialize CooTransformation lib."
            { new IDisposable with member x.Dispose() = CooTransformation.DeInit() }
            

        // let spiceFileName = Path.GetFullPath(Path.combine [ ".."; ".."; ".."; ".."; "./spice/kernels/mk/hera_ops.tm"])
        let spiceFileName = Path.GetFullPath spiceFileName
        let oldDir = System.Environment.CurrentDirectory

        if not (File.Exists spiceFileName) then
            failwithf "spice meta-kernel does not exist: %s" spiceFileName

        let spiceDirectory =
            let dir = Path.GetDirectoryName spiceFileName

            if String.IsNullOrWhiteSpace dir then
                failwithf "could not determine spice directory for file: %s" spiceFileName

            Path.GetFullPath dir

        System.Environment.CurrentDirectory <- spiceDirectory

        Log.line "using SPICE meta-kernel: %s" spiceFileName
        Log.line "SPICE working directory: %s" Environment.CurrentDirectory


        let r = CooTransformation.AddSpiceKernel spiceFileName
        if r <> 0 then failwith "could not add spice kernel"

        d