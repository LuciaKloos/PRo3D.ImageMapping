namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI.Primitives
open PRo3D.ImageMapping.Model

open System.IO

open PRo3D.InstrumentProjection
open PRo3D.Core

open System.Text.Json

open PRo3D.ImageMapping.ImageMetadata
open PRo3D.ImageMapping.ImageDefaults

module MbiLoader = 

    let tryReadMbiBands (mbiPath : string) : Option<list<MbiBandInfo>> =
        try
            let fullMbiPath = Path.GetFullPath mbiPath
            let baseDirectory = Path.GetDirectoryName fullMbiPath

            use document =
                JsonDocument.Parse(File.ReadAllText fullMbiPath)

            match tryGetProperty "mbi_bands" document.RootElement with
            | Some bandsElement when bandsElement.ValueKind = JsonValueKind.Array ->

                bandsElement.EnumerateArray()
                |> Seq.choose (fun bandElement ->
                    match tryGetInt "index" bandElement, tryGetString "file_path" bandElement with
                    | Some index, Some relativePath ->

                        let resolvedPath =
                            if Path.IsPathRooted relativePath then
                                relativePath
                            else
                                Path.Combine(baseDirectory, relativePath)

                        Some
                            {
                                index = index
                                filePath = Path.GetFullPath resolvedPath
                                label = tryGetString "label" bandElement
                                wavelength = tryGetDouble "wavelength" bandElement
                                exposure = tryGetDouble "exposure" bandElement
                            }

                    | _ ->
                        None
                )
                |> Seq.sortBy (fun band -> band.index)
                |> Seq.toList
                |> function
                    | [] -> None
                    | bands -> Some bands

            | _ ->
                None

        with error ->
            Log.warn "Could not read MBI manifest %s: %s" mbiPath error.Message
            None

    let tryReadWavelengthsFromJson (jsonPath : string) : Option<list<float>> =
        try
            use document =
                JsonDocument.Parse(File.ReadAllText jsonPath)

            let mutable wavelengthsElement =
                Unchecked.defaultof<JsonElement>

            if
                document.RootElement.TryGetProperty(
                    "wavelengths",
                    &wavelengthsElement
                )
                && wavelengthsElement.ValueKind = JsonValueKind.Array
            then
                wavelengthsElement.EnumerateArray()
                |> Seq.map (fun value -> value.GetDouble())
                |> Seq.toList
                |> Some
            else
                None

        with error ->
            Log.warn "Could not read wavelengths from %s: %s"
                jsonPath
                error.Message

            None

    let dataTypeFromMetadata (path : string) : DataType =
        let _, tiffJson = InstrumentMetadata.tryParseMetadataForImagePath path
        match tiffJson with
        | Some meta ->
            match meta.data_type.ToLowerInvariant() with
            | "uint16" -> DataType.UInt16
            | "uint32" -> DataType.UInt32
            | "float" | "float32" | "double" | "float64" -> DataType.Float
            | _ -> DataType.Float
        | None ->
            DataType.Float

    let sliderRangeForDataType (dataType : DataType) (minimum : float) (maximum : float) : float * float =
        match dataType with
        | DataType.Float ->
            minimum, maximum
        | DataType.UInt16 ->
            0.0, 65535.0
        | DataType.UInt32 ->
            0.0, float UInt32.MaxValue
            
    let createBandImage
        (texturePath : string)
        (logicalBandIndex : int)
        (channel : Channel)
        (wavelength : Option<float>)
        (dataType : DataType)
        (defaultMin : float)
        (defaultMax : float)
        (minimum : float)
        (maximum : float)
        (distance : float)
        (time : DateTime)
        : Image =

        let sliderMinimum, sliderMaximum =
            sliderRangeForDataType dataType minimum maximum
        {
            initial with
                texture = texturePath
                bandIndex = logicalBandIndex
                wavelength = wavelength
                selectedChannel = channel
                channelOptions = [ channel ]
                defaultMinValues = [ defaultMin ]
                defaultMaxValues = [ defaultMax ]
                inputMinValue =
                    {
                        minValue with
                            value = minimum
                            min = sliderMinimum
                            max = sliderMaximum
                    }
                inputMaxValue =
                    {
                        maxValue with
                            value = maximum
                            min = sliderMinimum
                            max = sliderMaximum
                    }
                dataType = dataType
                distance = distance
                time = time
        }

    let loadMbiBands (mbiPath : string) : list<Image> =
            match tryReadMbiBands mbiPath with
            | None ->
                []

            | Some mbiBands ->

                [
                    for band in mbiBands do
                        if File.Exists band.filePath then

                            
                            let _, tiffJson =
                                InstrumentMetadata.tryParseMetadataForImagePath band.filePath

                            let dataType = dataTypeFromMetadata band.filePath

                            let minimum, maximum =
                                match tiffJson with
                                | Some metadata when metadata.image_statistics.Length > 0 ->
                                    metadata.image_statistics.[0].minimum,
                                    metadata.image_statistics.[0].maximum
                                | _ ->
                                    0.0, 1.0

                            let bandName =
                                match band.label, band.wavelength with
                                | Some label, Some wavelength ->
                                    Some (sprintf "%s / %.0f nm" label wavelength)
                                | None, Some wavelength ->
                                    Some (sprintf "%.0f nm" wavelength)
                                | Some label, None ->
                                    Some label
                                | None, None ->
                                    Some (sprintf "Band %d" band.index)

                            let channel =
                                {
                                    idx = 0
                                    name = bandName
                                }

                            createBandImage
                                band.filePath
                                band.index
                                channel
                                band.wavelength
                                dataType
                                minimum
                                maximum
                                minimum
                                maximum
                                0.0
                                (DateTime())

                        else
                            Log.warn "MBI band file does not exist: %s" band.filePath
                ]
