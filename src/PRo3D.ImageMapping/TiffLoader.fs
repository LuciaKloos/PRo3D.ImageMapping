namespace PRo3D.ImageMapping

open System
open Aardvark.Base
open Aardvark.UI
open Aardvark.UI.Primitives
open Aardvark.Rendering
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open System.IO
open System.Runtime.InteropServices

open HDF.PInvoke

open Aardvark.PixImage.LibTiff
open PRo3D.InstrumentProjection
open PRo3D.InstrumentVisualization
open PRo3D.Core
open PRo3D.SPICE

open System.Text.Json
open System.Collections.Concurrent

open ImageMath
open ImageMetadata
open NetCdfLoader
open NetCdfLoader

open PRo3D.ImageMapping.MbiLoader
open PRo3D.ImageMapping.ImageDefaults

module TiffLoader =

    let  getBandAsFloat
        (bandIndex : int)
        (image : TiffReadResult)
        : float[] =
   
        match image.buffers with
        | PixelBuffers.Float32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.UInt16Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.Int16Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.UInt32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

        | PixelBuffers.Int32Bands bands ->
            bands.[bandIndex]
            |> Array.map float

    let loadBands (texturePath : string) : list<Image> =

        let fullPath = Path.GetFullPath texturePath

        let tiffMbiJson, tiffJson =
            InstrumentMetadata.tryParseMetadataForImagePath fullPath

        let channelCount =
            match tiffJson with
            | Some metadata -> max 1 metadata.channels
            | None -> 1

        let wavelengths =
            let jsonPath = Path.ChangeExtension(texturePath, ".json")

            if File.Exists jsonPath then
                tryReadWavelengthsFromJson jsonPath
                |> Option.defaultValue []
            else
                []

        let dataType =
            match tiffJson with
            | Some metadata ->
                match metadata.data_type.ToLowerInvariant() with
                | "uint16" -> DataType.UInt16
                | "uint32" -> DataType.UInt32
                | "float"  -> DataType.Float
                | _        -> DataType.UInt16
            | None ->
                DataType.UInt16

        let rawMinValues =
            match tiffJson with
            | Some metadata ->
                metadata.image_statistics
                |> Array.map (fun statistics -> statistics.minimum)
                |> Array.toList
            | None ->
                []

        let rawMaxValues =
            match tiffJson with
            | Some metadata ->
                metadata.image_statistics
                |> Array.map (fun statistics -> statistics.maximum)
                |> Array.toList
            | None ->
                []

        // Ensure that these lists always contain one value per channel.
        // ResetCustomMinMax indexes them using selectedChannel.idx.
        let defaultMinValues =
            [
                for channelIndex in 0 .. channelCount - 1 do
                    yield
                        rawMinValues
                        |> List.tryItem channelIndex
                        |> Option.defaultValue 0.0
            ]

        let defaultMaxValues =
            [
                for channelIndex in 0 .. channelCount - 1 do
                    yield
                        rawMaxValues
                        |> List.tryItem channelIndex
                        |> Option.defaultValue 1.0
            ]

        let distance =
            match tiffMbiJson with
            | Some metadata -> metadata.targetPos.Length
            | None -> 0.0

        let time =
            match tiffMbiJson with
            | Some metadata -> metadata.obs_date
            | None -> DateTime.MinValue

        [
            for channelIndex in 0 .. channelCount - 1 do

                let minimum = defaultMinValues[channelIndex]
                let maximum = defaultMaxValues[channelIndex]

                let wavelengthName =
                    wavelengths
                    |> List.tryItem channelIndex
                    |> Option.map (fun wavelength ->
                        sprintf "%.0f nm" wavelength
                    )

                let wavelength =
                    wavelengths
                    |> List.tryItem channelIndex

                let channel =
                    {
                        idx = channelIndex
                        name = wavelengthName
                    }

                let sliderMinimum, sliderMaximum =
                    match dataType with
                    | DataType.Float ->
                        minimum, maximum

                    | DataType.UInt16 ->
                        0.0, 65535.0

                    | DataType.UInt32 ->
                        0.0, float UInt32.MaxValue

                let inputMinimum =
                    {
                        minValue with
                            value = minimum
                            min = sliderMinimum
                            max = sliderMaximum
                    }

                let inputMaximum =
                    {
                        maxValue with
                            value = maximum
                            min = sliderMinimum
                            max = sliderMaximum
                    }

                yield
                    {
                        initial with
                            texture = fullPath
                            selectedChannel = channel
                            bandIndex = channelIndex
                            wavelength = wavelength

                            // This band entry represents exactly one channel.
                            channelOptions = [channel]

                            defaultMinValues = defaultMinValues
                            defaultMaxValues = defaultMaxValues

                            inputMinValue = inputMinimum
                            inputMaxValue = inputMaximum

                            dataType = dataType
                            distance = distance
                            time = time
                    }
        ]
   
    let loadDataset (path : string) : list<Image> =
        match tryResolveNcPathToLoad path with
        | Some ncPath ->
            loadNcBands ncPath

        | None ->
            match tryReadMbiBands path with
            | Some _ ->
                loadMbiBands path

            | None ->
                loadBands path

    let loadFile (texturePath : string) =
        // this could be a fallback
        let ifUsefulThisIsHowToExtractInfos = MultiBandReader.tryGetChannels texturePath

        let (tiffMbiJson, tiffJson) = InstrumentMetadata.tryParseMetadataForImagePath texturePath

        let channels =
            match tiffJson with
            | Some tf -> tf.channels
            | None -> 1

        let channelOptions = [ 0 .. channels - 1 ] |> List.map (fun channel -> {idx = channel; name = None})

        let selectedChannelIdx = 0

        let defaultMinValues = 
            match tiffJson with
            | Some tf -> tf.image_statistics |> Array.toList |> List.map (fun x -> x.minimum)
            | None -> [0.0]

        let defaultMaxValues = 
            match tiffJson with
            | Some tf -> tf.image_statistics |> Array.toList|> List.map (fun x -> x.maximum)
            | None -> [0.0]

        let dataType = 
            match tiffJson with
            | Some tf -> 
                match tf.data_type with
                | "uint16" -> DataType.UInt16
                | "uint32" -> DataType.UInt32
                | "float" -> DataType.Float
                | _ -> DataType.UInt16
            | None -> DataType.UInt16

        let (rangeMin, rangeMax) =
            match dataType with
            | DataType.Float -> (defaultMinValues[selectedChannelIdx], defaultMaxValues[selectedChannelIdx])
            | DataType.UInt16 
            | _ -> (0, 65536)

        let inputMinValue = { minValue with value = defaultMinValues[selectedChannelIdx]; min = rangeMin; max = rangeMax}

        let inputMaxValue = { minValue with value = defaultMaxValues[selectedChannelIdx]; min = rangeMin; max = rangeMax }

        let distance =
            match tiffMbiJson with
            | Some mbi -> mbi.targetPos.Length
            | None -> 0.0

        let time =
            match tiffMbiJson with
            | Some mbi -> mbi.obs_date
            | None -> System.DateTime.MinValue // which default time?

        { initial with
            texture = Path.GetFullPath(texturePath);
            defaultMinValues = defaultMinValues;
            defaultMaxValues = defaultMaxValues;
            inputMinValue = inputMinValue;
            inputMaxValue = inputMaxValue;
            selectedChannel = channelOptions[selectedChannelIdx];
            channelOptions = channelOptions;
            dataType = dataType;
            distance = distance;
            time = time;
        }
