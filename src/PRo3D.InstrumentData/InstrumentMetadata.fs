#if INTERACTIVE
#r "nuget: FSharp.Data"
#r "nuget: Aardvark.Base"
#else
module PRo3D.Core.InstrumentMetadata
#endif

open System.IO
open Aardvark.Base

module Tiff_Json = 
    open System
    open System.Text.Json
    open System.Text.Json.Serialization

    type ImageStatistics = {
        minimum: float
        maximum: float
        mean: float
        median: float
        standard_deviation: float
        variance: float
    }

    type ProductInformation = {
        schema_id: string
        schema_version: int
        product_type: string
        product_state: string
        creator_id: string
        creation_datetime: string
    }

    type ImageMetadata = {
        product_information: ProductInformation
        image_width: int
        image_height: int
        channels: int
        data_type: string
        file_md5: string
        image_statistics: ImageStatistics[]
        mission_name: string
        camera_system: string
    }

    let tryParseJson (jsonString: string) =    
        try
            let options = JsonSerializerOptions(PropertyNameCaseInsensitive = true)
            JsonSerializer.Deserialize<ImageMetadata>(jsonString, options) |> Result.Ok
        with e -> 
            Result.Error e

    let test () =

        // Example usage
        let jsonString = """
            {
                "label": "AFC1",
                "description": "AFC gray image",
                "exposure": 0.001285,
                "product_information": {
                    "schema_id": "https://www.joanneum.at/jim/product_information.schema.json",
                    "schema_version": 7262,
                    "product_type": "FITS imported MultiBandImage (MBI)",
                    "product_state": "imported",
                    "creator_id": "Fits2Mbi",
                    "creation_datetime": "2025-10-16T10:33:16.518865+0000"
                },
                "schema_id": "https://www.joanneum.at/jim/camera_image.schema.json",
                "schema_version": 8198,
                "product_type": "image",
                "image_width": 1020,
                "image_height": 1020,
                "channels": 1,
                "data_type": "float",
                "file_md5": "e83b582bde9d9b72287153a38eeada60",
                "image_file_format": "tif",
                "compression_type": "none",
                "image_statistics": [
                    {
                        "minimum": 0.0,
                        "maximum": 7.51392936706543,
                        "mean": 2.6648557319765795,
                        "median": 2.8954658227294314,
                        "standard_deviation": 0.9310177481678147,
                        "variance": 0.8667940474034683
                    }
                ]
            }
        """

        let metadata = tryParseJson jsonString
        printfn "%A" metadata

module Tiff_Mbi_Json = 
    
    open System
    open FSharp.Data
    open FSharp.Data.JsonExtensions
    open System.Globalization

    type Mbi = { obs_date : DateTime; sunPos : V3d; earthPos : V3d }

    let tryGetFitsHeader (headerName : string) (mbi : JsonValue) (m : JsonValue -> Option<'a>): Option<'a> = 
        let tryGetMatchingHeader (header: JsonValue) =
            match header.TryGetProperty(headerName) with 
            | Some header -> 
                m header?value 
            | _ -> None

        match mbi?fits_hdu_headers with
        | JsonValue.Array arr ->
            arr
            |> Array.tryPick tryGetMatchingHeader
        | _ -> 
            None


    let parseDate (s : JsonValue) : Option<DateTime> = 
        match s with
        | JsonValue.String s -> 
            match DateTime.TryParse(s, CultureInfo.InvariantCulture, DateTimeStyles.AssumeUniversal) with
            | (true, v) -> Some v
            | _ -> None
        | _ -> 
            None

    let parseFloat' (s : string) : Option<float> = 
        match System.Double.TryParse(s, NumberStyles.Float ||| NumberStyles.AllowThousands, CultureInfo.InvariantCulture) with
        | (true, v) -> Some v
        | _ -> None

    let parseFloat (s : JsonValue) : Option<float> = 
        match s with
        | JsonValue.Float s -> Some s
        | JsonValue.Number n -> Some (float n)
        | _ -> None

    let (|Float|_|) (s : string) = parseFloat' s

    let tryExtractDateObsFromMbi (mbi: JsonValue) : DateTime option =
        tryGetFitsHeader "DATE-OBS" mbi parseDate

    let tryExtractXyz (xName : string) (yName : string) (zName : string) (mbi : JsonValue) = 
        match tryGetFitsHeader xName mbi parseFloat, tryGetFitsHeader yName mbi parseFloat, tryGetFitsHeader zName mbi parseFloat with
        | Some x, Some y, Some z ->  
            V3d(x,y,z) |> Some
        | _ -> None

    let tryParseJson (content : string) = 
        match JsonValue.TryParse(content) with
        | Some mbi ->
            try
                let sunPos = tryExtractXyz "SUN_POSX" "SUN_POSY" "SUN_POSZ" mbi
                let earthPos = tryExtractXyz "EARTPOSX" "EARTPOSY" "EARTPOSZ" mbi
                match tryExtractDateObsFromMbi mbi, sunPos, earthPos  with
                | Some d, Some sunPos, Some earthPos -> { obs_date = d; sunPos = sunPos; earthPos = earthPos } |> Result.Ok
                | _ -> Result.Error (System.Exception("could not find DATE-OBS in mbi json"))
            with e ->
                Result.Error e
        | _ -> Result.Error (System.Exception("could not parse mbi json"))


type ParsedMetadata = Option<Tiff_Mbi_Json.Mbi> * Option<Tiff_Json.ImageMetadata>

let tryParseMetadataForImagePath (imagePath : string) : ParsedMetadata = 
    let getJsonInfoPath (imagePath : string) (suffix : string) : string = 
        let killPhrases = ["_Stacked"; "_AFC1"; "_AFC2"; "_HSH"]
        let fi = Path.Combine(Path.GetDirectoryName(imagePath), Path.GetFileNameWithoutExtension(imagePath) + suffix)
        if File.Exists fi then 
            fi
        else
            List.fold (fun (path : string) kill -> path.Replace(kill, "")) fi killPhrases

    let mbi_json = getJsonInfoPath imagePath ".mbi.json"
    let json = imagePath + ".json"
    match File.Exists(mbi_json), File.Exists(json) with
    | true, true -> 
        try
            let mbi_json = File.ReadAllText(mbi_json) 
            let jimMetadata = File.ReadAllText(json)
            match Tiff_Mbi_Json.tryParseJson mbi_json, Tiff_Json.tryParseJson jimMetadata with
            | Result.Ok mbi_json, Result.Ok tif_json -> Some mbi_json, Some tif_json
            | _, Result.Ok jimMetadata -> None, Some jimMetadata
            | Result.Ok mbi_json, _ -> Some mbi_json, None
            | _ -> None, None
        with e ->
            printfn $"could not parse json metadtata for {imagePath}: {e}"
            None, None
    | f, e -> 
        printfn "%s, %A" mbi_json  (f,e) 
        None, None

let discoverInstrumentFolder (dir : string) : seq<string * ParsedMetadata> = 
    let tifs = Directory.EnumerateFiles(dir, "*.tif", SearchOption.TopDirectoryOnly)
    tifs
    |> Seq.map (fun tifFilename -> 
        let metaData =  tryParseMetadataForImagePath tifFilename 
        tifFilename, metaData
    )
