namespace PRo3D.ImageMapping

open System
open Aardvark.UI.Primitives
open FSharp.Data.Adaptive
open PRo3D.ImageMapping.Model

open PRo3D.InstrumentProjection


type NcProductKind =
    | Reflectance
    | ReflectanceUncertainty
    | Mask

type NcDatasetInfo =
    {
        path        : string
        datasetPath : string
        width       : int
        height      : int
        bands       : int
        productKind : NcProductKind
    }

type RgbBandSource =
        {
            logicalIndex : int
            filePath     : string
            channelIndex : int
            wavelength   : Option<float>
        }

type RgbBandData =
    {
        source : RgbBandSource
        width  : int
        height : int
        values : float[]
    }

type MbiBandInfo =
        {
            index      : int
            filePath   : string
            label      : Option<string>
            wavelength : Option<float>
            exposure   : Option<float>
        }

type BandRatioRenderSettings =
        {
            redNumeratorBand : aval<Option<int>>
            redDenominatorBand : aval<Option<int>>
            greenNumeratorBand : aval<Option<int>>
            greenDenominatorBand : aval<Option<int>>
            blueNumeratorBand : aval<Option<int>>
            blueDenominatorBand : aval<Option<int>>
            gamma : aval<float>
        }

type RgbMappingRenderSettings = 
    {
        redBand : aval<Option<int>>
        greenBand : aval<Option<int>>
        blueBand : aval<Option<int>>
        gamma : aval<float>
    }

type TransferFunctionRenderSettings =
    {
        selectedBand : aval<Option<int>>
        gamma : aval<float>
    }

type ShadowsHighlightsAdjustmentsRenderSettings =
    {
        highlightAdjustments : aval<HighlightAdjustment>
        shadowAdjustments : aval<ShadowAdjustment>
        midtoneContrast : aval<MidtoneContrastAdjustment>
        blackWhiteClip : aval<BlackWhiteClip>
        saturation : aval<Saturation>
        brightness : aval<Brightness>
    }

type PreparedRgbImage =
    {
        width       : int
        height      : int
        red         : byte[]
        green       : byte[]
        blue        : byte[]
        alpha       : byte[]
        redValues   : float[]
        greenValues : float[]
        blueValues  : float[]
        valid       : bool[]
    }

type SortedChannels =
    {
        red   : float[]
        green : float[]
        blue  : float[]
    }

type HistogramBin =
    {
        lower    : float
        upper    : float
        count    : int
        fraction : float
    }

type SpectralProfilePoint =
    {
        wavelength : float
        value : float    
        color : string
    }

type RgbSelectedBandHistogram =
    {
        label : string
        bandIndices : int[] option
        histogram : HistogramBin[]
    }

type RGBSelectedBandSpectralProfile =
    {
        label : string
        wavelengthSpan : Option<float * float>
        spectralProfile : SpectralProfilePoint[]
    }

module ImageDefaults =

    let initialPath = ""

    let minValue =
        {
            value   = 0.0
            min     = 0.0
            max     = 65000.0
            step    = 1
            format  = "{0:0.00}"
        }

    let maxValue =
        {
            value   = 0.0
            min     = 0.0
            max     = 65000.0
            step    = 1
            format  = "{0:0.00}"
        }

    let initial : PRo3D.ImageMapping.Model.Image =
        {
            colorMap = ColorMap.Magma
            useFalseColor = true
            selectedChannel = { idx = 0; name = None }
            channelOptions = []
            dataType = PRo3D.ImageMapping.Model.DataType.UInt16
            defaultMinValues = [ minValue.value ]
            defaultMaxValues = [ maxValue.value ]
            inputMinValue = minValue
            inputMaxValue = maxValue
            texture = initialPath

            bandIndex = 0
            wavelength = None

            distance = 0.0
            time = DateTime()
        }