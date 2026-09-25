namespace PRo3D.ImageMapping

open Aardvark.Base


module Shaders = 
    open FShade
    open Aardvark.Rendering.Effects

    let instrumentSampler = 
        sampler2d {
            texture uniform?InstrumentImage
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let colormapTextureSampler =
        sampler2d {
            texture uniform?ColormapTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Wrap
            addressV WrapMode.Wrap
        }

    let rgbCompositeSampler =
        sampler2d {
            texture uniform?RgbCompositeTexture
            filter Filter.MinMagMipLinear
            addressU WrapMode.Clamp
            addressV WrapMode.Clamp
        }

    type UniformScope with
        member x.MinValue : float = uniform?MinValue
        member x.MaxValue : float = uniform?MaxValue
        member x.UseFalseColor : bool = uniform?UseFalseColor
        member x.Brightness : float = uniform?Brightness
        member x.DataType : int = uniform?DataType
        member x.OverlayMax : V2d = uniform?OverlayMax
        member x.OverlayMin : V2d = uniform?OverlayMin
        member x.ShowPixelMarker : bool = uniform?ShowPixelMarker
        member x.PixelMarkerMin : V2d = uniform?PixelMarkerMin
        member x.PixelMarkerMax : V2d = uniform?PixelMarkerMax

    let hshColors (v : Vertex)  = 
        fragment {
            let hshValueX = instrumentSampler.Sample(v.tc).X 
            let remappedClampedNormalizedXInt16 =
                ((min uniform.MaxValue (max uniform.MinValue (hshValueX * 65000.0))) - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue)
            let remappedClampedNormalizedXFloat =
                (hshValueX - uniform.MinValue) / (uniform.MaxValue - uniform.MinValue)
            let remapClampNormalize =
                if uniform.UseFalseColor then
                    V4d(
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        (if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16),
                        1.0
                    )
                else 
                    if uniform.Brightness <> 0.0 then
                        let rgb = rgbCompositeSampler.Sample(v.tc)

                        let r = min 1.0 (max 0.0 (rgb.X + uniform.Brightness))
                        let g = min 1.0 (max 0.0 (rgb.Y + uniform.Brightness))
                        let b = min 1.0 (max 0.0 (rgb.Z + uniform.Brightness))

                        V4d(r, g, b, rgb.W)
                        
                    else
                        colormapTextureSampler.Sample(V2d ((if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16), 0.0))

            return remapClampNormalize
        }

    let displayRgbComposite (v : Vertex) =
        fragment {
            if
                uniform.ShowPixelMarker &&
                v.tc.X >= uniform.PixelMarkerMin.X &&
                v.tc.X <= uniform.PixelMarkerMax.X &&
                v.tc.Y >= uniform.PixelMarkerMin.Y &&
                v.tc.Y <= uniform.PixelMarkerMax.Y
            then
                return V4d(1.0, 0.0, 0.0, 1.0)
            else
                return rgbCompositeSampler.Sample(v.tc)
        }

    let solidRed (_ : Vertex) =
        fragment {
            return V4d(1.0, 0.0, 0.0, 1.0)
        }