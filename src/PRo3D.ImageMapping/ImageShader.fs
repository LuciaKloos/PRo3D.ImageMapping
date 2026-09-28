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

    let midtoneMaskSampler =
        sampler2d {
            texture uniform?MidtoneMaskTexture
            filter Filter.MinMagMipPoint
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
        member x.MidtoneContrastAdjustment : float = uniform?MidtoneContrastAdjustment
        member x.Midpoint : float = uniform?Midpoint
        member x.Saturation : float = uniform?Saturation

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
                    let midtoneMask = midtoneMaskSampler.Sample(v.tc).X
                    let rgb = rgbCompositeSampler.Sample(v.tc)
                    let midtoneGain = uniform.MidtoneContrastAdjustment
                        
                    let slider = min 1.0 (max -1.0 midtoneGain)
                    let gain = 1.0 + slider
                    let midpoint = uniform.Midpoint

                    let offset = midpoint * (1.0 - gain)

                    let rTarget = min 1.0 (max 0.0 (gain * rgb.X + offset))
                    let gTarget = min 1.0 (max 0.0 (gain * rgb.Y + offset))
                    let bTarget = min 1.0 (max 0.0 (gain * rgb.Z + offset))

                    let rCorrected = min 1.0 (max 0.0 (rgb.X + midtoneMask * (rTarget - rgb.X)))
                    let gCorrected = min 1.0 (max 0.0 (rgb.Y + midtoneMask * (gTarget - rgb.Y)))
                    let bCorrected = min 1.0 (max 0.0 (rgb.Z + midtoneMask * (bTarget - rgb.Z)))

                    let luminance =
                        0.2126 * rCorrected +
                        0.7152 * gCorrected +
                        0.0722 * bCorrected

                    let saturationGain = 1.0 + uniform?Saturation
                    
                    let rSaturated = luminance + saturationGain * (rCorrected - luminance)
                    let gSaturated = luminance + saturationGain * (gCorrected - luminance)
                    let bSaturated = luminance + saturationGain * (bCorrected - luminance)

                    let brightness = min 1.0 (max -1.0 uniform.Brightness)

                    // nonlinear brightening: move toward sqrt(c) (or toward c*c)
                    let r = (if (brightness > 0.0) then (rSaturated + brightness * (sqrt(rSaturated) - rSaturated)) else (rSaturated + (-brightness) * (rSaturated * rSaturated - rSaturated)))
                    let g = (if (brightness > 0.0) then (gSaturated + brightness * (sqrt(gSaturated) - gSaturated)) else (gSaturated + (-brightness) * (gSaturated * gSaturated - gSaturated)))
                    let b = (if (brightness > 0.0) then (bSaturated + brightness * (sqrt(bSaturated) - bSaturated)) else (bSaturated + (-brightness) * (bSaturated * bSaturated - bSaturated)))
                        
                    V4d(r, g, b, rgb.W)

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