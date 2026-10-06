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

    let shadowsHighlightsMaskSampler =
        sampler2d {
            texture uniform?ShadowsHighlightsMaskTexture
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
        member x.MidtoneContrastAdjustment : float = uniform?MidtoneContrastAdjustment
        member x.Midpoint : float = uniform?Midpoint
        member x.Saturation : float = uniform?Saturation
        member x.HighlightAmount : float = uniform?HighlightAmount
        member x.HighlightTone : float = uniform?HighlightTone
        member x.HighlightRadius : float = uniform?HighlightRadius
        member x.ShadowAmount : float = uniform?ShadowAmount
        member x.ShadowTone : float = uniform?ShadowTone
        member x.ShadowRadius : float = uniform?ShadowRadius
        member x.BlurDirection : V2d = uniform?BlurDirection
        member x.BlurTextureSize : V2d = uniform?BlurTextureSize
        member x.BlurShadowRadius : float = uniform?BlurShadowRadius
        member x.BlurHighlightRadius : float = uniform?BlurHighlightRadius

    let hshShadowsHighlights (v : Vertex) =
        fragment {
            let rgb = rgbCompositeSampler.Sample(v.tc)

            let luminance =
                0.2126 * rgb.X +
                0.7152 * rgb.Y +
                0.0722 * rgb.Z

            let highlightStart =
                1.0 - min 1.0 (max 0.0 uniform.HighlightTone)

            let rawHighlightMask =
                if rgb.W > 0.0 then smoothstep highlightStart 1.0 luminance
                else 0.0

            let shadowEnd =
                min 1.0 (max 0.0 uniform.ShadowTone)

            let rawShadowMask =
                if rgb.W > 0.0 then 1.0 - smoothstep 0.0 shadowEnd luminance
                else 0.0

            // R = shadow, G = highlight
            return V4d(rawShadowMask, rawHighlightMask, 0.0, 1.0)
        }

    let boxBlur (v : Vertex) =
        fragment {
            let sizeX = max 1.0 uniform.BlurTextureSize.X
            let sizeY = max 1.0 uniform.BlurTextureSize.Y

            let shadowScale =
                max 0.0 uniform.BlurShadowRadius / 4.0

            let highlightScale =
                max 0.0 uniform.BlurHighlightRadius / 4.0

            let shadowStep =
                V2d(
                    uniform.BlurDirection.X * shadowScale / sizeX,
                    uniform.BlurDirection.Y * shadowScale / sizeY
                )

            let highlightStep =
                V2d(
                    uniform.BlurDirection.X * highlightScale / sizeX,
                    uniform.BlurDirection.Y * highlightScale / sizeY
                )

            let centre =
                shadowsHighlightsMaskSampler.Sample(v.tc)

            let shadowBlur =
                (centre.X +
                 shadowsHighlightsMaskSampler.Sample(v.tc - shadowStep).X +
                 shadowsHighlightsMaskSampler.Sample(v.tc + shadowStep).X +
                 shadowsHighlightsMaskSampler.Sample(v.tc - 2.0 * shadowStep).X +
                 shadowsHighlightsMaskSampler.Sample(v.tc + 2.0 * shadowStep).X +
                 shadowsHighlightsMaskSampler.Sample(v.tc - 3.0 * shadowStep).X +
                 shadowsHighlightsMaskSampler.Sample(v.tc + 3.0 * shadowStep).X +
                 shadowsHighlightsMaskSampler.Sample(v.tc - 4.0 * shadowStep).X +
                 shadowsHighlightsMaskSampler.Sample(v.tc + 4.0 * shadowStep).X) / 9.0

            let highlightBlur =
                (centre.Y +
                 shadowsHighlightsMaskSampler.Sample(v.tc - highlightStep).Y +
                 shadowsHighlightsMaskSampler.Sample(v.tc + highlightStep).Y +
                 shadowsHighlightsMaskSampler.Sample(v.tc - 2.0 * highlightStep).Y +
                 shadowsHighlightsMaskSampler.Sample(v.tc + 2.0 * highlightStep).Y +
                 shadowsHighlightsMaskSampler.Sample(v.tc - 3.0 * highlightStep).Y +
                 shadowsHighlightsMaskSampler.Sample(v.tc + 3.0 * highlightStep).Y +
                 shadowsHighlightsMaskSampler.Sample(v.tc - 4.0 * highlightStep).Y +
                 shadowsHighlightsMaskSampler.Sample(v.tc + 4.0 * highlightStep).Y) / 9.0

            return V4d(shadowBlur, highlightBlur, 0.0, 1.0)
        }

    [<ReflectedDefinition>]
    let applyShadowsHighlights
        (c : float)
        (shadowMask : float)
        (highlightMask : float)
        (highlightAmount : float)
        (shadowAmount : float) =
        let exponent = 2.8   // Gamma.init.exponent
        let hStrength = min 1.0 (max 0.0 (highlightAmount * highlightMask))
        let sStrength = min 1.0 (max 0.0 (shadowAmount * shadowMask))
        let hDelta = hStrength * (pow c exponent - c)
        let sDelta = sStrength * ((1.0 - pow (1.0 - c) exponent) - c)
        min 1.0 (max 0.0 (c + hDelta + sDelta))

    let hshColorsTF (v : Vertex) =
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
                    colormapTextureSampler.Sample(V2d ((if (uniform.DataType = 2) then remappedClampedNormalizedXFloat else remappedClampedNormalizedXInt16), 0.0))


            return remapClampNormalize
        }


    let hshColorsAdjustment (v : Vertex)  = 
        fragment {
            let src = rgbCompositeSampler.Sample(v.tc)

            let lum0 = 0.2126 * src.X + 0.7152 * src.Y + 0.0722 * src.Z
            //let highlightStart = 1.0 - min 1.0 (max 0.0 uniform.HighlightTone)
            //let highlightMask = if src.W > 0.0 then smoothstep highlightStart 1.0 lum0 else 0.0
            //let shadowEnd = min 1.0 (max 0.0 uniform.ShadowTone)
            //let shadowMask = if src.W > 0.0 then 1.0 - smoothstep 0.0 shadowEnd lum0 else 0.0

            let shadowMask = shadowsHighlightsMaskSampler.Sample(v.tc).X
            let highlightMask = shadowsHighlightsMaskSampler.Sample(v.tc).Y

            let hAmt = uniform.HighlightAmount
            let sAmt = uniform.ShadowAmount

            let rgb =
                V4d(applyShadowsHighlights src.X shadowMask highlightMask hAmt sAmt,
                    applyShadowsHighlights src.Y shadowMask highlightMask hAmt sAmt,
                    applyShadowsHighlights src.Z shadowMask highlightMask hAmt sAmt,
                    src.W)

            let remapClampNormalize =
                let midtoneMask = midtoneMaskSampler.Sample(v.tc).X
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
                    
                let rSaturated =
                    min 1.0 (max 0.0 (luminance + saturationGain * (rCorrected - luminance)))

                let gSaturated =
                    min 1.0 (max 0.0 (luminance + saturationGain * (gCorrected - luminance)))

                let bSaturated =
                    min 1.0 (max 0.0 (luminance + saturationGain * (bCorrected - luminance)))

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