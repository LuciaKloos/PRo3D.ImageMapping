namespace PRo3D.Core

open Aardvark.Base
open FSharp.Data.Adaptive
open Aardvark.SceneGraph
open Aardvark.Rendering

open PRo3D.InstrumentVisualization

module ImageProjection =

    module Shaders =

        open Aardvark.Base
    
        open FShade
        open Aardvark.Rendering.Effects


        type UniformScope with  
            member x.ProjectedImageModelViewProjValid : bool = uniform?ProjectedImageModelViewProjValid
            member x.ProjectedImageModelViewProj : M44d = uniform?ProjectedImageModelViewProj
            member x.ProjectedImagesLocalTrafos : M44d[] = uniform?StorageBuffer?ProjectedImagesLocalTrafos
            member x.ProjectedImagesCount : int = uniform?ProjectedImagesLocalTrafosCount
            member x.ProjectedImageOpacity : float = uniform?ProjectedImageOpacity
            member x.RedMinValue : float = uniform?RedMinValue
            member x.RedMaxValue : float = uniform?RedMaxValue
            member x.GreenMinValue : float = uniform?GreenMinValue
            member x.GreenMaxValue : float = uniform?GreenMaxValue
            member x.BlueMinValue : float = uniform?BlueMinValue
            member x.BlueMaxValue : float = uniform?BlueMaxValue
            member x.RgbDataType : int = uniform?RgbDataType
            member x.RgbProjectionDebug : bool = uniform?RgbProjectionDebug


        type Vertex = {
            [<Position>]    pos     : V4d
            [<Semantic("ProjectedImagePos")>] projectedPos : V4d
            [<Color>] c: V4d
            [<Semantic("BodyLocalPos")>] localPos : V4d
            [<Semantic("LocalNormal")>] localNormalNumericallyUnstable : V3d
            [<Normal>] n : V3d
        }
         
        let private projectedRedTexture =
            sampler2d {
                texture uniform?ProjectedRedTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Border
                addressV WrapMode.Border
                borderColor C4f.Black
            }

        let private projectedGreenTexture =
            sampler2d {
                texture uniform?ProjectedGreenTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Border
                addressV WrapMode.Border
                borderColor C4f.Black
            }

        let private projectedBlueTexture =
            sampler2d {
                texture uniform?ProjectedBlueTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Border
                addressV WrapMode.Border
                borderColor C4f.Black
            }

        [<ReflectedDefinition>]
        let normalizeRgbBand
            (sampledValue : float)
            (minValue : float)
            (maxValue : float)
            (dataType : int) =

            let rawValue =
                if dataType = 2 then
                    sampledValue
                elif dataType = 1 then
                    sampledValue * 65535.0
                else
                    sampledValue * 4294967295.0

            let valueRange =
                max 0.0000001 (maxValue - minValue)

            clamp 0.0 1.0 ((rawValue - minValue) / valueRange)

        let private projectedTexture =
            sampler2d {
                texture uniform?ProjectedTexture
                filter Filter.MinMagMipLinear
                addressU WrapMode.Border
                addressV WrapMode.Border
                borderColor C4f.Black
            }

        let stableImageProjectionTrafo (v : Vertex) =
            vertex {
                return { 
                    v with 
                        projectedPos = 
                            uniform.ProjectedImageModelViewProj * v.pos;
 
                        localPos = v.pos; }
            }

        let stableImageProjection (v : Vertex) = 
            fragment {
                let p = v.projectedPos.XYZ / v.projectedPos.W   // convert to ndc [-1,1]
                let tc = V3d(0.5, 0.5,0.5) + V3d(0.5, 0.5, 0.5) * p.XYZ     // convert to texture coordinates [0,1]
                let inRange = Vec.allGreaterOrEqual tc V3d.OOO && Vec.allSmallerOrEqual tc.XYZ V3d.III      // tests whether frag is in projector frustum
                let borderWidth = 0.01 

                // transforms the fragment’s normal into the projected camera’s coordinate system
                let normal = uniform.ProjectedImageModelViewProj.TransformDir(v.localNormalNumericallyUnstable) |> Vec.normalize

                let c = 
                    if uniform.ProjectedImageModelViewProjValid && inRange && normal.Z < 0.0 then
                        let AFC = V2d(1.0 - tc.Y, 1.0 - tc.X)
                        let HSH = V2d(1.0 - tc.Y, 1.0 - tc.X)
                        let AFC2 = V2d(tc.X, tc.Y)  // directly maps projected X and Y to texture U and V: no axis flipping
                        let c = projectedTexture.Sample(AFC2).X |> Shaders.remap    // only takes red channel of the texture, which is sufficient for grayscale images and allows to use single channel textures for better memory efficiency
                        let xBorder = (smoothstep 0.0 borderWidth tc.X) * smoothstep 1.0 (1.0 - borderWidth) tc.X 
                        let yBorder = (smoothstep 0.0 borderWidth tc.Y) * smoothstep 1.0 (1.0 - borderWidth) tc.Y
                        let borderFactor = xBorder * yBorder
                        let borderColor = V3d(0.0, 1.0, 0.0)
                        let blendedProjected = Fun.Lerp(uniform.ProjectedImageOpacity, v.c.XYZ, c.XYZ)
                        let borderImage = blendedProjected.XYZ * borderFactor + borderColor * (1.0 - borderFactor)
                        V4d(borderImage.XYZ, 1.0) 

                    
                    else
                        v.c
                return { v with c = c }
            }


        /// Projects three selected spectral bands onto the sphere.
        /// When RgbProjectionDebug is true, the projected footprint is split into:
        /// top-left = red only, top-right = green only,
        /// bottom-left = blue only, bottom-right = final RGB composite.
        let stableImageProjectionDebug (v : Vertex) =
            fragment {
                let p =
                    v.projectedPos.XYZ / v.projectedPos.W

                let tc =
                    V3d(0.5, 0.5, 0.5) +
                    V3d(0.5, 0.5, 0.5) * p.XYZ

                let inRange =
                    Vec.allGreaterOrEqual tc V3d.OOO &&
                    Vec.allSmallerOrEqual tc V3d.III

                let normal =
                    uniform.ProjectedImageModelViewProj.TransformDir(
                        v.localNormalNumericallyUnstable
                    )
                    |> Vec.normalize

                let color =
                    if uniform.ProjectedImageModelViewProjValid &&
                       inRange &&
                       normal.Z < 0.0 then

                        let projectedTc =
                            V2d(tc.X, tc.Y)

                        let isLeft =
                            projectedTc.X < 0.5

                        let isBottom =
                            projectedTc.Y < 0.5

                        // In diagnostic mode each quadrant displays the full image.
                        let sampledTc =
                            if uniform.RgbProjectionDebug then
                                let tcX = if isLeft then projectedTc.X * 2.0 else (projectedTc.X - 0.5) * 2.0
                                let tcY = if isBottom then projectedTc.Y * 2.0 else (projectedTc.Y - 0.5) * 2.0
                                V2d(
                                    tcX,
                                    tcY
                                )
                            else
                                projectedTc

                        let r =
                            normalizeRgbBand
                                (projectedRedTexture.Sample(sampledTc).X)
                                uniform.RedMinValue
                                uniform.RedMaxValue
                                uniform.RgbDataType

                        let g =
                            normalizeRgbBand
                                (projectedGreenTexture.Sample(sampledTc).X)
                                uniform.GreenMinValue
                                uniform.GreenMaxValue
                                uniform.RgbDataType

                        let b =
                            normalizeRgbBand
                                (projectedBlueTexture.Sample(sampledTc).X)
                                uniform.BlueMinValue
                                uniform.BlueMaxValue
                                uniform.RgbDataType

                        let rgb =
                            V3d(r, g, b)

                        let diagnosticColor =
                            if isLeft && not isBottom then
                                V3d(r, 0.0, 0.0)
                            elif not isLeft && not isBottom then
                                V3d(0.0, g, 0.0)
                            elif isLeft && isBottom then
                                V3d(0.0, 0.0, b)
                            else
                                rgb

                        let onSeparator =
                            uniform.RgbProjectionDebug &&
                            (abs(projectedTc.X - 0.5) < 0.004 ||
                             abs(projectedTc.Y - 0.5) < 0.004)

                        let projectedColor =
                            if onSeparator then
                                V3d.III
                            elif uniform.RgbProjectionDebug then
                                diagnosticColor
                            else
                                rgb

                        let borderWidth = 0.01
                        let xBorder =
                            smoothstep 0.0 borderWidth tc.X *
                            smoothstep 1.0 (1.0 - borderWidth) tc.X

                        let yBorder =
                            smoothstep 0.0 borderWidth tc.Y *
                            smoothstep 1.0 (1.0 - borderWidth) tc.Y

                        let borderFactor =
                            xBorder * yBorder

                        let borderColor =
                            V3d(0.0, 1.0, 0.0)

                        let blendedProjected =
                            Fun.Lerp(
                                uniform.ProjectedImageOpacity,
                                v.c.XYZ,
                                projectedColor
                            )

                        let borderedColor =
                            blendedProjected * borderFactor +
                            borderColor * (1.0 - borderFactor)

                        V4d(borderedColor, 1.0)
                    else
                        v.c

                return { v with c = color }
            }

        [<ReflectedDefinition>]
        let isBorder (tc : V3d) =
            let borderWidth = 0.0001 
            //let xBorder = (smoothstep 0.0 borderWidth tc.X) * smoothstep 1.0 (1.0 - borderWidth) tc.X 
            //let yBorder = (smoothstep 0.0 borderWidth tc.Y) * smoothstep 1.0 (1.0 - borderWidth) tc.Y
            //let borderFactor = xBorder * yBorder
            let borderX = tc.X < borderWidth || tc.X > 1.0 - borderWidth 
            let borderY = tc.Y < borderWidth || tc.Y > 1.0 - borderWidth
            borderX || borderY

        [<ReflectedDefinition>]
        let mapClippedProjectionsToColor (validCount : int) (totalCount : int) =
            let ratio = float validCount / float totalCount
            let color = 
                if ratio < 0.1 then V3d(0.0, 0.0, 1.0) // Blue
                elif ratio < 0.2 then V3d(0.0, 1.0, 1.0) // Cyan
                elif ratio < 0.3 then V3d(0.0, 1.0, 0.0) // Green
                elif ratio < 0.4 then V3d(1.0, 1.0, 0.0) // Yellow
                else V3d(1.0, 0.0, 0.0) // Red
            color

        [<ReflectedDefinition>]
        let mapClippedProjectionsToColor2 (validCount : int) (totalCount : int) =
            let ratio = float validCount / float totalCount
            let color = 
                if ratio < 0.1 then V3d(0.0, 0.0, 1.0) // Blue
                elif ratio < 0.2 then V3d(0.0, 0.5, 1.0) // Light Blue
                elif ratio < 0.3 then V3d(0.0, 1.0, 1.0) // Cyan
                elif ratio < 0.4 then V3d(0.0, 1.0, 0.5) // Light Green
                elif ratio < 0.5 then V3d(0.0, 1.0, 0.0) // Green
                elif ratio < 0.6 then V3d(0.5, 1.0, 0.0) // Yellow-Green
                elif ratio < 0.7 then V3d(1.0, 1.0, 0.0) // Yellow
                elif ratio < 0.8 then V3d(1.0, 0.5, 0.0) // Orange
                elif ratio < 0.9 then V3d(1.0, 0.0, 0.0) // Red
                else V3d(0.5, 0.0, 0.0) // Dark Red
            color

        let localImageProjections (v : Vertex) = 
            fragment {
                let mutable clippedCount = 0
                for i in 0 .. uniform.ProjectedImagesCount - 1 do
                    let ndc = uniform.ProjectedImagesLocalTrafos[i] * v.localPos
                    let normal = uniform.ProjectedImagesLocalTrafos[i].TransformDir(v.localNormalNumericallyUnstable).Normalized
                    let p = ndc.XYZ / ndc.W
                    let tc = V3d(0.5, 0.5, 0.5) + V3d(0.5, 0.5, 0.5) * p.XYZ
                    let clipped = Vec.anyGreater tc.XY V2d.II || Vec.anySmaller tc.XY V2d.OO
                    let onRightSide =  normal.Z < 0.0
                    if not onRightSide || clipped then
                        clippedCount <- clippedCount + 1

                if clippedCount < uniform.ProjectedImagesCount then 
                    let color = mapClippedProjectionsToColor2 (uniform.ProjectedImagesCount  - clippedCount) uniform.ProjectedImagesCount 
                    let c = v.c.XYZ * 0.8 + color * 0.2
                    return V4d(c, 1.0)
                else 
                    return v.c
            }

        type NormalVertex = {
            [<Position>] pos : V4d
            [<Semantic("LocalNormal")>] localNormal : V3d
            [<Normal>] n : V3d
            [<SourceVertexIndex>] i : int
        }


        let generateNormal (t : Triangle<NormalVertex>) =
            triangle {
                let p0 = t.P0.pos.XYZ
                let p1 = t.P1.pos.XYZ
                let p2 = t.P2.pos.XYZ

                let edge1 = p1 - p0
                let edge2 = p2 - p0

                let normal = Vec.cross edge2 edge1 |> Vec.normalize

                yield { t.P0 with localNormal = normal; i = 0 }
                yield { t.P1 with localNormal = normal; i = 1 }
                yield { t.P2 with localNormal = normal; i = 2 }
            }

        let useVertexNormals (v : NormalVertex) =
            vertex {
                return { v with localNormal = v.n.Normalized }
            }


module ImageProjectionTrafoSceneGraph =
    open Aardvark.Base.Ag
    open Aardvark.SceneGraph.Semantics.TrafoExtensions

    type PlanetApplicator(child : ISg, planet : string) =
        inherit Sg.AbstractApplicator(child)
        member x.Planet = planet
        
    [<Rule>]
    type PlanetSemantics() =
        member x.Planet(app : PlanetApplicator, scope : Ag.Scope) =
            app.Child?Planet <- app.Planet
        

    type ProjectedImageApplicator(child : ISg, primaryViewProjection : string -> aval<Option<Trafo3d>>, overlayViewProjection : string -> aval<Option<Trafo3d>>) =
        inherit Sg.AbstractApplicator(child)

        member x.PrimaryViewProjection = primaryViewProjection
        member x.OverlayViewProjection = overlayViewProjection

    [<Rule>]
    type ProjectedImageSemantics() =
        member private x.ComputeModelViewProj
            (
                viewProjection : string -> aval<Option<Trafo3d>>,
                planet : string,
                modelTrafo : aval<Trafo3d>
            ) =
        
            let projectionTrafo =
                viewProjection planet

            let possiblyTrafo =
                projectionTrafo
                |> AVal.bind (function
                    | None ->
                        AVal.constant None

                    | Some vp ->
                        modelTrafo
                        |> AVal.map (fun m ->
                            Some (m * vp)
                        )
                )

            possiblyTrafo
            |> AVal.map (Option.defaultValue Trafo3d.Identity)


        member x.ProjectedImageModelViewProj(app : ProjectedImageApplicator, scope : Ag.Scope) =
            let planet : string = scope?Planet
            let modelTrafo = scope.ModelTrafo 
            
            let primaryTrafo =
                x.ComputeModelViewProj(app.PrimaryViewProjection, planet, modelTrafo)

            let overlayTrafo =
                x.ComputeModelViewProj(app.OverlayViewProjection, planet, modelTrafo)

            app.Child?ProjectedImageModelViewProj <- primaryTrafo
            app.Child?OverlayImageModelViewProj <- overlayTrafo

     
module Sg = 
    open ImageProjectionTrafoSceneGraph

    let applyPlanet (planet : string) (sg : ISg) =
        PlanetApplicator(sg, planet) :> ISg

    let applyProjectedImages
        (primaryViewProjection : string -> aval<Option<Trafo3d>>)
        (overlayViewProjection : string -> aval<Option<Trafo3d>>)
        (sg : ISg) =
        ProjectedImageApplicator(sg, primaryViewProjection, overlayViewProjection) :> ISg

    let applyProjectedImage
        (viewProjection : string -> aval<Option<Trafo3d>>)
        (sg : ISg) =

        let noOverlayProjection (_planet : string) =
            AVal.constant None

        ProjectedImageApplicator(
            sg,
            viewProjection,
            noOverlayProjection
        ) :> ISg