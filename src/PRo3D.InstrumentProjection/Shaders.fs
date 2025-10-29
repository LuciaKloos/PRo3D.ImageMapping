namespace PRo3D.SPICE

open Aardvark.Base
open FShade

[<ReflectedDefinition>]
module Shaders =

    open Aardvark.Rendering
    open Aardvark.Rendering.Effects

    type TexturedVertex = {
        [<TexCoord>] tc : V2d
        [<Normal>] n : V3d
    }

    let genAndFlipTextureCoord (v : TexturedVertex) =
        vertex {
            return { v with tc = V2d(v.tc.X + 0.5, 1.0 - v.tc.Y) }
        }