namespace Aardvark.UI.Primitives

open Adaptify
open Aardvark.Base
open Aardvark.Rendering

[<ModelType>]
type OrbitState =
    internal {
        sky     : V3d
        right   : V3d

        center  : V3d
        phi     : float
        theta   : float
        _radius  : float

        targetPhi : float
        targetTheta : float
        targetRadius : float
        targetCenter : V3d

        
        dragStart : Option<V2i>
        panning   : bool
        pan       : V2d
        targetPan : V2d

        [<NonAdaptive>]
        lastRender : Option<MicroTime>

        _view : CameraView
        
        radiusRange : Range1d
        thetaRange : Range1d
        moveSensitivity : float
        zoomSensitivity : float
        speed : float

        config : OrbitControllerConfig
    } 
