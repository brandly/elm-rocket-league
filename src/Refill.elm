module Refill exposing (Refill, Size(..), init, isActive, view)

import Color
import Cylinder3d
import Direction3d
import Length exposing (Meters, meters)
import Luminance
import Materials
import Physics.Coordinates exposing (WorldCoordinates)
import Point3d exposing (Point3d)
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import Sphere3d
import Vector3d


type alias Refill =
    { point : Point3d Meters WorldCoordinates
    , time : Float
    , size : Size
    }


type Size
    = FullRefill
    | SmallRefill


isActive : Float -> Float -> Refill -> Bool
isActive reloadTime currentTime { time } =
    (currentTime - time) > reloadTime


init : { startTime : Float, measure : Float } -> List Refill
init { startTime, measure } =
    let
        refillRing x y =
            [ ( -measure * x, measure * y )
            , ( -measure * x, -measure * y )
            , ( measure * x, measure * y )
            , ( measure * x, -measure * y )
            ]
    in
    List.concat
        [ -- four surrounding center
          List.concat
            [ [ ( measure, 0 )
              , ( -measure, 0 )
              , ( 0, measure )
              , ( 0, -measure )

              -- center line
              , ( measure * 2.7, 0 )
              , ( -measure * 2.7, 0 )
              , ( measure * 4.1, 0 )
              , ( -measure * 4.1, 0 )
              ]
            , --
              refillRing 4.05 1.75
            , refillRing 3.2 0.9
            , refillRing 2.2 1.75
            , refillRing 2.4 3.4
            , refillRing 1 2
            ]
            |> List.map
                (\( x, y ) ->
                    { point = Point3d.meters x y 0
                    , time = startTime
                    , size = SmallRefill
                    }
                )
        , [ -- center sideline
            ( 0, measure * 3.6 )
          , ( 0, -measure * 3.6 )
          , --
            ( -measure * 4, measure * 3 )
          , ( -measure * 4, -measure * 3 )
          , --
            ( measure * 4, measure * 3 )
          , ( measure * 4, -measure * 3 )
          ]
            |> List.map
                (\( x, y ) ->
                    { point = Point3d.meters x y 0
                    , time = startTime
                    , size = FullRefill
                    }
                )
        ]


view : Bool -> Refill -> Scene3d.Entity WorldCoordinates
view active { point, size } =
    let
        shape =
            Cylinder3d.centeredOn point
                Direction3d.z
                { radius = Length.meters 0.75, length = Length.meters 0.3 }

        glowingOrange =
            Material.emissive (Light.color (Color.rgb255 255 127 0)) (Luminance.nits 5000)

        material =
            if active then
                glowingOrange

            else
                Material.uniform Materials.chromium
    in
    case ( size, active ) of
        ( FullRefill, True ) ->
            let
                orbPosition =
                    Point3d.translateBy (Vector3d.meters 0 0 1.5) point
            in
            Scene3d.group
                [ Scene3d.cylinder material shape
                , Scene3d.sphere material
                    (Sphere3d.atPoint orbPosition (Length.meters 0.6))
                ]

        _ ->
            Scene3d.cylinder material shape
