module Main exposing (main)

import Acceleration
import Angle exposing (Angle)
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Camera3d
import Color exposing (Color)
import Cylinder3d exposing (Cylinder3d)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Illuminance
import Json.Decode
import Length exposing (Length, Meters, inMeters, meters)
import Luminance
import Mass
import Materials
import Physics.Body as Body exposing (Body)
import Physics.Constraint as Constraint exposing (Constraint)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d exposing (Sphere3d)
import Task
import Vector3d
import Viewpoint3d
import WebGL.Texture


type alias Data =
    { entity : Scene3d.Entity BodyCoordinates
    , id : EntityId
    }


type EntityId
    = Car (List Wheel)
    | Ball
    | Obstacle


isBall : EntityId -> Bool
isBall id =
    id == Ball


isCar : EntityId -> Bool
isCar id =
    case id of
        Car _ ->
            True

        _ ->
            False


type CameraFocus
    = BallCam
    | ForwardCam


type alias Wheel =
    { chassisConnectionPoint : Point3d Meters BodyCoordinates
    , steering : Angle
    , rotation : Angle
    , deltaRotation : Angle
    , suspensionImpulse :
        Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , suspensionLength : Length
    , engineForce : Force
    , brake : Force
    , contact :
        Maybe
            { point : Point3d Meters WorldCoordinates
            , normal : Direction3d WorldCoordinates
            , body : Body Data
            }
    }


defaultWheel : Wheel
defaultWheel =
    { chassisConnectionPoint = Point3d.origin -- set for different wheels
    , steering = Quantity.zero
    , rotation = Quantity.zero
    , deltaRotation = Quantity.zero
    , suspensionImpulse = Quantity.zero
    , suspensionLength = Quantity.zero
    , engineForce = Quantity.zero
    , brake = Quantity.zero
    , contact = Nothing
    }


type alias Game =
    { world : World Data
    , controls : Controls
    , boostTank : Float
    , focus : CameraFocus
    , lastTick : Float
    , refills : List Refill
    }


type alias Controls =
    { rockets : Bool
    , steering : Float -- -1, 0, 1
    , speeding : Float -- -1, 0, 1
    , braking : Bool
    }


type alias Refill =
    { point : Point3d Meters WorldCoordinates
    , time : Float
    , size : RefillSize
    }


type RefillSize
    = FullRefill
    | SmallRefill


refillIsActive : Float -> Refill -> Bool
refillIsActive currentTime { time } =
    (currentTime - time) > boostSettings.reloadTime


boostSettings =
    { reloadTime = 10000
    , initial = 45
    , max = 100
    , refill = 12
    }


type Msg
    = Tick Float
    | Resize Float Float
    | KeyDown Command
    | KeyUp Command
    | TextureResponse (Result WebGL.Texture.Error (Material.Texture Color))


type Command
    = Jump
    | Rocket
    | Steer Float
    | Speed Float
    | ToggleCam


type alias CarSettings =
    { downDirection : Direction3d BodyCoordinates
    , rightDirection : Direction3d BodyCoordinates
    , forwardDirection : Direction3d BodyCoordinates
    , suspensionRestLength : Length
    , minSuspensionLength : Length
    , maxSuspensionLength : Length
    , radius : Length
    , suspensionStiffness : Float
    , dampingCompression : Float
    , dampingRelaxation : Float
    , frictionSlip : Float
    , rollInfluence : Float
    , maxSuspensionForce : Force
    , customSlidingRotationalSpeed : Maybe Float
    }


carSettings : CarSettings
carSettings =
    { downDirection = Direction3d.negativeZ
    , rightDirection = Direction3d.y
    , forwardDirection = Direction3d.x
    , suspensionRestLength = Length.meters 0.3
    , minSuspensionLength = Length.meters 0
    , maxSuspensionLength = Length.meters 0.6
    , radius = Length.meters 0.5
    , suspensionStiffness = 160
    , dampingCompression = 4.4
    , dampingRelaxation = 2.3
    , frictionSlip = 5
    , rollInfluence = 0.01
    , maxSuspensionForce = Force.newtons 100000
    , customSlidingRotationalSpeed = Just -30
    }


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias ScreenSize =
    { width : Float
    , height : Float
    }


type alias Model =
    { status : Status
    , screenSize : ScreenSize
    }


type Status
    = Loading
    | LoadingError String
    | Playing Game


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screenSize = { width = 0, height = 0 }, status = Loading }
    , Cmd.batch
        [ Task.attempt TextureResponse <|
            Material.loadWith Material.trilinearFiltering
                "static/rl-map.png"
        , Task.perform (\{ viewport } -> Resize viewport.width viewport.height)
            Dom.getViewport
        ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        keepPlaying g =
            { model | status = Playing g }

        mapControls : (Controls -> Controls) -> Model
        mapControls fn =
            case model.status of
                Playing g ->
                    { model | status = Playing { g | controls = fn g.controls } }

                _ ->
                    model
    in
    case ( model.status, msg ) of
        ( _, Resize w h ) ->
            ( { model | screenSize = { width = w, height = h } }
            , Cmd.none
            )

        ( Playing game, Tick tick ) ->
            let
                controls =
                    game.controls

                carPoint =
                    game.world
                        |> World.bodies
                        |> List.filter
                            (Body.data
                                >> .id
                                >> isCar
                            )
                        |> List.head
                        |> Maybe.map Body.originPoint
                        |> Maybe.withDefault (Point3d.meters 0 0 0)

                ballPoint =
                    game.world
                        |> World.bodies
                        |> List.filter
                            (Body.data
                                >> .id
                                >> isBall
                            )
                        |> List.head
                        |> Maybe.map Body.originPoint
                        |> Maybe.withDefault (Point3d.meters 0 0 0)

                fullTank =
                    game.boostTank >= boostSettings.max

                carHits : Refill -> Bool
                carHits { point, time } =
                    not fullTank
                        && ((currentTick - time) > boostSettings.reloadTime)
                        && (Point3d.distanceFrom carPoint point |> Length.inMeters)
                        < 2.5

                currentTick =
                    game.lastTick + tick

                applyRockets boostTank =
                    if controls.rockets then
                        max 0 (boostTank - 0.5)

                    else
                        boostTank

                applyRefills boostTank =
                    let
                        adding =
                            game.refills
                                |> List.filter carHits
                                |> List.map
                                    (\refill ->
                                        case refill.size of
                                            FullRefill ->
                                                boostSettings.max

                                            SmallRefill ->
                                                boostSettings.refill
                                    )
                                |> List.sum
                    in
                    min boostSettings.max (adding + boostTank)

                bodyUpdate body =
                    case (Body.data body).id of
                        Car wheels ->
                            let
                                boost =
                                    if controls.rockets && game.boostTank > 0 then
                                        Body.applyForce (Force.newtons 30000)
                                            (Direction3d.placeIn (Body.frame body) carSettings.forwardDirection)
                                            (Body.originPoint body)

                                    else
                                        identity
                            in
                            simulateCar (Duration.milliseconds tick) game wheels body
                                |> boost

                        _ ->
                            body
            in
            ( keepPlaying
                { game
                    | world =
                        game.world
                            |> World.update bodyUpdate
                            |> World.simulate (Duration.seconds (tick / 1000))
                    , boostTank =
                        game.boostTank
                            |> applyRefills
                            |> applyRockets
                    , lastTick = currentTick
                    , refills =
                        game.refills
                            |> List.map
                                (\refill ->
                                    if refillIsActive currentTick refill && carHits refill then
                                        { refill | time = currentTick }

                                    else
                                        refill
                                )
                }
            , Cmd.none
            )

        ( Playing game, KeyDown Jump ) ->
            ( keepPlaying
                { game
                    | world =
                        World.update
                            (\body ->
                                case (Body.data body).id of
                                    Car _ ->
                                        body
                                            |> Body.applyForce (Force.newtons 400000)
                                                -- TODO: add direction modifier keys
                                                (body |> Body.frame >> Frame3d.zDirection)
                                                (Body.originPoint body)

                                    _ ->
                                        body
                            )
                            game.world
                }
            , Cmd.none
            )

        ( Playing _, KeyUp Jump ) ->
            ( model, Cmd.none )

        ( Playing game, KeyDown Rocket ) ->
            ( mapControls (\c -> { c | rockets = True }), Cmd.none )

        ( Playing game, KeyUp Rocket ) ->
            ( mapControls (\c -> { c | rockets = False }), Cmd.none )

        ( Playing game, KeyDown (Steer k) ) ->
            ( mapControls (\c -> { c | steering = k }), Cmd.none )

        ( Playing game, KeyUp (Steer k) ) ->
            ( mapControls
                (\c ->
                    { c
                        | steering =
                            if k == c.steering then
                                0

                            else
                                c.steering
                    }
                )
            , Cmd.none
            )

        ( Playing game, KeyDown (Speed k) ) ->
            ( mapControls (\c -> { c | speeding = k }), Cmd.none )

        ( Playing game, KeyUp (Speed k) ) ->
            ( mapControls
                (\c ->
                    { c
                        | speeding =
                            if k == c.speeding then
                                0

                            else
                                c.speeding
                    }
                )
            , Cmd.none
            )

        ( Playing game, KeyDown ToggleCam ) ->
            ( keepPlaying
                { game
                    | focus =
                        if game.focus == BallCam then
                            ForwardCam

                        else
                            BallCam
                }
            , Cmd.none
            )

        ( Playing game, KeyUp ToggleCam ) ->
            ( model, Cmd.none )

        ( Playing _, TextureResponse _ ) ->
            ( model, Cmd.none )

        ( Loading, TextureResponse (Ok texture) ) ->
            let
                measure =
                    roomSize.length / 10.8

                refillRing x y =
                    [ ( -measure * x, measure * y )
                    , ( -measure * x, -measure * y )
                    , ( measure * x, measure * y )
                    , ( measure * x, -measure * y )
                    ]
            in
            ( { model
                | status =
                    Playing
                        { world =
                            initialWorld
                                |> World.add (floor texture)
                                -- TODO: ceiling
                                |> (\world -> List.foldl World.add world panels)
                        , controls =
                            { rockets = False
                            , steering = 0
                            , speeding = 0
                            , braking = False
                            }
                        , boostTank = boostSettings.initial
                        , focus = BallCam
                        , lastTick = 0
                        , refills =
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
                                            , time = -boostSettings.reloadTime
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
                                            , time = -boostSettings.reloadTime
                                            , size = FullRefill
                                            }
                                        )
                                ]
                        }
              }
            , Cmd.none
            )

        ( Loading, TextureResponse (Err err) ) ->
            ( { model
                | status =
                    LoadingError
                        (case err of
                            WebGL.Texture.LoadError ->
                                "Load error"

                            WebGL.Texture.SizeError x y ->
                                "Image not powers of 2: " ++ String.fromInt x ++ " " ++ String.fromInt y
                        )
              }
            , Cmd.none
            )

        ( Loading, _ ) ->
            ( model, Cmd.none )

        ( LoadingError _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
        , Events.onAnimationFrameDelta Tick
        , Events.onKeyDown (keyDecoder KeyDown)
        , Events.onKeyUp (keyDecoder KeyUp)
        ]


keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                case string of
                    "ArrowLeft" ->
                        Json.Decode.succeed (toMsg (Steer -1))

                    "ArrowRight" ->
                        Json.Decode.succeed (toMsg (Steer 1))

                    "ArrowUp" ->
                        Json.Decode.succeed (toMsg (Speed 1))

                    "ArrowDown" ->
                        Json.Decode.succeed (toMsg (Speed -1))

                    " " ->
                        Json.Decode.succeed (toMsg Jump)

                    "Shift" ->
                        Json.Decode.succeed (toMsg Rocket)

                    "c" ->
                        Json.Decode.succeed (toMsg ToggleCam)

                    _ ->
                        Json.Decode.fail ("Unrecognized key: " ++ string)
            )


view : Model -> Html Msg
view model =
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        ]
        (case model.status of
            Loading ->
                [ Html.p [] [ Html.text "Loading........" ] ]

            Playing game ->
                viewGame model.screenSize game

            LoadingError error ->
                [ Html.p [] [ Html.text ("Error: " ++ error) ] ]
        )


viewGame : ScreenSize -> Game -> List (Html Msg)
viewGame { width, height } { world, refills, boostTank, focus, lastTick } =
    let
        car =
            world
                |> World.bodies
                |> List.filter
                    (Body.data
                        >> .id
                        >> isCar
                    )
                |> List.head

        camera =
            let
                ballBody =
                    world
                        |> World.bodies
                        |> List.filter
                            (Body.data
                                >> .id
                                >> isBall
                            )
                        |> List.head

                frameOrigin =
                    Body.frame >> Frame3d.originPoint

                defaultAngle =
                    Angle.degrees 180

                defaultPoint =
                    Point3d.meters 0 0 0

                { focalPoint, azimuth, distance, elevation } =
                    case ( focus, ballBody, car ) of
                        ( BallCam, Just ball_, Just car_ ) ->
                            { -- Focus on the ball
                              focalPoint = frameOrigin ball_
                            , azimuth =
                                Direction3d.from (frameOrigin ball_) (frameOrigin car_)
                                    |> Maybe.map (Direction3d.azimuthIn SketchPlane3d.xy)
                                    |> Maybe.withDefault defaultAngle
                            , distance =
                                Point3d.distanceFrom (frameOrigin ball_) (frameOrigin car_)
                                    |> Quantity.plus (Length.meters 30)
                            , elevation =
                                Direction3d.elevationFrom SketchPlane3d.xy
                                    (Direction3d.from
                                        (frameOrigin ball_
                                            |> Point3d.translateBy (Vector3d.meters 0 0 -3)
                                        )
                                        (frameOrigin car_
                                            |> Point3d.translateBy (Vector3d.meters 0 30 0)
                                        )
                                        |> Maybe.withDefault Direction3d.x
                                    )
                            }

                        ( ForwardCam, Just ball_, Just car_ ) ->
                            { -- Focus on a point meters above the car
                              focalPoint =
                                frameOrigin car_
                                    |> Point3d.translateBy (Vector3d.meters 0 0 4)

                            -- Look the direction the car is moving
                            , azimuth =
                                Body.velocity car_
                                    |> Vector3d.direction
                                    |> Maybe.withDefault Direction3d.x
                                    |> Direction3d.reverse
                                    |> Direction3d.azimuthIn
                                        SketchPlane3d.xy
                            , distance = Quantity 30
                            , elevation = Angle.degrees 3
                            }

                        _ ->
                            { focalPoint = defaultPoint, azimuth = defaultAngle, distance = Quantity 30, elevation = defaultAngle }
            in
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbit
                        { focalPoint = focalPoint
                        , azimuth = azimuth
                        , elevation = elevation
                        , distance = distance
                        , groundPlane = SketchPlane3d.xy
                        }
                , verticalFieldOfView = Angle.degrees 24
                }

        drawables : List (Scene3d.Entity WorldCoordinates)
        drawables =
            List.concat
                [ world
                    |> World.bodies
                    |> List.filter
                        (\body ->
                            case (Body.data body).id of
                                Obstacle ->
                                    let
                                        eyePoint =
                                            Viewpoint3d.eyePoint (Camera3d.viewpoint camera)

                                        wallPlane =
                                            Frame3d.xyPlane (Body.frame body)
                                    in
                                    Point3d.signedDistanceFrom wallPlane eyePoint
                                        |> Quantity.greaterThan Quantity.zero

                                _ ->
                                    True
                        )
                    |> List.map getTransformedDrawable
                , case ( car, Maybe.map (Body.data >> .id) car ) of
                    ( Just car_, Just (Car wheels) ) ->
                        renderWheels car_ wheels

                    _ ->
                        []
                , List.map
                    (\refill ->
                        renderRefill (refillIsActive lastTick refill) refill
                    )
                    refills
                ]

        sunlight =
            Light.directional (Light.castsShadows True)
                { chromaticity = Light.sunlight
                , intensity = Illuminance.lux 10000
                , direction = Direction3d.xyZ (Angle.degrees 45) (Angle.degrees -60)
                }

        daylight =
            Light.overhead
                { upDirection = Direction3d.z
                , chromaticity = Light.daylight
                , intensity = Illuminance.lux 15000
                }
    in
    [ Scene3d.custom
        { dimensions = ( pixels (Basics.floor width), pixels (Basics.floor height) )
        , antialiasing = Scene3d.multisampling
        , camera = camera
        , lights = Scene3d.twoLights sunlight daylight
        , exposure = Scene3d.maxLuminance (Luminance.nits 10000)
        , toneMapping = Scene3d.noToneMapping
        , whiteBalance = Light.daylight
        , background = Scene3d.transparentBackground
        , clipDepth = meters 0.1
        , entities = drawables
        }
    , Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "bottom" "12px"
        , Html.Attributes.style "left" "24px"
        , Html.Attributes.style "padding" "0 12px"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "background-color" "rgba(255,255,255, 0.5)"
        ]
        [ Html.p [] [ Html.text "Drive - Arrow keys" ]
        , Html.p [] [ Html.text "Boost - Shift" ]
        , Html.p [] [ Html.text "Toggle Camera - C" ]
        , Html.p [] [ Html.text "Jump (buggy) - Spacebar" ]
        ]
    , Html.div
        [ Html.Attributes.style "position" "fixed"
        , Html.Attributes.style "bottom" "12px"
        , Html.Attributes.style "right" "24px"
        , Html.Attributes.style "text-align" "right"
        , Html.Attributes.style "padding" "6px 12px"
        , Html.Attributes.style "border-radius" "4px"
        , Html.Attributes.style "background-color" "rgba(255,255,255, 0.5)"
        ]
        [ Html.p
            [ Html.Attributes.style "font-size" "48px"
            , Html.Attributes.style "margin" "0"
            ]
            [ Html.text (String.fromInt (round boostTank)) ]
        , Html.p [ Html.Attributes.style "margin" "0" ]
            [ Html.text "BOOST" ]
        ]
    ]


renderWheels : Body Data -> List Wheel -> List (Scene3d.Entity WorldCoordinates)
renderWheels car wheels =
    List.map
        (\wheel ->
            let
                frame =
                    Body.frame car

                position =
                    wheel.chassisConnectionPoint
                        |> Point3d.placeIn frame

                downDirection =
                    carSettings.downDirection
                        |> Direction3d.placeIn frame

                newPosition =
                    position |> Point3d.translateBy (Vector3d.withLength wheel.suspensionLength downDirection)

                newFrame =
                    frame
                        |> Frame3d.moveTo newPosition
                        |> Frame3d.rotateAround (Axis3d.through newPosition downDirection) wheel.steering
            in
            wheelBody
                |> Scene3d.placeIn newFrame
        )
        wheels


wheelBody =
    Scene3d.cylinderWithShadow (Material.uniform Materials.chromium) wheelShape


wheelShape : Cylinder3d Meters BodyCoordinates
wheelShape =
    Cylinder3d.centeredOn
        Point3d.origin
        Direction3d.y
        { radius = carSettings.radius, length = Length.meters 0.3 }


initialWorld : World Data
initialWorld =
    let
        earthGravity =
            Acceleration.metersPerSecondSquared 9.807
    in
    World.empty
        |> World.withGravity earthGravity Direction3d.negativeZ
        |> World.add base
        |> World.add ball


renderRefill : Bool -> Refill -> Scene3d.Entity WorldCoordinates
renderRefill active { point, size } =
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


simulateCar : Duration -> Game -> List Wheel -> Body Data -> Body Data
simulateCar dt { world, controls } wheels car =
    case wheels of
        [ w1, w2, w3, w4 ] ->
            let
                engineForce =
                    Force.newtons (4000 * controls.speeding)

                brake =
                    if controls.braking then
                        Force.newtons 4000

                    else
                        Quantity.zero

                wheel1 =
                    { w1 | steering = Angle.degrees (20 * controls.steering), engineForce = engineForce, brake = brake }

                wheel2 =
                    { w2 | steering = Angle.degrees (20 * controls.steering), engineForce = engineForce, brake = brake }

                wheel3 =
                    { w3 | engineForce = engineForce, brake = brake }

                wheel4 =
                    { w4 | engineForce = engineForce, brake = brake }
            in
            updateSuspension dt world (Body.frame car) car [ wheel1, wheel2, wheel3, wheel4 ] car [] 0

        _ ->
            car


updateSuspension : Duration -> World Data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> List Wheel -> Body Data -> List Wheel -> Int -> Body Data
updateSuspension dt world frame originalCar currentWheels updatedCar updatedWheels numWheelsOnGround =
    case currentWheels of
        [] ->
            updateFriction dt world frame updatedCar numWheelsOnGround updatedWheels [] [] False

        wheel :: remainingWheels ->
            let
                ray =
                    Axis3d.through wheel.chassisConnectionPoint carSettings.downDirection
                        |> Axis3d.placeIn frame
            in
            case World.raycast ray (World.keepIf (\b -> (Body.data b).id == Obstacle) world) of
                Just { body, normal, point } ->
                    let
                        bodyFrame =
                            Body.frame body

                        contactPoint =
                            Point3d.placeIn bodyFrame point

                        contactNormal =
                            Direction3d.placeIn bodyFrame normal

                        distance =
                            Point3d.distanceFrom contactPoint (Axis3d.originPoint ray)

                        maxDistance =
                            Quantity.plus carSettings.suspensionRestLength carSettings.radius
                    in
                    if Quantity.lessThan maxDistance distance then
                        let
                            suspensionLength =
                                distance
                                    |> Quantity.minus carSettings.radius
                                    |> Quantity.clamp
                                        carSettings.minSuspensionLength
                                        carSettings.maxSuspensionLength

                            difference =
                                carSettings.suspensionRestLength
                                    |> Quantity.minus suspensionLength
                                    |> Length.inMeters

                            (Quantity projectedVelocity) =
                                Vector3d.dot
                                    (Direction3d.toVector contactNormal)
                                    (Body.velocityAt contactPoint originalCar)

                            (Quantity denominator) =
                                Vector3d.dot
                                    (Direction3d.toVector contactNormal)
                                    (Direction3d.toVector (Axis3d.direction ray))

                            ( suspensionRelativeVelocity, clippedInvContactDotSuspension ) =
                                if denominator >= -0.1 then
                                    ( 0, 1 / 0.1 )

                                else
                                    ( -projectedVelocity / denominator, -1 / denominator )

                            damping =
                                if suspensionRelativeVelocity < 0 then
                                    carSettings.dampingCompression

                                else
                                    carSettings.dampingRelaxation

                            suspensionImpulse =
                                ((carSettings.suspensionStiffness * difference * clippedInvContactDotSuspension)
                                    - (damping * suspensionRelativeVelocity)
                                )
                                    |> (*) (Body.mass originalCar |> Maybe.map Mass.inKilograms |> Maybe.withDefault 0)
                                    |> Force.newtons
                                    |> Quantity.clamp Quantity.zero carSettings.maxSuspensionForce
                                    |> Quantity.times dt
                        in
                        updateSuspension dt
                            world
                            frame
                            originalCar
                            remainingWheels
                            (Body.applyImpulse
                                suspensionImpulse
                                contactNormal
                                contactPoint
                                updatedCar
                            )
                            ({ wheel
                                | contact =
                                    Just
                                        { point = contactPoint
                                        , normal = contactNormal
                                        , body = body
                                        }
                                , suspensionLength = suspensionLength
                                , suspensionImpulse = suspensionImpulse
                             }
                                :: updatedWheels
                            )
                            (numWheelsOnGround + 1)

                    else
                        updateSuspension dt
                            world
                            frame
                            originalCar
                            remainingWheels
                            updatedCar
                            ({ wheel
                                | contact = Nothing
                                , suspensionLength = carSettings.suspensionRestLength
                             }
                                :: updatedWheels
                            )
                            numWheelsOnGround

                Nothing ->
                    updateSuspension dt
                        world
                        frame
                        originalCar
                        remainingWheels
                        updatedCar
                        ({ wheel
                            | contact = Nothing
                            , suspensionLength = carSettings.suspensionRestLength
                         }
                            :: updatedWheels
                        )
                        numWheelsOnGround


type alias WheelFriction =
    { forward : Direction3d WorldCoordinates
    , axle : Direction3d WorldCoordinates
    , sideImpulse : Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , forwardImpulse : Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
    , skidInfo : Float
    , contactPoint : Point3d Meters WorldCoordinates
    , contactBody : Body Data
    }


updateFriction : Duration -> World Data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> Int -> List Wheel -> List WheelFriction -> List Wheel -> Bool -> Body Data
updateFriction dt world frame updatedCar numWheelsOnGround currentWheels wheelFrictions updatedWheels sliding =
    case currentWheels of
        [] ->
            applyImpulses dt world frame updatedCar updatedWheels sliding wheelFrictions

        wheel :: remainingWheels ->
            case wheel.contact of
                Just { point, normal, body } ->
                    let
                        worldAxle =
                            carSettings.rightDirection
                                |> Direction3d.rotateAround (Axis3d.through wheel.chassisConnectionPoint carSettings.downDirection) wheel.steering
                                |> Direction3d.placeIn frame

                        (Quantity proj) =
                            Vector3d.dot (Direction3d.toVector normal) (Direction3d.toVector worldAxle)

                        axle =
                            Direction3d.toVector worldAxle
                                |> Vector3d.minus (Vector3d.scaleBy proj (Direction3d.toVector normal))
                                |> Vector3d.direction
                                |> Maybe.withDefault normal

                        forward =
                            Vector3d.cross (Direction3d.toVector normal) (Direction3d.toVector axle)
                                |> Vector3d.direction
                                |> Maybe.withDefault normal

                        sideImpulse =
                            resolveSingleBilateral updatedCar body point axle

                        maxImpulse =
                            if wheel.brake == Quantity.zero then
                                -- TODO: think about default rolling friction impulse
                                Quantity.zero

                            else
                                Quantity.times dt wheel.brake

                        forwardImpulse =
                            Quantity.times dt wheel.engineForce
                                |> Quantity.plus (calcRollingFriction updatedCar body point forward maxImpulse numWheelsOnGround)

                        -- Switch between active rolling (throttle), braking and non-active rolling friction (nthrottle/break)
                        maximpSide =
                            Quantity.multiplyBy carSettings.frictionSlip wheel.suspensionImpulse

                        impulseSquared =
                            Quantity.times forwardImpulse forwardImpulse
                                |> Quantity.multiplyBy 0.25
                                |> Quantity.plus (Quantity.times sideImpulse sideImpulse)

                        isSliding =
                            Quantity.lessThan (Quantity.times maximpSide maximpSide) impulseSquared

                        skidInfo =
                            if isSliding then
                                1

                            else
                                Quantity.ratio maximpSide (Quantity.sqrt impulseSquared)
                    in
                    updateFriction
                        dt
                        world
                        frame
                        updatedCar
                        numWheelsOnGround
                        remainingWheels
                        ({ forward = forward
                         , axle = axle
                         , sideImpulse = sideImpulse
                         , forwardImpulse = forwardImpulse
                         , skidInfo = skidInfo
                         , contactPoint = point
                         , contactBody = body
                         }
                            :: wheelFrictions
                        )
                        (wheel :: updatedWheels)
                        (sliding || isSliding)

                Nothing ->
                    updateFriction
                        dt
                        world
                        frame
                        updatedCar
                        numWheelsOnGround
                        remainingWheels
                        wheelFrictions
                        (wheel :: updatedWheels)
                        sliding


applyImpulses : Duration -> World Data -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> List Wheel -> Bool -> List WheelFriction -> Body Data
applyImpulses dt world frame car wheels sliding wheelFrictions =
    case wheelFrictions of
        [] ->
            rotateWheels dt frame car wheels []

        friction :: remainingFrictions ->
            let
                centerOfMass =
                    Body.centerOfMass car
                        |> Point3d.placeIn frame

                up =
                    Direction3d.reverse carSettings.downDirection
                        |> Direction3d.placeIn frame

                verticalDistance =
                    Vector3d.from friction.contactPoint centerOfMass
                        |> Vector3d.componentIn up
                        |> Quantity.multiplyBy (1 - carSettings.rollInfluence)

                closerToCenterOfMass =
                    Point3d.translateIn up verticalDistance friction.contactPoint

                forwardImpulse =
                    if sliding then
                        Quantity.multiplyBy friction.skidInfo friction.forwardImpulse

                    else
                        friction.forwardImpulse

                sideImpulse =
                    if sliding then
                        Quantity.multiplyBy friction.skidInfo friction.sideImpulse

                    else
                        friction.sideImpulse

                newCar =
                    car
                        |> Body.applyImpulse forwardImpulse friction.forward friction.contactPoint
                        |> Body.applyImpulse sideImpulse friction.axle closerToCenterOfMass

                -- TODO: apply the reverse of the sideImpulse on the ground object too, for now assume it is static
            in
            applyImpulses
                dt
                world
                frame
                newCar
                wheels
                sliding
                remainingFrictions


rotateWheels : Duration -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> List Wheel -> List Wheel -> Body Data
rotateWheels dt frame car wheels updatedWheels =
    case wheels of
        [] ->
            Body.withData
                { id = Car (List.reverse updatedWheels)
                , entity = (Body.data car).entity
                }
                car

        wheel :: remainingWheels ->
            case wheel.contact of
                Just { point, normal } ->
                    let
                        velocity =
                            Body.velocityAt point car

                        forward =
                            Direction3d.placeIn frame carSettings.forwardDirection

                        proj =
                            Direction3d.componentIn normal forward

                        (Quantity proj2) =
                            forward
                                |> Direction3d.toVector
                                |> Vector3d.minus (Vector3d.withLength (Quantity proj) normal)
                                |> Vector3d.dot velocity

                        deltaRotation =
                            Quantity (proj2 * Duration.inSeconds dt / Length.inMeters carSettings.radius)

                        newWheel =
                            { wheel
                                | deltaRotation = deltaRotation
                                , rotation = Quantity.plus wheel.rotation wheel.deltaRotation
                            }
                    in
                    rotateWheels dt frame car remainingWheels (newWheel :: updatedWheels)

                Nothing ->
                    let
                        deltaRotation =
                            Quantity.multiplyBy 0.99 wheel.deltaRotation

                        newWheel =
                            { wheel
                              -- damping when not in contact
                                | deltaRotation = Quantity.multiplyBy 0.99 wheel.deltaRotation
                                , rotation = Quantity.plus wheel.rotation deltaRotation
                            }
                    in
                    rotateWheels dt frame car remainingWheels (newWheel :: updatedWheels)


resolveSingleBilateral : Body Data -> Body Data -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
resolveSingleBilateral body1 body2 point direction =
    let
        velocity1 =
            Body.velocityAt point body1

        velocity2 =
            Body.velocityAt point body2

        (Quantity relativeVelocity) =
            Vector3d.dot (Vector3d.minus velocity2 velocity1) (Direction3d.toVector direction)

        contactDamping =
            0.2

        invMass1 =
            case Body.mass body1 of
                Just mass ->
                    1 / Mass.inKilograms mass

                Nothing ->
                    0

        invMass2 =
            case Body.mass body2 of
                Just mass ->
                    1 / Mass.inKilograms mass

                Nothing ->
                    0

        massTerm =
            1 / (invMass1 + invMass2)
    in
    Quantity (-contactDamping * relativeVelocity * massTerm)


calcRollingFriction : Body Data -> Body Data -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds) -> Int -> Quantity Float (Quantity.Product Force.Newtons Duration.Seconds)
calcRollingFriction body1 body2 point forward maxImpulse numWheelsOnGround =
    let
        velocity1 =
            Body.velocityAt point body1

        velocity2 =
            Body.velocityAt point body2

        (Quantity relativeVelocity) =
            Vector3d.dot (Vector3d.minus velocity2 velocity1) (Direction3d.toVector forward)

        denom1 =
            computeImpulseDenominator body1 point forward

        denom2 =
            computeImpulseDenominator body2 point forward
    in
    Quantity (-relativeVelocity / (denom1 + denom2) / toFloat numWheelsOnGround)
        |> Quantity.clamp (Quantity.negate maxImpulse) maxImpulse


computeImpulseDenominator : Body Data -> Point3d Meters WorldCoordinates -> Direction3d WorldCoordinates -> Float
computeImpulseDenominator body point normal =
    let
        position =
            Point3d.placeIn (Body.frame body) (Body.centerOfMass body)

        r0 =
            Vector3d.from position point

        c0 =
            Vector3d.cross r0 (Direction3d.toVector normal)

        vec =
            Vector3d.cross (Body.transformWithInverseInertia body c0) r0

        (Quantity dot) =
            Vector3d.dot (Direction3d.toVector normal) vec
    in
    case Body.mass body of
        Just mass ->
            1 / Mass.inKilograms mass + dot

        Nothing ->
            dot


base : Body Data
base =
    -- TODO: better car shape
    let
        offset =
            Point3d.meters -35 0 1.1

        size =
            ( Length.meters 2.8, Length.meters 2, Length.meters 0.5 )

        shape =
            Block3d.centeredOn Frame3d.atOrigin size

        material =
            Material.nonmetal
                { baseColor = Color.rgb255 0 180 180
                , roughness = 0.5
                }

        entity =
            Scene3d.group
                [ Scene3d.blockWithShadow material
                    (Block3d.centeredOn Frame3d.atOrigin size)
                , Scene3d.block material
                    (Block3d.centeredOn
                        (Frame3d.atOrigin
                            |> Frame3d.translateBy
                                (Vector3d.withLength (Length.meters 0.5)
                                    (Frame3d.zDirection Frame3d.atOrigin)
                                )
                            |> Frame3d.translateBy
                                (Vector3d.withLength (Length.meters -0.3)
                                    (Frame3d.xDirection Frame3d.atOrigin)
                                )
                        )
                        ( Length.meters 2.2, Length.meters 1.8, Length.meters 0.5 )
                    )
                ]

        wheels =
            [ { defaultWheel | chassisConnectionPoint = Point3d.meters 1 1 0 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters 1 -1 0 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters -1 1 0 }
            , { defaultWheel | chassisConnectionPoint = Point3d.meters -1 -1 0 }
            ]
    in
    { id = Car wheels
    , entity = entity
    }
        |> Body.block shape
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 1190))
        |> Body.moveTo offset


wheelRadius =
    Length.meters 0.8


ball : Body Data
ball =
    let
        shape =
            Sphere3d.atOrigin (Length.meters 2)

        entity =
            Scene3d.sphereWithShadow (Material.uniform Materials.chromium) shape
    in
    { id = Ball
    , entity = entity
    }
        |> Body.sphere shape
        |> Body.withMaterial (Physics.Material.custom { friction = 0.3, bounciness = 0.8 })
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 1))
        |> Body.moveTo (Point3d.meters 0 0 2)


floorSize : Length
floorSize =
    Length.meters 200


floor : Material.Texture Color -> Body Data
floor texture =
    let
        point x y =
            Point3d.meters x y 0

        texturedMaterial =
            Material.texturedNonmetal
                { baseColor = texture
                , roughness = Material.constant 0.25
                }

        length =
            180

        half =
            length / 2
    in
    Body.plane
        { id = Obstacle
        , --entity = Scene3d.group entities
          entity =
            Scene3d.quad texturedMaterial
                (point -half -half)
                (point -half half)
                (point half half)
                (point half -half)
        }


roomSize =
    { width = 131
    , length = 161
    , height = 40
    }


panels : List (Body Data)
panels =
    let
        goalSize =
            { width = roomSize.width * 0.17
            , height = roomSize.width * 0.08
            , depth = 15
            }

        buildGoal =
            List.concat
                [ --left wall
                  buildPanel Both ((roomSize.width / 2) - (goalSize.width / 2)) roomSize.height
                    |> List.map (Body.translateBy (Vector3d.meters 0 ((roomSize.width / 4) + (goalSize.width / 4)) 0))

                --right wall
                , buildPanel Both ((roomSize.width / 2) - (goalSize.width / 2)) roomSize.height
                    |> List.map (Body.translateBy (Vector3d.meters 0 (-(roomSize.width / 4) - (goalSize.width / 4)) 0))

                -- above goal
                , buildPanel Top goalSize.width (roomSize.height - goalSize.height)
                    |> List.map (Body.translateBy (Vector3d.meters 0 0 goalSize.height))

                -- back of goal
                , buildPanel Both goalSize.width goalSize.height
                    |> List.map (Body.translateBy (Vector3d.meters -goalSize.depth 0 0))
                , [ --goal sides
                    buildPlane goalSize.depth goalSize.height
                        |> Body.translateBy (Vector3d.meters (-goalSize.width / 2) (goalSize.depth / 2) (goalSize.height / 2))
                        |> Body.rotateAround Axis3d.z (Angle.degrees 90)
                  , buildPlane goalSize.depth goalSize.height
                        |> Body.translateBy (Vector3d.meters (-goalSize.width / 2) (-goalSize.depth / 2) (goalSize.height / 2))
                        |> Body.rotateAround Axis3d.z (Angle.degrees -90)

                  -- goal ceiling
                  , buildPlane goalSize.width goalSize.depth
                        |> Body.translateBy (Vector3d.meters -goalSize.height 0 (-goalSize.depth / 2))
                        |> Body.rotateAround Axis3d.y (Angle.degrees 90)
                  ]
                ]

        cornerWallDistance =
            10

        cornerWallLength =
            -- a^2 + b^2 = c^2
            sqrt ((cornerWallDistance ^ 2) * 2) * 2
    in
    List.concat
        [ -- front
          buildGoal
            |> List.map (Body.rotateAround Axis3d.z (Angle.degrees 180))
            |> List.map (Body.translateBy (Vector3d.meters (roomSize.length / 2) 0 0))
        , buildPanel Both cornerWallLength roomSize.height
            |> List.map (Body.rotateAround Axis3d.z (Angle.degrees (180 - 45)))
            |> List.map (Body.translateBy (Vector3d.meters (roomSize.length / 2 - cornerWallDistance) (-roomSize.width / 2 + cornerWallDistance) 0))

        -- right
        , buildPanel Both roomSize.length roomSize.height
            |> List.map (Body.rotateAround Axis3d.z (Angle.degrees 90))
            |> List.map (Body.translateBy (Vector3d.meters 0 (-roomSize.width / 2) 0))
        , buildPanel Both cornerWallLength roomSize.height
            |> List.map (Body.rotateAround Axis3d.z (Angle.degrees 45))
            |> List.map (Body.translateBy (Vector3d.meters (-roomSize.length / 2 + cornerWallDistance) (-roomSize.width / 2 + cornerWallDistance) 0))

        --back
        , buildGoal
            |> List.map (Body.translateBy (Vector3d.meters (-roomSize.length / 2) 0 0))
        , buildPanel Both cornerWallLength roomSize.height
            |> List.map (Body.rotateAround Axis3d.z (Angle.degrees -45))
            |> List.map (Body.translateBy (Vector3d.meters (-roomSize.length / 2 + cornerWallDistance) (roomSize.width / 2 - cornerWallDistance) 0))

        -- left
        , buildPanel Both roomSize.length roomSize.height
            |> List.map (Body.rotateAround Axis3d.z (Angle.degrees -90))
            |> List.map (Body.translateBy (Vector3d.meters 0 (roomSize.width / 2) 0))
        , buildPanel Both cornerWallLength roomSize.height
            |> List.map (Body.rotateAround Axis3d.z (Angle.degrees (-180 + 45)))
            |> List.map (Body.translateBy (Vector3d.meters (roomSize.length / 2 - cornerWallDistance) (roomSize.width / 2 - cornerWallDistance) 0))
        ]


buildPlane w h =
    let
        block =
            Block3d.centeredOn Frame3d.atOrigin
                ( meters h, meters w, meters 0.1 )
    in
    Body.block block
        { id = Obstacle
        , entity =
            Scene3d.block
                (Material.uniform Materials.chromium)
                block
        }
        |> Body.rotateAround Axis3d.y (Angle.degrees 90)


type Slope
    = Top
    | Bottom
    | Both
    | Neither


buildPanel : Slope -> Float -> Float -> List (Body Data)
buildPanel type_ width height =
    -- A wall with ramp(s)
    let
        -- TODO: Body.compound, Scene3d.group
        --
        wall =
            buildPlane width height
                |> Body.translateBy (Vector3d.meters 0 0 (height / 2))

        ( slopeRadius, stepCount ) =
            ( 5, 30 )

        bottomSlope =
            List.range 1 (stepCount - 1)
                |> List.map toFloat
                |> List.map
                    (\step ->
                        let
                            angle =
                                (90 / stepCount) * step

                            vec =
                                Vector3d.meters
                                    (cos (degrees angle) * slopeRadius)
                                    0
                                    -(sin (degrees angle) * slopeRadius)
                        in
                        buildPlane width 2
                            |> Body.translateBy
                                (Vector3d.meters slopeRadius 0 slopeRadius)
                            |> (\body ->
                                    Body.translateBy (Vector3d.placeIn (Body.frame body) vec)
                                        body
                               )
                            |> (\body ->
                                    Body.rotateAround
                                        (Body.frame body |> Frame3d.yAxis)
                                        (Angle.degrees (-90 + angle))
                                        body
                               )
                    )

        slopeAxis =
            Axis3d.through (Point3d.meters slopeRadius 0 slopeRadius) Direction3d.x

        topSlope =
            bottomSlope
                |> List.map (Body.rotateAround slopeAxis (Angle.degrees 180))
                |> List.map (Body.translateBy (Vector3d.meters 0 0 (height - (slopeRadius * 2))))
    in
    case type_ of
        Top ->
            wall :: topSlope

        Bottom ->
            wall :: bottomSlope

        Both ->
            wall
                :: bottomSlope
                ++ topSlope

        Neither ->
            [ wall ]


getTransformedDrawable : Body Data -> Scene3d.Entity WorldCoordinates
getTransformedDrawable body =
    Scene3d.placeIn (Body.frame body) (Body.data body).entity


applySpeed : Float -> Frame3d Meters WorldCoordinates { defines : BodyCoordinates } -> Body Data -> Body Data
applySpeed speed baseFrame body =
    let
        forward =
            Frame3d.yDirection baseFrame

        up =
            Frame3d.zDirection baseFrame

        wheelPoint =
            Frame3d.originPoint (Body.frame body)

        pointOnTheWheel =
            wheelPoint
                |> Point3d.translateBy
                    (Vector3d.withLength wheelRadius up)

        pointUnderTheWheel =
            wheelPoint
                |> Point3d.translateBy
                    (Vector3d.withLength wheelRadius (Direction3d.reverse up))
    in
    body
        |> Body.applyForce (Force.newtons (-speed * 10000))
            forward
            pointOnTheWheel
        |> Body.applyForce (Force.newtons (-speed * 10000))
            (Direction3d.reverse forward)
            pointUnderTheWheel
