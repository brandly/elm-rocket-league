module Main exposing (main)

import Acceleration
import Angle
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Camera3d
import Direction3d
import Duration
import Force
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
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d
import Quantity exposing (Quantity(..))
import Scene3d
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d
import Task
import Vector3d
import Viewpoint3d


type alias Data =
    { entity : Scene3d.Entity BodyCoordinates
    , id : EntityId
    }


type EntityId
    = Arena
    | Car
    | Ball
    | Wheel WheelId


type WheelId
    = FrontRight
    | FrontLeft
    | BackRight
    | BackLeft


type alias Model =
    { world : World Data
    , rockets : Bool
    , steering : Float -- -1, 0, 1
    , speeding : Float -- -1, 0, 1
    , screenWidth : Float
    , screenHeight : Float
    }


type Msg
    = Tick Float
    | Resize Float Float
    | KeyDown Command
    | KeyUp Command


type Command
    = Jump
    | Rocket
    | Steer Float
    | Speed Float


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { world = initialWorld
      , rockets = False
      , steering = 0
      , speeding = 0
      , screenWidth = 0
      , screenHeight = 0
      }
    , Task.perform
        (\{ viewport } -> Resize viewport.width viewport.height)
        Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick _ ->
            -- TODO: measure tick time instead of 1/60?
            ( { model
                | world =
                    let
                        baseFrame =
                            model.world
                                |> World.bodies
                                |> List.filter (\b -> (Body.data b).id == Car)
                                |> List.head
                                |> Maybe.map Body.frame
                                |> Maybe.withDefault Frame3d.atOrigin
                    in
                    model.world
                        |> World.constrain (constrainCar model.steering)
                        |> World.update
                            (\body ->
                                case (Body.data body).id of
                                    Car ->
                                        if model.rockets then
                                            body
                                                |> Body.applyForce (Force.newtons 50000)
                                                    -- TODO: sort out coords, remove need for this reverse
                                                    (body |> Body.frame >> Frame3d.yDirection >> Direction3d.reverse)
                                                    (Body.originPoint body)

                                        else
                                            body

                                    Wheel FrontRight ->
                                        applySpeed model.speeding baseFrame body

                                    Wheel FrontLeft ->
                                        applySpeed model.speeding baseFrame body

                                    _ ->
                                        body
                            )
                        |> World.simulate (Duration.seconds (1 / 60))
              }
            , Cmd.none
            )

        KeyDown Jump ->
            ( { model
                | world =
                    World.update
                        (\body ->
                            if (Body.data body).id == Car then
                                body
                                    |> Body.applyForce (Force.newtons 400000)
                                        -- TODO: add direction modifier keys
                                        (body |> Body.frame >> Frame3d.zDirection)
                                        (Body.originPoint body)

                            else
                                body
                        )
                        model.world
              }
            , Cmd.none
            )

        KeyUp Jump ->
            ( model, Cmd.none )

        KeyDown Rocket ->
            ( { model | rockets = True }, Cmd.none )

        KeyUp Rocket ->
            ( { model | rockets = False }, Cmd.none )

        KeyDown (Steer k) ->
            ( { model | steering = k }, Cmd.none )

        KeyUp (Steer k) ->
            ( { model
                | steering =
                    if k == model.steering then
                        0

                    else
                        model.steering
              }
            , Cmd.none
            )

        KeyDown (Speed k) ->
            ( { model | speeding = k }, Cmd.none )

        KeyUp (Speed k) ->
            ( { model
                | speeding =
                    if k == model.speeding then
                        0

                    else
                        model.speeding
              }
            , Cmd.none
            )

        Resize width height ->
            ( { model | screenWidth = width, screenHeight = height }
            , Cmd.none
            )


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

                    _ ->
                        Json.Decode.fail ("Unrecognized key: " ++ string)
            )


view : Model -> Html Msg
view { world, screenWidth, screenHeight } =
    let
        camera =
            Camera3d.perspective
                { viewpoint =
                    Viewpoint3d.orbit
                        { focalPoint =
                            let
                                car =
                                    world
                                        |> World.bodies
                                        |> List.filter (Body.data >> .id >> (==) Car)
                                        |> List.head
                            in
                            car
                                |> Maybe.map (Body.frame >> Frame3d.originPoint)
                                |> Maybe.withDefault (Point3d.meters 0 0 0)
                        , groundPlane = SketchPlane3d.xy
                        , azimuth = Angle.degrees 90
                        , elevation = Angle.degrees 12
                        , distance = Quantity 60.0
                        }
                , verticalFieldOfView = Angle.degrees 24
                }

        drawables =
            List.map getTransformedDrawable (World.bodies world)

        sunlight =
            Scene3d.directionalLight Scene3d.castsShadows
                { chromaticity = Scene3d.sunlight
                , intensity = Illuminance.lux 10000
                , direction = Direction3d.xyZ (Angle.degrees 45) (Angle.degrees -60)
                }

        daylight =
            Scene3d.overheadLighting
                { upDirection = Direction3d.z
                , chromaticity = Scene3d.daylight
                , intensity = Illuminance.lux 15000
                }
    in
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        ]
        [ Scene3d.toHtml
            { dimensions = ( pixels screenWidth, pixels screenHeight )
            , antialiasing = Scene3d.multisampling
            , camera = camera
            , lights = Scene3d.twoLights sunlight daylight
            , exposure = Scene3d.maxLuminance (Luminance.nits 10000)
            , toneMapping = Scene3d.noToneMapping
            , whiteBalance = Scene3d.daylight
            , background = Scene3d.transparentBackground
            , clipDepth = meters 0.1
            }
            drawables
        ]


initialWorld : World Data
initialWorld =
    let
        earthGravity =
            Acceleration.metersPerSecondSquared 9.807
    in
    World.empty
        |> World.withGravity earthGravity Direction3d.negativeZ
        |> World.add floor
        |> addCar
        |> addBall


addCar : World Data -> World Data
addCar world =
    let
        offset =
            Point3d.meters 0 0 3

        shape =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 3, Length.meters 5.26, Length.meters 2 )

        entity =
            Scene3d.block Scene3d.castsShadows Materials.gold shape

        body =
            { id = Car
            , entity = entity
            }
                |> Body.block shape
                |> Body.withBehavior (Body.dynamic (Mass.kilograms 1190))
                |> Body.moveTo offset
    in
    world
        |> World.add body
        |> World.add
            (wheel BackLeft
                |> Body.moveTo offset
                |> Body.translateBy (Vector3d.meters 2.5 2.5 -1)
            )
        |> World.add
            (wheel BackRight
                |> Body.moveTo offset
                |> Body.translateBy (Vector3d.meters -2.5 2.5 -1)
            )
        |> World.add
            (wheel FrontRight
                |> Body.moveTo offset
                |> Body.translateBy (Vector3d.meters -2.5 -2.5 -1)
            )
        |> World.add
            (wheel FrontLeft
                |> Body.moveTo offset
                |> Body.translateBy (Vector3d.meters 2.5 -2.5 -1)
            )


wheelRadius =
    Length.meters 0.8


wheel : WheelId -> Body Data
wheel id =
    let
        sphere =
            Sphere3d.atOrigin wheelRadius

        entity =
            Scene3d.sphere Scene3d.castsShadows (Material.uniform Materials.chromium) sphere
    in
    Body.sphere sphere
        { id = Wheel id
        , entity = entity
        }
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 2))


addBall : World Data -> World Data
addBall =
    let
        shape =
            Sphere3d.atOrigin (Length.meters 5)

        entity =
            Scene3d.sphere Scene3d.castsShadows (Material.uniform Materials.chromium) shape
    in
    { id = Ball
    , entity = entity
    }
        |> Body.sphere shape
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 200))
        |> Body.moveTo (Point3d.meters 0 -30 10)
        |> World.add


floorSize : Length
floorSize =
    Length.meters 500


floor : Body Data
floor =
    let
        point x y =
            Point3d.meters x y 0

        size =
            Length.inMeters floorSize

        entity =
            Scene3d.quad Scene3d.doesNotCastShadows
                (Material.uniform Materials.blackPlastic)
                (point -size -size)
                (point -size size)
                (point size size)
                (point size -size)
    in
    { id = Arena, entity = entity }
        |> Body.plane
        |> Body.moveTo
            (Point3d.meters 0 0 0)


getTransformedDrawable : Body Data -> Scene3d.Entity WorldCoordinates
getTransformedDrawable body =
    Scene3d.placeIn (Body.frame body) (Body.data body).entity


constrainCar : Float -> Body Data -> Body Data -> List Constraint
constrainCar steering b1 b2 =
    let
        steeringAngle =
            steering * pi / 8

        dx =
            cos steeringAngle

        dy =
            sin steeringAngle

        backLeftHinge =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters 2.5 2.5 -1)
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )

        backRightHinge =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters -2.5 2.5 -1)
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        frontRightHinge =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters -2.5 -2.5 -1)
                    (Direction3d.unsafe { x = -dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = 1, y = 0, z = 0 })
                )

        frontLeftHinge =
            Constraint.hinge
                (Axis3d.through
                    (Point3d.meters 2.5 -2.5 -1)
                    (Direction3d.unsafe { x = -dx, y = dy, z = 0 })
                )
                (Axis3d.through
                    Point3d.origin
                    (Direction3d.unsafe { x = -1, y = 0, z = 0 })
                )
    in
    case ( (Body.data b1).id, (Body.data b2).id ) of
        ( Car, Wheel BackLeft ) ->
            [ backLeftHinge ]

        ( Car, Wheel BackRight ) ->
            [ backRightHinge ]

        ( Car, Wheel FrontRight ) ->
            [ frontRightHinge ]

        ( Car, Wheel FrontLeft ) ->
            [ frontLeftHinge ]

        _ ->
            []


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
