module Main exposing (main)

import Acceleration
import Angle exposing (Angle)
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Camera3d
import Direction3d exposing (Direction3d)
import Duration
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
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Scene3d
import Scene3d.Light as Light
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
    | Car (List Wheel)
    | Ball


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
    , suspensionStiffness = 30
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
                    model.world
                        |> World.update
                            (\body ->
                                case (Body.data body).id of
                                    Car wheels ->
                                        if model.rockets then
                                            body
                                                |> Body.applyForce (Force.newtons 50000)
                                                    (Direction3d.placeIn (Body.frame body) carSettings.forwardDirection)
                                                    (Body.originPoint body)

                                        else
                                            body

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
                                        |> List.filter
                                            (Body.data
                                                >> .id
                                                >> (\id ->
                                                        case id of
                                                            Car _ ->
                                                                True

                                                            _ ->
                                                                False
                                                   )
                                            )
                                        |> List.head
                            in
                            car
                                |> Maybe.map (Body.frame >> Frame3d.originPoint)
                                |> Maybe.withDefault (Point3d.meters 0 0 0)
                        , groundPlane = SketchPlane3d.xy
                        , azimuth = Angle.degrees 180
                        , elevation = Angle.degrees 12
                        , distance = Quantity 60.0
                        }
                , verticalFieldOfView = Angle.degrees 24
                }

        drawables =
            List.map getTransformedDrawable (World.bodies world)

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
    Html.div
        [ Html.Attributes.style "position" "absolute"
        , Html.Attributes.style "left" "0"
        , Html.Attributes.style "top" "0"
        ]
        [ Scene3d.custom
            { dimensions = ( pixels (Basics.floor screenWidth), pixels (Basics.floor screenHeight) )
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
        |> World.add base
        |> addBall


base : Body Data
base =
    let
        offset =
            Point3d.meters 0 0 3

        shape =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 5.26, Length.meters 3, Length.meters 2 )

        entity =
            Scene3d.block Materials.gold shape

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


addBall : World Data -> World Data
addBall =
    let
        shape =
            Sphere3d.atOrigin (Length.meters 5)

        entity =
            Scene3d.sphere (Material.uniform Materials.chromium) shape
    in
    { id = Ball
    , entity = entity
    }
        |> Body.sphere shape
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 200))
        |> Body.moveTo (Point3d.meters 30 0 5)
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
            Scene3d.quad
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
