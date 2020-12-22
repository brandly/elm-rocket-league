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
import Dict exposing (Dict)
import Direction3d exposing (Direction3d)
import Duration exposing (Duration)
import Force exposing (Force)
import Frame3d exposing (Frame3d)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Illuminance
import Json.Decode
import Length exposing (Length, Meters, inMeters, meters)
import Luminance
import Mass
import Materials
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.Material
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d exposing (Point3d)
import Quantity exposing (Quantity(..))
import Refill exposing (Refill, Size(..))
import Scene3d
import Scene3d.Light as Light
import Scene3d.Material as Material
import SketchPlane3d
import Sphere3d
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
    , player : Player
    , lastTick : Float
    , refills : List Refill
    , status : Status
    , score :
        { blue : Int
        , orange : Int
        }
    }


type Status
    = Preparing
    | Live
    | Paused Status
    | Replay Float


type alias Player =
    { controls : Controls
    , boostTank : Float
    , focus : CameraFocus
    }


type alias Controls =
    { rockets : Bool
    , steering : Float -- -1, 0, 1
    , speeding : Float -- -1, 0, 1
    , braking : Bool
    }


boostSettings =
    { reloadTime = 10000
    , initial = 45
    , max = 100
    , refill = 12
    }


refillIsActive : Float -> Refill -> Bool
refillIsActive =
    Refill.isActive boostSettings.reloadTime


refillSize : Refill -> Float
refillSize refill =
    case refill.size of
        FullRefill ->
            boostSettings.max

        SmallRefill ->
            boostSettings.refill


type Msg
    = Tick Float
    | Resize Float Float
    | StartGame
    | KeyDown Command
    | KeyUp Command
    | TextureResponse (Result WebGL.Texture.Error (Material.Texture Color))


type Command
    = Jump
    | Rocket
    | Steer Float
    | Speed Float
    | ToggleCam
    | TogglePause


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
    { screen : Screen
    , screenSize : ScreenSize
    }


type alias Config =
    { texture : Material.Texture Color
    }


type Screen
    = Loading
    | LoadingError String
    | Menu Config
    | Playing Game Config


init : () -> ( Model, Cmd Msg )
init _ =
    ( { screenSize = { width = 0, height = 0 }, screen = Loading }
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
        mapGame : (Game -> Game) -> Model
        mapGame fn =
            case model.screen of
                Playing g config ->
                    { model | screen = Playing (fn g) config }

                _ ->
                    model

        mapPlayer : (Player -> Player) -> Model
        mapPlayer fn =
            mapGame (\g -> { g | player = fn g.player })

        mapControls : (Controls -> Controls) -> Model
        mapControls fn =
            mapPlayer (\p -> { p | controls = fn p.controls })
    in
    case ( model.screen, msg ) of
        ( _, Resize w h ) ->
            ( { model | screenSize = { width = w, height = h } }
            , Cmd.none
            )

        ( Menu config, StartGame ) ->
            ( { model | screen = Playing (initGame config) config }, Cmd.none )

        ( Menu _, _ ) ->
            ( model, Cmd.none )

        ( Playing game config, Tick tick ) ->
            ( updateGame config tick
                |> mapGame
            , Cmd.none
            )

        ( Playing game _, KeyDown cmd ) ->
            case cmd of
                Jump ->
                    ( mapGame (\g -> { g | world = World.update applyJump g.world })
                    , Cmd.none
                    )

                Rocket ->
                    ( mapControls (\c -> { c | rockets = True }), Cmd.none )

                Steer k ->
                    ( mapControls (\c -> { c | steering = k }), Cmd.none )

                Speed k ->
                    ( mapControls (\c -> { c | speeding = k }), Cmd.none )

                ToggleCam ->
                    ( mapPlayer
                        (\p ->
                            { p
                                | focus =
                                    if p.focus == BallCam then
                                        ForwardCam

                                    else
                                        BallCam
                            }
                        )
                    , Cmd.none
                    )

                TogglePause ->
                    case game.status of
                        Paused status ->
                            ( mapGame (\g -> { g | status = status }), Cmd.none )

                        _ ->
                            ( mapGame (\g -> { g | status = Paused g.status }), Cmd.none )

        ( Playing _ _, KeyUp cmd ) ->
            case cmd of
                Jump ->
                    ( model, Cmd.none )

                Rocket ->
                    ( mapControls (\c -> { c | rockets = False }), Cmd.none )

                Steer k ->
                    let
                        steering c =
                            if k == c.steering then
                                0

                            else
                                c.steering
                    in
                    ( mapControls (\c -> { c | steering = steering c }), Cmd.none )

                Speed k ->
                    let
                        speeding c =
                            if k == c.speeding then
                                0

                            else
                                c.speeding
                    in
                    ( mapControls (\c -> { c | speeding = speeding c }), Cmd.none )

                ToggleCam ->
                    ( model, Cmd.none )

                TogglePause ->
                    ( model, Cmd.none )

        ( Playing _ _, TextureResponse _ ) ->
            ( model, Cmd.none )

        ( Playing _ _, StartGame ) ->
            ( model, Cmd.none )

        ( Loading, TextureResponse (Ok texture) ) ->
            ( { model | screen = Menu { texture = texture } }, Cmd.none )

        ( Loading, TextureResponse (Err err) ) ->
            ( { model
                | screen =
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


updateGame : Config -> Float -> Game -> Game
updateGame config tick game =
    let
        currentTick =
            game.lastTick + tick
    in
    case game.status of
        Paused _ ->
            game

        Replay startTime ->
            if currentTick - startTime > 3000 then
                let
                    g =
                        initGame config
                in
                { g | score = game.score }

            else
                { game
                    | world =
                        game.world
                            |> World.update (updateBody game tick)
                            |> World.simulate (Duration.seconds (tick / 1000))
                    , lastTick = currentTick
                }

        Preparing ->
            if currentTick > 2000 then
                { game
                    | status = Live
                    , world = World.add base game.world
                    , lastTick = currentTick
                }

            else
                { game | lastTick = currentTick }

        Live ->
            let
                player =
                    game.player

                controls =
                    player.controls

                carPoint =
                    game.world
                        |> World.bodies
                        |> List.filter (Body.data >> .id >> isCar)
                        |> List.head
                        |> Maybe.map Body.originPoint
                        |> Maybe.withDefault (Point3d.meters 0 0 0)

                ballPoint =
                    game.world
                        |> World.bodies
                        |> List.filter (Body.data >> .id >> isBall)
                        |> List.head
                        |> Maybe.map Body.originPoint
                        |> Maybe.withDefault (Point3d.meters 0 0 0)

                fullTank =
                    player.boostTank >= boostSettings.max

                carHits : Refill -> Bool
                carHits { point, time } =
                    not fullTank
                        && ((currentTick - time) > boostSettings.reloadTime)
                        && (Point3d.distanceFrom carPoint point |> Length.inMeters)
                        < 2.5

                applyCarHit : Refill -> Refill
                applyCarHit refill =
                    if refillIsActive currentTick refill && carHits refill then
                        { refill | time = currentTick }

                    else
                        refill

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
                                |> List.map refillSize
                                |> List.sum
                    in
                    min boostSettings.max (adding + boostTank)

                blueGoal =
                    (Length.inMeters << Point3d.xCoordinate) ballPoint > (roomSize.length / 2)

                orangeGoal =
                    (Length.inMeters << Point3d.xCoordinate) ballPoint < (-roomSize.length / 2)

                incrementIf bool num =
                    if bool then
                        num + 1

                    else
                        num
            in
            { game
                | world =
                    game.world
                        |> World.update (updateBody game tick)
                        |> World.simulate (Duration.seconds (tick / 1000))
                , player =
                    { player
                        | boostTank =
                            player.boostTank
                                |> applyRefills
                                |> applyRockets
                    }
                , lastTick = currentTick
                , refills = game.refills |> List.map applyCarHit
                , score =
                    { blue = incrementIf blueGoal game.score.blue
                    , orange = incrementIf orangeGoal game.score.orange
                    }
                , status =
                    if blueGoal || orangeGoal then
                        Replay currentTick

                    else
                        game.status
            }


updateBody : Game -> Float -> Body Data -> Body Data
updateBody game tick body =
    case (Body.data body).id of
        Car wheels ->
            let
                boost =
                    if game.player.controls.rockets && game.player.boostTank > 0 then
                        Body.applyForce (Force.newtons 30000)
                            (Direction3d.placeIn (Body.frame body) carSettings.forwardDirection)
                            (Body.originPoint body)

                    else
                        identity
            in
            simulateCar (Duration.milliseconds tick) game.world game.player.controls wheels body
                |> boost

        _ ->
            body


initGame : Config -> Game
initGame config =
    { world =
        initialWorld
            |> World.add (floor config.texture)
    , player =
        { controls =
            { rockets = False
            , steering = 0
            , speeding = 0
            , braking = False
            }
        , boostTank = boostSettings.initial
        , focus = BallCam
        }
    , lastTick = 0
    , refills =
        Refill.init
            { startTime = -boostSettings.reloadTime
            , measure = roomSize.length / 10.8
            }
    , status = Preparing
    , score = { blue = 0, orange = 0 }
    }


applyJump : Body Data -> Body Data
applyJump body =
    -- TODO: restrict to double jump
    -- TODO: raycast to see if on ground
    -- TODO: handle arrow directions
    case (Body.data body).id of
        Car _ ->
            body
                |> Body.applyForce (Force.newtons 400000)
                    -- TODO: add direction modifier keys
                    (body |> Body.frame >> Frame3d.zDirection)
                    (Body.originPoint body)

        _ ->
            body


subscriptions : Model -> Sub Msg
subscriptions { screen } =
    case screen of
        Playing game _ ->
            case game.status of
                Paused _ ->
                    Sub.batch
                        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
                        , Events.onKeyDown (keyDecoder KeyDown)
                        , Events.onKeyUp (keyDecoder KeyUp)
                        ]

                _ ->
                    Sub.batch
                        [ Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
                        , Events.onAnimationFrameDelta Tick
                        , Events.onKeyDown (keyDecoder KeyDown)
                        , Events.onKeyUp (keyDecoder KeyUp)
                        ]

        _ ->
            Events.onResize (\w h -> Resize (toFloat w) (toFloat h))


controlDict : Dict String Command
controlDict =
    Dict.fromList
        [ ( "ArrowLeft", Steer -1 )
        , ( "ArrowRight", Steer 1 )
        , ( "ArrowUp", Speed 1 )
        , ( "ArrowDown", Speed -1 )
        , ( " ", Jump )
        , ( "Shift", Rocket )
        , ( "c", ToggleCam )
        , ( "p", TogglePause )
        ]


keyDecoder : (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                Dict.get string controlDict
                    |> Maybe.map (toMsg >> Json.Decode.succeed)
                    |> Maybe.withDefault (Json.Decode.fail ("Unrecognized key: " ++ string))
            )


view : Model -> Html Msg
view model =
    let
        controls =
            [ ( "Drive", "Arrow keys" )
            , ( "Boost", "Shift" )
            , ( "Toggle Camera", "C" )
            , ( "Jump (buggy)", "Spacebar" )
            ]
    in
    Html.div [ Html.Attributes.class "container" ]
        (case model.screen of
            Loading ->
                [ Html.p [ Html.Attributes.class "center-popup" ] [ Html.text "Loading..." ] ]

            Menu _ ->
                [ Html.div [ Html.Attributes.class "center-popup" ]
                    [ Html.h1 [] [ Html.text "Rocket League" ]
                    , Html.div []
                        (List.map
                            (\( action, key ) ->
                                Html.div [ Html.Attributes.class "controls-row" ]
                                    [ Html.span [] [ Html.text action ]
                                    , Html.span [] [ Html.text key ]
                                    ]
                            )
                            controls
                        )
                    , Html.div [ Html.Attributes.class "btn-row" ]
                        [ Html.button [ Html.Events.onClick StartGame, Html.Attributes.class "btn-primary" ]
                            [ Html.text "Let's go!" ]
                        ]
                    ]
                ]

            Playing game _ ->
                viewGame model.screenSize game

            LoadingError error ->
                [ Html.p [] [ Html.text ("Error: " ++ error) ] ]
        )


viewGame : ScreenSize -> Game -> List (Html Msg)
viewGame { width, height } { world, player, refills, lastTick, score, status } =
    let
        car =
            world
                |> World.bodies
                |> List.filter (Body.data >> .id >> isCar)
                |> List.head

        camera =
            let
                ballBody =
                    world
                        |> World.bodies
                        |> List.filter (Body.data >> .id >> isBall)
                        |> List.head

                frameOrigin =
                    Body.frame >> Frame3d.originPoint

                defaultAngle =
                    Angle.degrees 0

                defaultPoint =
                    Point3d.meters 30 0 10

                { focalPoint, azimuth, distance, elevation } =
                    case ( player.focus, ballBody, car ) of
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

                        ( ForwardCam, Just _, Just car_ ) ->
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
                        Refill.view (refillIsActive lastTick refill) refill
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
    , Html.div [ Html.Attributes.class "hud-pane hud-top-center" ]
        [ Html.p [] [ Html.text <| "blue: " ++ String.fromInt score.blue ++ " orange: " ++ String.fromInt score.orange ]
        ]
    , Html.div [ Html.Attributes.class "hud-pane hud-bottom-right" ]
        [ Html.p
            [ Html.Attributes.style "font-size" "48px" ]
            [ Html.text (String.fromInt (round player.boostTank)) ]
        , Html.p []
            [ Html.text "BOOST" ]
        ]
    , case status of
        Replay _ ->
            Html.h1 [ Html.Attributes.class "hud-pane center-popup" ] [ Html.text "GOOAAALLLL!!!!!" ]

        _ ->
            Html.text ""
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


wheelBody : Scene3d.Entity BodyCoordinates
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
        |> (\world -> List.foldl World.add world panels)
        |> World.add ceiling
        |> World.add ball


simulateCar : Duration -> World Data -> Controls -> List Wheel -> Body Data -> Body Data
simulateCar dt world controls wheels car =
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
                { baseColor = Color.rgb255 43 142 228
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


ceiling : Body Data
ceiling =
    let
        point x y =
            Point3d.meters x y 0

        half =
            roomSize.length / 2
    in
    Body.plane
        { id = Obstacle
        , entity =
            Scene3d.quad (Material.uniform Materials.chromium)
                (point -half -half)
                (point -half half)
                (point half half)
                (point half -half)
        }
        |> Body.rotateAround Axis3d.x (Angle.degrees 180)
        |> Body.translateBy (Vector3d.meters 0 0 roomSize.height)


roomSize : { width : Float, length : Float, height : Float }
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


buildPlane : Float -> Float -> Body Data
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
