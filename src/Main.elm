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
    = Car PlayerId (List Wheel)
    | Ball
    | Obstacle


isBall : EntityId -> Bool
isBall id =
    id == Ball


isCar : EntityId -> Bool
isCar id =
    case id of
        Car _ _ ->
            True

        _ ->
            False


isPlayersCar : Player -> EntityId -> Bool
isPlayersCar player id =
    case id of
        Car playerId _ ->
            samePlayerId playerId player.id

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
    , players : List Player
    , lastTick : Float
    , timeLeft : Quantity Float Duration.Seconds
    , refills : List Refill
    , status : Status
    , score :
        { blue : Int
        , orange : Int
        }
    }


type Status
    = Preparing Bool (Quantity Float Duration.Seconds)
    | Live
    | Paused Status
    | Replay Float
    | GameOver


type Driver
    = Keyboard (Dict String Command)


defaultKeyboard : Dict String Command
defaultKeyboard =
    Dict.fromList
        [ ( "ArrowLeft", Steer -1 )
        , ( "ArrowRight", Steer 1 )
        , ( "ArrowUp", Speed 1 )
        , ( "ArrowDown", Speed -1 )
        , ( " ", Jump )
        , ( "Shift", Rocket )
        , ( "c", ToggleCam )
        ]


twoPlayerDrivers : List ( Driver, Team )
twoPlayerDrivers =
    [ ( Keyboard
            (Dict.fromList
                [ ( "a", Steer -1 )
                , ( "d", Steer 1 )
                , ( "w", Speed 1 )
                , ( "s", Speed -1 )
                , ( "z", Jump )
                , ( "Shift", Rocket )
                , ( "e", ToggleCam )
                ]
            )
      , Orange
      )
    , ( Keyboard
            (Dict.fromList
                [ ( "ArrowLeft", Steer -1 )
                , ( "ArrowRight", Steer 1 )
                , ( "ArrowUp", Speed 1 )
                , ( "ArrowDown", Speed -1 )
                , ( ",", Jump )
                , ( "m", Rocket )
                , ( ".", ToggleCam )
                ]
            )
      , Blue
      )
    ]


type PlayerId
    = PlayerId Int


samePlayerId : PlayerId -> PlayerId -> Bool
samePlayerId (PlayerId a) (PlayerId b) =
    a == b


type alias Player =
    { id : PlayerId
    , team : Team
    , driver : Driver
    , controls : Controls
    , boostTank : Float
    , focus : CameraFocus
    }


type alias Controls =
    { rockets : Bool
    , steering : Float -- -1, 0, 1
    , speeding : Float -- -1, 0, 1
    , braking : Bool
    }


boostSettings : { reloadTime : Float, initial : Float, max : Float, refill : Float }
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
    | SetDrivers (List ( Driver, Team ))
    | StartGame
    | LeaveGame
    | KeyDown PlayerId Command
    | KeyUp PlayerId Command
    | TextureResponse (Result WebGL.Texture.Error (Material.Texture Color))


type Command
    = Jump
    | Rocket
    | Steer Float
    | Speed Float
    | ToggleCam
    | TogglePause


commandToString : Command -> String
commandToString cmd =
    case cmd of
        Jump ->
            "Jump"

        Rocket ->
            "Rocket"

        Steer dir ->
            if dir == -1 then
                "Turn Left"

            else if dir == 1 then
                "Turn Right"

            else
                "shrug"

        Speed dir ->
            if dir == -1 then
                "Brake"

            else if dir == 1 then
                "Gas"

            else
                "shrug"

        ToggleCam ->
            "Toggle Camera"

        TogglePause ->
            "Pause"


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


type Team
    = Orange
    | Blue


teamColor : Team -> Color
teamColor team =
    case team of
        Blue ->
            Color.rgb255 43 142 228

        Orange ->
            Color.rgb255 255 165 0


type alias Config =
    { texture : Material.Texture Color
    , drivers : List ( Driver, Team )
    }


type Screen
    = Loading
    | LoadingError String
    | Menu (World Data) Config
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


simulationStep : Duration
simulationStep =
    Duration.seconds (1 / 60)


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

        mapPlayer : PlayerId -> (Player -> Player) -> Model
        mapPlayer playerId fn =
            mapGame
                (\g ->
                    { g
                        | players =
                            List.map
                                (\player ->
                                    if samePlayerId player.id playerId then
                                        fn player

                                    else
                                        player
                                )
                                g.players
                    }
                )

        mapControls : PlayerId -> (Controls -> Controls) -> Model
        mapControls pid fn =
            mapPlayer pid (\p -> { p | controls = fn p.controls })
    in
    case ( model.screen, msg ) of
        ( _, Resize w h ) ->
            ( { model | screenSize = { width = w, height = h } }
            , Cmd.none
            )

        ( Menu world config, SetDrivers drivers ) ->
            ( { model | screen = Menu world { config | drivers = drivers } }, Cmd.none )

        ( Menu _ config, StartGame ) ->
            ( { model | screen = Playing (initGame config) config }, Cmd.none )

        ( Menu world config, Tick _ ) ->
            ( { model | screen = Menu (world |> World.simulate simulationStep) config }, Cmd.none )

        ( Menu _ _, _ ) ->
            ( model, Cmd.none )

        ( Playing _ config, Tick tick ) ->
            ( updateGame config tick
                |> mapGame
            , Cmd.none
            )

        ( Playing game _, KeyDown playerId cmd ) ->
            case cmd of
                Jump ->
                    ( mapGame (\g -> { g | world = World.update (applyJump playerId) g.world })
                    , Cmd.none
                    )

                Rocket ->
                    ( mapControls playerId (\c -> { c | rockets = True }), Cmd.none )

                Steer k ->
                    ( mapControls playerId (\c -> { c | steering = k }), Cmd.none )

                Speed k ->
                    ( mapControls playerId (\c -> { c | speeding = k }), Cmd.none )

                ToggleCam ->
                    ( mapPlayer playerId
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

        ( Playing _ _, KeyUp playerId cmd ) ->
            case cmd of
                Jump ->
                    ( model, Cmd.none )

                Rocket ->
                    ( mapControls playerId (\c -> { c | rockets = False }), Cmd.none )

                Steer k ->
                    let
                        steering c =
                            if k == c.steering then
                                0

                            else
                                c.steering
                    in
                    ( mapControls playerId (\c -> { c | steering = steering c }), Cmd.none )

                Speed k ->
                    let
                        speeding c =
                            if k == c.speeding then
                                0

                            else
                                c.speeding
                    in
                    ( mapControls playerId (\c -> { c | speeding = speeding c }), Cmd.none )

                ToggleCam ->
                    ( model, Cmd.none )

                TogglePause ->
                    ( model, Cmd.none )

        ( Playing _ _, TextureResponse _ ) ->
            ( model, Cmd.none )

        ( Playing _ _, SetDrivers _ ) ->
            ( model, Cmd.none )

        ( Playing _ _, StartGame ) ->
            ( model, Cmd.none )

        ( Playing _ config, LeaveGame ) ->
            ( { model | screen = Menu (initialWorld |> World.add (floor config.texture)) config }, Cmd.none )

        ( Loading, TextureResponse (Ok texture) ) ->
            ( { model
                | screen =
                    Menu (initialWorld |> World.add (floor texture))
                        { texture = texture
                        , drivers = [ ( Keyboard defaultKeyboard, Blue ) ]
                        }
              }
            , Cmd.none
            )

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
                { g | score = game.score, timeLeft = game.timeLeft }

            else
                { game
                    | world =
                        game.world
                            |> World.update (updateBody False game)
                            |> World.simulate simulationStep
                    , lastTick = currentTick
                }

        Preparing hasCar countdown ->
            if Duration.inSeconds countdown <= 0 then
                { game | status = Live }

            else if not hasCar && currentTick > 500 then
                { game
                    | world = List.foldl (\p w -> World.add (base p) w) game.world game.players
                    , status = Preparing True countdown
                }

            else
                { game
                    | world =
                        game.world
                            |> World.update (updateBody True game)
                            |> World.simulate simulationStep
                    , status =
                        Preparing hasCar
                            (Quantity.minus (Duration.milliseconds tick) countdown)
                    , lastTick = currentTick
                }

        GameOver ->
            game

        Live ->
            let
                { score } =
                    game

                ballPoint =
                    game.world
                        |> World.bodies
                        |> List.filter (Body.data >> .id >> isBall)
                        |> List.head
                        |> Maybe.map Body.originPoint
                        |> Maybe.withDefault (Point3d.meters 0 0 0)

                playerCars : List ( Player, Point3d Meters WorldCoordinates )
                playerCars =
                    game.world
                        |> World.bodies
                        |> List.filterMap
                            (\car ->
                                case ( (Body.data car).id, List.head game.players ) of
                                    ( Car playerId _, Just somePlayer ) ->
                                        Just
                                            ( game.players
                                                |> List.filter (.id >> (==) playerId)
                                                |> List.head
                                                |> Maybe.withDefault somePlayer
                                            , Body.originPoint car
                                            )

                                    _ ->
                                        Nothing
                            )

                carHits : ( Player, Point3d Meters WorldCoordinates ) -> Refill -> Bool
                carHits ( player, carPoint ) { point, time } =
                    let
                        fullTank =
                            player.boostTank >= boostSettings.max
                    in
                    not fullTank
                        && ((currentTick - time) > boostSettings.reloadTime)
                        && (Point3d.distanceFrom carPoint point |> Length.inMeters)
                        < 2.5

                applyCarHit : ( Player, Point3d Meters WorldCoordinates ) -> Refill -> Refill
                applyCarHit playerCar refill =
                    if refillIsActive currentTick refill && carHits playerCar refill then
                        { refill | time = currentTick }

                    else
                        refill

                applyRefills : ( Player, Point3d Meters WorldCoordinates ) -> Float -> Float
                applyRefills playerCar boostTank =
                    let
                        adding =
                            game.refills
                                |> List.filter (carHits playerCar)
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

                currentTimeLeft =
                    Quantity.minus (Duration.milliseconds tick) game.timeLeft

                ballOnGround =
                    inMeters (Point3d.zCoordinate ballPoint) <= inMeters ballSettings.radius
            in
            { game
                | world =
                    game.world
                        |> World.update (updateBody False game)
                        |> World.simulate simulationStep
                , players =
                    List.map
                        (\( player, carPoint ) ->
                            { player
                                | boostTank =
                                    player.boostTank
                                        |> applyRefills ( player, carPoint )
                                        |> applyRockets player
                            }
                        )
                        playerCars
                , lastTick = currentTick
                , timeLeft = currentTimeLeft
                , refills =
                    game.refills
                        |> List.map
                            (\refill -> List.foldl applyCarHit refill playerCars)
                , score =
                    { blue = incrementIf blueGoal score.blue
                    , orange = incrementIf orangeGoal score.orange
                    }
                , status =
                    if ballOnGround && Duration.inSeconds currentTimeLeft <= 0 && score.blue /= score.orange then
                        GameOver

                    else if blueGoal || orangeGoal then
                        Replay currentTick

                    else
                        game.status
            }


updateBody : Bool -> Game -> Body Data -> Body Data
updateBody dry game body =
    case (Body.data body).id of
        Car playerId wheels ->
            let
                maybePlayer =
                    game.players
                        |> List.filter (.id >> samePlayerId playerId)
                        |> List.head

                boost =
                    case ( not dry, maybePlayer ) of
                        ( True, Just player ) ->
                            if player.controls.rockets && player.boostTank > 0 then
                                Body.applyForce (Force.newtons 30000)
                                    (Direction3d.placeIn (Body.frame body) carSettings.forwardDirection)
                                    (Body.originPoint body)

                            else
                                identity

                        _ ->
                            identity

                controls =
                    case ( not dry, maybePlayer ) of
                        ( True, Just player ) ->
                            player.controls

                        _ ->
                            initControls
            in
            simulateCar simulationStep game.world controls wheels body
                |> boost

        _ ->
            body


initGame : Config -> Game
initGame config =
    { world =
        initialWorld
            |> World.add (floor config.texture)
    , players =
        List.indexedMap
            (\index ( driver, team ) ->
                { id = PlayerId index
                , team = team
                , driver = driver
                , controls = initControls
                , boostTank = boostSettings.initial
                , focus = BallCam
                }
            )
            config.drivers
    , lastTick = 0
    , refills =
        Refill.init
            { startTime = -boostSettings.reloadTime
            , measure = roomSize.length / 10.8
            }
    , status = Preparing False (Duration.seconds 3)
    , score = { blue = 0, orange = 0 }
    , timeLeft = Quantity (Duration.minutes 5 |> Duration.inSeconds)
    }


initControls : Controls
initControls =
    { rockets = False
    , steering = 0
    , speeding = 0
    , braking = False
    }


applyJump : PlayerId -> Body Data -> Body Data
applyJump playerId body =
    -- TODO: restrict to double jump
    -- TODO: raycast to see if on ground
    -- TODO: handle arrow directions
    case (Body.data body).id of
        Car carPlayerId _ ->
            if playerId == carPlayerId then
                body
                    |> Body.applyForce (Force.newtons 400000)
                        -- TODO: add direction modifier keys
                        (body |> Body.frame >> Frame3d.zDirection)
                        (Body.originPoint body)

            else
                body

        _ ->
            body


applyRockets : Player -> Float -> Float
applyRockets player boostTank =
    if player.controls.rockets then
        max 0 (boostTank - 0.5)

    else
        boostTank


subscriptions : Model -> Sub Msg
subscriptions { screen } =
    let
        playerEvents player =
            [ Events.onKeyDown (keyDecoder player.driver (KeyDown player.id))
            , Events.onKeyUp (keyDecoder player.driver (KeyUp player.id))
            ]

        pauseKey =
            Events.onKeyDown
                (Json.Decode.field "key" Json.Decode.string
                    |> Json.Decode.andThen
                        (\string ->
                            if string == "p" then
                                Json.Decode.succeed (KeyDown (PlayerId -1) TogglePause)

                            else
                                Json.Decode.fail ("Unrecognized key: " ++ string)
                        )
                )

        resize =
            Events.onResize (\w h -> Resize (toFloat w) (toFloat h))
    in
    case screen of
        Menu _ _ ->
            Sub.batch
                [ Events.onAnimationFrameDelta Tick
                , resize
                ]

        Playing game _ ->
            case game.status of
                Paused _ ->
                    (Sub.batch << List.concat)
                        ([ resize, pauseKey ]
                            :: List.map playerEvents
                                game.players
                        )

                _ ->
                    (Sub.batch << List.concat)
                        ([ resize
                         , pauseKey
                         , Events.onAnimationFrameDelta Tick
                         ]
                            :: List.map playerEvents game.players
                        )

        _ ->
            resize


keyDecoder : Driver -> (Command -> Msg) -> Json.Decode.Decoder Msg
keyDecoder (Keyboard controlDict) toMsg =
    Json.Decode.field "key" Json.Decode.string
        |> Json.Decode.andThen
            (\string ->
                Dict.get string controlDict
                    |> Maybe.map (toMsg >> Json.Decode.succeed)
                    |> Maybe.withDefault (Json.Decode.fail ("Unrecognized key: " ++ string))
            )


view : Model -> Html Msg
view model =
    Html.div [ Html.Attributes.class "container" ]
        (case model.screen of
            Loading ->
                [ Html.p [ Html.Attributes.class "center-popup" ] [ Html.text "Loading..." ] ]

            Menu world config ->
                let
                    { width, height } =
                        model.screenSize
                in
                [ Scene3d.custom
                    { dimensions = ( pixels (Basics.floor width), pixels (Basics.floor height) )
                    , antialiasing = Scene3d.multisampling
                    , camera =
                        Camera3d.perspective
                            { viewpoint =
                                Viewpoint3d.lookAt
                                    { eyePoint = Point3d.meters 30 -5 5
                                    , focalPoint = Point3d.meters 0 5 5
                                    , upDirection = Direction3d.positiveZ
                                    }
                            , verticalFieldOfView = Angle.degrees 24
                            }
                    , lights = Scene3d.twoLights sunlight daylight
                    , exposure = Scene3d.maxLuminance (Luminance.nits 10000)
                    , toneMapping = Scene3d.noToneMapping
                    , whiteBalance = Light.daylight
                    , background = Scene3d.transparentBackground
                    , clipDepth = meters 0.1
                    , entities = world |> World.bodies |> List.map getTransformedDrawable
                    }
                , Html.div [ Html.Attributes.class "menu" ]
                    [ Html.div []
                        [ Html.h1 [] [ Html.text "Rocket League" ]
                        , Html.div []
                            [ radio
                                { group = "mode"
                                , value = "1p"
                                , label = "One Player"
                                , checked = List.length config.drivers == 1
                                , onCheck = \_ -> SetDrivers [ ( Keyboard defaultKeyboard, Blue ) ]
                                }
                            , radio
                                { group = "mode"
                                , value = "2p"
                                , label = "Two Player"
                                , checked = List.length config.drivers == 2
                                , onCheck = \_ -> SetDrivers twoPlayerDrivers
                                }
                            ]
                        , Html.div [ Html.Attributes.class "btn-row" ]
                            [ Html.button [ Html.Events.onClick StartGame, Html.Attributes.class "btn-primary" ]
                                [ Html.text "Let's go!" ]
                            ]
                        ]
                    , Html.div [ Html.Attributes.style "display" "flex" ]
                        (config.drivers
                            |> List.indexedMap
                                (\index ( driver, _ ) ->
                                    case driver of
                                        Keyboard controlDict ->
                                            Html.div
                                                [ Html.Attributes.style "flex-grow" "1"
                                                , Html.Attributes.style "padding" "0 12px"
                                                , Html.Attributes.style "min-width" "180px"
                                                ]
                                                (Html.h3 [ Html.Attributes.style "margin" "12px 0 8px" ] [ Html.text (String.fromInt (index + 1) ++ "p controls") ]
                                                    :: (controlDict
                                                            |> Dict.toList
                                                            |> List.map
                                                                (\( key, command ) ->
                                                                    Html.div [ Html.Attributes.class "controls-row" ]
                                                                        [ Html.span []
                                                                            [ Html.text
                                                                                (if key == " " then
                                                                                    "Spacebar"

                                                                                 else
                                                                                    key
                                                                                )
                                                                            ]
                                                                        , Html.span [] [ Html.text (commandToString command) ]
                                                                        ]
                                                                )
                                                       )
                                                )
                                )
                        )
                    ]
                ]

            Playing game _ ->
                -- TODO: loop over (human) players, split screen as needed
                let
                    humans =
                        game.players

                    scoreboard =
                        Html.div [ Html.Attributes.class "hud-pane hud-top-center hud-score-clock" ]
                            [ Html.span [ Html.Attributes.class "hud-score hud-score-orange" ] [ Html.text <| String.fromInt game.score.orange ]
                            , viewClock game.timeLeft
                            , Html.span [ Html.Attributes.class "hud-score hud-score-blue" ] [ Html.text <| String.fromInt game.score.blue ]
                            ]

                    message =
                        case game.status of
                            Preparing _ countdown ->
                                Html.h1 [ Html.Attributes.class "hud-pane hud-pane-msg center-popup" ]
                                    [ Html.text (String.fromInt (Basics.ceiling (Duration.inSeconds countdown))) ]

                            Replay _ ->
                                Html.h1 [ Html.Attributes.class "hud-pane hud-pane-msg center-popup" ] [ Html.text "GOOAAALLLL!!!!!" ]

                            GameOver ->
                                Html.div [ Html.Attributes.class "hud-pane hud-pane-msg center-popup" ]
                                    [ Html.h1 []
                                        [ Html.text
                                            (if game.score.blue > game.score.orange then
                                                "Blue wins!!!"

                                             else
                                                "Orange wins!!!"
                                            )
                                        ]
                                    , Html.div [ Html.Attributes.class "btn-row" ]
                                        [ Html.button [ Html.Attributes.class "btn-primary", Html.Events.onClick LeaveGame ]
                                            [ Html.text "Menu" ]
                                        ]
                                    ]

                            _ ->
                                Html.text ""

                    commonEntities =
                        List.concat
                            [ game.world
                                |> World.bodies
                                |> List.filter (Body.data >> .id >> isCar)
                                |> List.map
                                    (\car_ ->
                                        case (Body.data >> .id) car_ of
                                            Car _ wheels ->
                                                renderWheels car_ wheels

                                            _ ->
                                                []
                                    )
                                |> List.concat
                            , List.map
                                (\refill ->
                                    Refill.view (refillIsActive game.lastTick refill) refill
                                )
                                game.refills
                            ]
                in
                case humans of
                    [ p1 ] ->
                        viewPlayer model.screenSize p1 game commonEntities
                            ++ [ scoreboard
                               , message
                               ]

                    [ _, _ ] ->
                        let
                            screenSize =
                                { width = model.screenSize.width / 2
                                , height = model.screenSize.height
                                }

                            playerViews =
                                Html.div [ Html.Attributes.style "display" "flex" ]
                                    (humans
                                        |> List.sortBy
                                            (\p ->
                                                case p.id of
                                                    PlayerId int ->
                                                        int
                                            )
                                        |> List.map
                                            (\p ->
                                                Html.div [ Html.Attributes.style "position" "relative" ]
                                                    (viewPlayer screenSize p game commonEntities)
                                            )
                                    )
                        in
                        [ playerViews, scoreboard, message ]

                    _ ->
                        [ Html.text ("Too many players: " ++ String.fromInt (List.length humans)) ]

            LoadingError error ->
                [ Html.p [] [ Html.text ("Error: " ++ error) ] ]
        )


type alias RadioProps msg =
    { group : String
    , value : String
    , label : String
    , checked : Bool
    , onCheck : Bool -> msg
    }


radio : RadioProps msg -> Html msg
radio { group, value, label, checked, onCheck } =
    let
        id =
            String.join "-" [ group, value ]
    in
    Html.div [ Html.Attributes.style "padding" "4px 0" ]
        [ Html.input
            [ Html.Attributes.type_ "radio"
            , Html.Attributes.id id
            , Html.Attributes.name group
            , Html.Attributes.value value
            , Html.Attributes.checked checked
            , Html.Events.onCheck onCheck
            ]
            []
        , Html.label [ Html.Attributes.for id ] [ Html.text label ]
        ]


viewPlayer : ScreenSize -> Player -> Game -> List (Scene3d.Entity WorldCoordinates) -> List (Html Msg)
viewPlayer { width, height } player { world } commonEntities =
    let
        car =
            world
                |> World.bodies
                |> List.filter (Body.data >> .id >> isPlayersCar player)
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
            commonEntities
                ++ (world
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
                   )
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
    , Html.div [ Html.Attributes.class "hud-pane hud-pane-msg hud-bottom-right" ]
        [ Html.p
            [ Html.Attributes.style "font-size" "48px" ]
            [ Html.text (String.fromInt (round player.boostTank)) ]
        , Html.p []
            [ Html.text "BOOST" ]
        ]
    , case player.focus of
        BallCam ->
            let
                toggleCamKey =
                    case player.driver of
                        Keyboard controlDict ->
                            controlDict
                                |> Dict.toList
                                |> List.filter (Tuple.second >> (==) ToggleCam)
                                |> List.head
                                |> Maybe.map Tuple.first
                                |> Maybe.withDefault ""
            in
            Html.div [ Html.Attributes.class "hud-pane-msg hud-bottom-left hud-cam" ]
                [ Html.p
                    [ Html.Attributes.style "font-size" "24px"
                    , Html.Attributes.class "hud-cam-indicator"
                    ]
                    [ Html.text "• Ball cam" ]
                , Html.p []
                    [ Html.text <| "Press [" ++ toggleCamKey ++ "] to toggle" ]
                ]

        _ ->
            Html.text ""
    ]


sunlight : Light.Light coordinates Bool
sunlight =
    Light.directional (Light.castsShadows True)
        { chromaticity = Light.sunlight
        , intensity = Illuminance.lux 10000
        , direction = Direction3d.xyZ (Angle.degrees 45) (Angle.degrees -60)
        }


daylight : Light.Light coordinates Never
daylight =
    Light.overhead
        { upDirection = Direction3d.z
        , chromaticity = Light.daylight
        , intensity = Illuminance.lux 15000
        }


viewClock : Duration -> Html Msg
viewClock timeLeft =
    let
        ceilSeconds =
            Basics.ceiling <| Duration.inSeconds timeLeft

        seconds =
            max 0 (remainderBy 60 <| ceilSeconds)

        minutes =
            max 0 (Basics.floor (toFloat ceilSeconds / 60))

        twoChar str =
            if String.length str < 2 then
                twoChar ("0" ++ str)

            else
                str
    in
    Html.span [ Html.Attributes.class "hud-clock" ]
        [ Html.text (String.fromInt minutes)
        , Html.text ":"
        , Html.text (twoChar (String.fromInt seconds))
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
                            Quantity.greaterThan (Quantity.times maximpSide maximpSide) impulseSquared

                        skidInfo =
                            if isSliding then
                                Quantity.ratio maximpSide (Quantity.sqrt impulseSquared)

                            else
                                1
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
    case ( wheels, (Body.data car).id ) of
        ( [], Car playerId _ ) ->
            Body.withData
                { id = Car playerId (List.reverse updatedWheels)
                , entity = (Body.data car).entity
                }
                car

        ( [], _ ) ->
            -- Never should get hit cause `car` should always be a `Car`...
            car

        ( wheel :: remainingWheels, _ ) ->
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


base : Player -> Body Data
base player =
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
                { baseColor = teamColor player.team
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
    { id = Car player.id wheels
    , entity = entity
    }
        |> Body.block shape
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 1190))
        |> Body.moveTo offset
        |> (\body ->
                case player.team of
                    Orange ->
                        Body.rotateAround Axis3d.z (Angle.degrees 180) body

                    _ ->
                        body
           )


ballSettings : { radius : Length }
ballSettings =
    { radius = Length.meters 2
    }


ball : Body Data
ball =
    let
        shape =
            Sphere3d.atOrigin ballSettings.radius

        entity =
            Scene3d.sphereWithShadow (Material.uniform Materials.chromium) shape
    in
    { id = Ball
    , entity = entity
    }
        |> Body.sphere shape
        |> Body.withMaterial (Physics.Material.custom { friction = 0.3, bounciness = 0.8 })
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 1))
        |> Body.moveTo (Point3d.meters 0 0 (inMeters ballSettings.radius))


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
            ( 5, 10 )

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
