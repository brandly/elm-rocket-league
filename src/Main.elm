module Main exposing (main)

import Acceleration
import Angle
import Array exposing (Array)
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Camera3d
import Direction3d
import Duration
import Force
import Frame3d
import Html exposing (Html)
import Html.Attributes
import Illuminance
import Json.Decode
import Length exposing (Length, inMeters, meters)
import Luminance
import Mass
import Materials
import Palette.Tango as Tango
import Physics.Body as Body exposing (Body)
import Physics.Coordinates exposing (BodyCoordinates, WorldCoordinates)
import Physics.World as World exposing (World)
import Pixels exposing (pixels)
import Point3d
import Random
import Scene3d
import Scene3d.Material as Material
import Sphere3d
import Task
import Viewpoint3d


type alias Model =
    { world : World (Scene3d.Entity BodyCoordinates)
    , rockets : Bool
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
                                if model.rockets then
                                    body
                                        |> Body.applyForce (Force.newtons 50000)
                                            Direction3d.negativeY
                                            (Body.originPoint body)

                                else
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
                            body
                                |> Body.applyForce (Force.newtons 100000)
                                    Direction3d.positiveZ
                                    (Body.originPoint body)
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
                    Viewpoint3d.lookAt
                        { eyePoint = Point3d.meters 0 100 100
                        , focalPoint =
                            world
                                |> World.bodies
                                -- TODO: identify these bodies, find the car
                                |> List.drop 1
                                |> List.head
                                |> Maybe.map (Body.frame >> Frame3d.originPoint)
                                |> Maybe.withDefault (Point3d.meters 0 0 0)
                        , upDirection = Direction3d.positiveZ
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


initialWorld : World (Scene3d.Entity BodyCoordinates)
initialWorld =
    let
        earthGravity =
            Acceleration.metersPerSecondSquared 9.807
    in
    World.empty
        |> World.withGravity earthGravity Direction3d.negativeZ
        |> World.add floor
        --|> addBoxes
        |> addCar


materials : Array (Material.Uniform coordinates)
materials =
    Array.fromList
        [ Materials.aluminum
        , Materials.whitePlastic
        , Materials.copper
        , Material.nonmetal
            { baseColor = Tango.skyBlue1
            , roughness = 0.25
            }
        , Materials.gold
        , Materials.whitePlastic
        , Material.nonmetal
            { baseColor = Tango.skyBlue2
            , roughness = 0.25
            }
        ]


type alias Offsets =
    { x : Float
    , y : Float
    , z : Float
    }


offsetGenerator : Random.Generator Offsets
offsetGenerator =
    let
        magnitude =
            0.01
    in
    Random.map3 Offsets
        (Random.float -magnitude magnitude)
        (Random.float -magnitude magnitude)
        (Random.float -magnitude magnitude)


randomOffsets : Int -> Offsets
randomOffsets index =
    Random.step offsetGenerator (Random.initialSeed index)
        |> Tuple.first


addCar : World (Scene3d.Entity BodyCoordinates) -> World (Scene3d.Entity BodyCoordinates)
addCar =
    let
        shape =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 3, Length.meters 5.26, Length.meters 2 )

        body =
            Scene3d.block Scene3d.castsShadows Materials.gold shape
                |> Body.block shape
                |> Body.withBehavior (Body.dynamic (Mass.kilograms 1190))
    in
    body
        |> Body.moveTo (Point3d.meters 0 0 3)
        |> World.add


addBoxes : World (Scene3d.Entity BodyCoordinates) -> World (Scene3d.Entity BodyCoordinates)
addBoxes world =
    let
        xySize =
            4

        zSize =
            5

        xyDimensions =
            List.map toFloat (List.range 0 (xySize - 1))

        zDimensions =
            List.map toFloat (List.range 0 (zSize - 1))

        distance =
            1

        coords : List ( Float, Float, Float )
        coords =
            List.map (\x -> List.map (\y -> List.map (\z -> ( x, y, z )) zDimensions) xyDimensions) xyDimensions
                |> List.concat
                |> List.concat
    in
    List.foldl
        (\( x, y, z ) ->
            let
                index =
                    round (z * xySize * xySize + y * xySize + x)

                material =
                    Array.get (index |> modBy (Array.length materials)) materials
                        |> Maybe.withDefault Materials.aluminum

                body =
                    if (index |> modBy 3) == 0 then
                        box material

                    else
                        sphere (Material.uniform material)

                offsets =
                    randomOffsets index
            in
            body
                |> Body.moveTo
                    (Point3d.meters
                        ((x - (xySize - 1) / 2) * distance + offsets.x)
                        ((y - (xySize - 1) / 2) * distance + offsets.y)
                        ((z + (2 * zSize + 1) / 2) * distance + offsets.z)
                    )
                |> World.add
        )
        world
        coords


floorSize : Length
floorSize =
    Length.meters 150


floor : Body (Scene3d.Entity BodyCoordinates)
floor =
    let
        point x y =
            Point3d.meters x y 0

        size =
            Length.inMeters floorSize
    in
    Scene3d.quad Scene3d.doesNotCastShadows
        (Material.uniform Materials.blackPlastic)
        (point -size -size)
        (point -size size)
        (point size size)
        (point size -size)
        |> Body.plane
        |> Body.moveTo
            (Point3d.meters 0 0 0)


boxSize : Length
boxSize =
    Length.meters 0.9


boxWithSize : Length -> Material.Uniform BodyCoordinates -> Body (Scene3d.Entity BodyCoordinates)
boxWithSize size_ material =
    let
        shape =
            Block3d.centeredOn Frame3d.atOrigin
                ( size_, size_, size_ )
    in
    Scene3d.block Scene3d.castsShadows material shape
        |> Body.block shape
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 5))


box : Material.Uniform BodyCoordinates -> Body (Scene3d.Entity BodyCoordinates)
box =
    boxWithSize boxSize


sphereRadius : Length
sphereRadius =
    Length.meters 0.45


sphere : Material.Textured BodyCoordinates -> Body (Scene3d.Entity BodyCoordinates)
sphere material =
    let
        shape =
            Sphere3d.atOrigin sphereRadius
    in
    Scene3d.sphere Scene3d.castsShadows material shape
        |> Body.sphere shape
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 2.5))


getTransformedDrawable : Body (Scene3d.Entity BodyCoordinates) -> Scene3d.Entity WorldCoordinates
getTransformedDrawable body =
    Scene3d.placeIn (Body.frame body) (Body.data body)
