module Main exposing (main)

import Acceleration
import Angle exposing (Angle)
import Axis3d
import Block3d
import Browser
import Browser.Dom as Dom
import Browser.Events as Events
import Camera3d
import Color
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


type alias Data =
    { entity : Scene3d.Entity BodyCoordinates
    , id : EntityId
    }


type EntityId
    = Arena
    | Car (List Wheel)
    | Ball
    | Obstacle


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
    , braking : Bool
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
      , braking = False
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
                                        let
                                            boost =
                                                if model.rockets then
                                                    Body.applyForce (Force.newtons 30000)
                                                        (Direction3d.placeIn (Body.frame body) carSettings.forwardDirection)
                                                        (Body.originPoint body)

                                                else
                                                    identity
                                        in
                                        simulateCar (Duration.seconds (1 / 60)) model wheels body
                                            |> boost

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
            List.map getTransformedDrawable (World.bodies (addWheelsToWorld world))

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


addWheelsToWorld : World Data -> World Data
addWheelsToWorld world =
    let
        maybeCar =
            world
                |> World.bodies
                |> List.filterMap
                    (\b ->
                        case (Body.data b).id of
                            Car wheels ->
                                Just ( wheels, b )

                            _ ->
                                Nothing
                    )
                |> List.head
    in
    case maybeCar of
        Just ( wheels, car ) ->
            List.foldl
                (\wheel ->
                    let
                        frame =
                            Body.frame car

                        position =
                            wheel.chassisConnectionPoint
                                |> Point3d.placeIn (Body.frame car)

                        downDirection =
                            carSettings.downDirection
                                |> Direction3d.placeIn (Body.frame car)

                        rightDirection =
                            carSettings.rightDirection
                                |> Direction3d.placeIn (Body.frame car)

                        newPosition =
                            position |> Point3d.translateBy (Vector3d.withLength wheel.suspensionLength downDirection)

                        newFrame =
                            frame
                                |> Frame3d.moveTo newPosition
                                |> Frame3d.rotateAround (Axis3d.through newPosition downDirection) wheel.steering
                                |> Frame3d.rotateAround (Axis3d.through newPosition rightDirection) wheel.rotation
                    in
                    World.add
                        (Body.sphere wheelShape
                            { id = Obstacle
                            , entity = wheelBody
                            }
                            |> Body.placeIn newFrame
                        )
                )
                world
                wheels

        Nothing ->
            world


wheelBody =
    Scene3d.sphere (Material.uniform Materials.chromium) (Sphere3d.atOrigin carSettings.radius)


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


simulateCar : Duration -> Model -> List Wheel -> Body Data -> Body Data
simulateCar dt { world, steering, braking, speeding } wheels car =
    case wheels of
        [ w1, w2, w3, w4 ] ->
            let
                engineForce =
                    Force.newtons (1000 * speeding)

                brake =
                    if braking then
                        Force.newtons 1000

                    else
                        Quantity.zero

                wheel1 =
                    { w1 | steering = Angle.degrees (30 * steering), engineForce = engineForce, brake = brake }

                wheel2 =
                    { w2 | steering = Angle.degrees (30 * steering), engineForce = engineForce, brake = brake }

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


wheelShape : Sphere3d Meters BodyCoordinates
wheelShape =
    Sphere3d.atOrigin carSettings.radius


base : Body Data
base =
    let
        offset =
            Point3d.meters 0 0 3

        shape =
            Block3d.centeredOn Frame3d.atOrigin
                ( Length.meters 4, Length.meters 2, Length.meters 1 )

        entity =
            Scene3d.block
                (Material.nonmetal
                    { baseColor = Color.rgb255 0 0 0
                    , roughness = 0.5
                    }
                )
                shape

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
            Sphere3d.atOrigin (Length.meters 2)

        entity =
            Scene3d.sphere (Material.uniform Materials.chromium) shape
    in
    { id = Ball
    , entity = entity
    }
        |> Body.sphere shape
        |> Body.withBehavior (Body.dynamic (Mass.kilograms 50))
        |> Body.moveTo (Point3d.meters 10 0 5)
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
    { id = Obstacle, entity = entity }
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
