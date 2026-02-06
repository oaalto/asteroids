port module Main exposing (main)

import Browser
import Browser.Events
import Html exposing (Html)
import Html.Attributes
import Json.Decode as Decode
import Json.Encode as Encode
import Random
import Time


-- PORTS

port render : Encode.Value -> Cmd msg


-- CONSTANTS

canvasWidth : Float
canvasWidth = 800

canvasHeight : Float
canvasHeight = 600

shipRadius : Float
shipRadius = 15

bulletSpeed : Float
bulletSpeed = 7

bulletLifetime : Float
bulletLifetime = 60

maxBullets : Int
maxBullets = 5

shipThrust : Float
shipThrust = 0.12

shipFriction : Float
shipFriction = 0.99

shipRotationSpeed : Float
shipRotationSpeed = 5

invincibilityDuration : Float
invincibilityDuration = 120

respawnDelay : Float
respawnDelay = 60


-- MODEL

type GameState
    = Start
    | Playing
    | GameOver


type AsteroidSize
    = Large
    | Medium
    | Small


type alias Vec2 =
    { x : Float, y : Float }


type alias Ship =
    { pos : Vec2
    , vel : Vec2
    , angle : Float
    , thrusting : Bool
    , invincible : Float
    }


type alias Bullet =
    { pos : Vec2
    , vel : Vec2
    , life : Float
    }


type alias Asteroid =
    { pos : Vec2
    , vel : Vec2
    , size : AsteroidSize
    , angle : Float
    , rotSpeed : Float
    , vertices : List Vec2
    }


type alias Particle =
    { pos : Vec2
    , vel : Vec2
    , life : Float
    , maxLife : Float
    , color : String
    }


type alias Keys =
    { left : Bool
    , right : Bool
    , up : Bool
    , space : Bool
    }


type alias Model =
    { state : GameState
    , ship : Ship
    , bullets : List Bullet
    , asteroids : List Asteroid
    , particles : List Particle
    , score : Int
    , lives : Int
    , level : Int
    , keys : Keys
    , shootCooldown : Float
    , respawnTimer : Float
    , seed : Random.Seed
    , frameCount : Int
    }


initShip : Ship
initShip =
    { pos = { x = canvasWidth / 2, y = canvasHeight / 2 }
    , vel = { x = 0, y = 0 }
    , angle = -90
    , thrusting = False
    , invincible = invincibilityDuration
    }


initKeys : Keys
initKeys =
    { left = False
    , right = False
    , up = False
    , space = False
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( asteroids, seed ) =
            generateAsteroids 4 (Random.initialSeed 42)
    in
    ( { state = Start
      , ship = initShip
      , bullets = []
      , asteroids = asteroids
      , particles = []
      , score = 0
      , lives = 3
      , level = 1
      , keys = initKeys
      , shootCooldown = 0
      , respawnTimer = 0
      , seed = seed
      , frameCount = 0
      }
    , Cmd.none
    )


asteroidRadius : AsteroidSize -> Float
asteroidRadius size =
    case size of
        Large -> 40
        Medium -> 20
        Small -> 10


asteroidScore : AsteroidSize -> Int
asteroidScore size =
    case size of
        Large -> 20
        Medium -> 50
        Small -> 100


generateAsteroidVertices : Random.Seed -> ( List Vec2, Random.Seed )
generateAsteroidVertices seed0 =
    let
        numVerts = 10

        step i ( accVerts, currentSeed ) =
            let
                angleFrac = toFloat i / toFloat numVerts * 2 * pi

                ( jitter, nextSeed ) =
                    Random.step (Random.float 0.7 1.3) currentSeed
            in
            ( accVerts ++ [ { x = cos angleFrac * jitter, y = sin angleFrac * jitter } ]
            , nextSeed
            )
    in
    List.range 0 (numVerts - 1)
        |> List.foldl step ( [], seed0 )


generateAsteroid : Random.Seed -> ( Asteroid, Random.Seed )
generateAsteroid seed0 =
    let
        ( edgeSide, seed1 ) = Random.step (Random.int 0 3) seed0
        ( posAlongEdge, seed2 ) = Random.step (Random.float 0 1) seed1
        pos =
            case edgeSide of
                0 -> { x = posAlongEdge * canvasWidth, y = -40 }
                1 -> { x = canvasWidth + 40, y = posAlongEdge * canvasHeight }
                2 -> { x = posAlongEdge * canvasWidth, y = canvasHeight + 40 }
                _ -> { x = -40, y = posAlongEdge * canvasHeight }

        ( speedVal, seed3 ) = Random.step (Random.float 0.5 2.0) seed2
        ( dirAngle, seed4 ) = Random.step (Random.float 0 (2 * pi)) seed3
        vel = { x = cos dirAngle * speedVal, y = sin dirAngle * speedVal }
        ( rotSpeed, seed5 ) = Random.step (Random.float -2 2) seed4
        ( startAngle, seed6 ) = Random.step (Random.float 0 360) seed5
        ( verts, seed7 ) = generateAsteroidVertices seed6
    in
    ( { pos = pos
      , vel = vel
      , size = Large
      , angle = startAngle
      , rotSpeed = rotSpeed
      , vertices = verts
      }
    , seed7
    )


generateAsteroids : Int -> Random.Seed -> ( List Asteroid, Random.Seed )
generateAsteroids count seed0 =
    List.range 1 count
        |> List.foldl
            (\_ ( acc, s ) ->
                let
                    ( ast, s2 ) = generateAsteroid s
                in
                ( acc ++ [ ast ], s2 )
            )
            ( [], seed0 )


splitAsteroid : Asteroid -> Random.Seed -> ( List Asteroid, Random.Seed )
splitAsteroid ast seed0 =
    case ast.size of
        Small ->
            ( [], seed0 )

        _ ->
            let
                newSize =
                    case ast.size of
                        Large -> Medium
                        _ -> Small

                makeChild s0 =
                    let
                        ( angle, s1 ) = Random.step (Random.float 0 (2 * pi)) s0
                        ( speed, s2 ) = Random.step (Random.float 1.5 3.0) s1
                        ( rotSpd, s3 ) = Random.step (Random.float -3 3) s2
                        ( verts, s4 ) = generateAsteroidVertices s3
                    in
                    ( { pos = ast.pos
                      , vel = { x = cos angle * speed, y = sin angle * speed }
                      , size = newSize
                      , angle = ast.angle
                      , rotSpeed = rotSpd
                      , vertices = verts
                      }
                    , s4
                    )

                ( child1, seed1 ) = makeChild seed0
                ( child2, seed2 ) = makeChild seed1
            in
            ( [ child1, child2 ], seed2 )



-- UPDATE

type Msg
    = Tick Time.Posix
    | KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            let
                newKeys = updateKeys True key model.keys
            in
            case model.state of
                Start ->
                    if key == "Enter" then
                        let
                            ( asteroids, seed ) = generateAsteroids 4 model.seed
                        in
                        ( { model
                            | state = Playing
                            , ship = initShip
                            , bullets = []
                            , asteroids = asteroids
                            , particles = []
                            , score = 0
                            , lives = 3
                            , level = 1
                            , keys = newKeys
                            , seed = seed
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | keys = newKeys }, Cmd.none )

                GameOver ->
                    if key == "Enter" then
                        let
                            ( asteroids, seed ) = generateAsteroids 4 model.seed
                        in
                        ( { model
                            | state = Playing
                            , ship = initShip
                            , bullets = []
                            , asteroids = asteroids
                            , particles = []
                            , score = 0
                            , lives = 3
                            , level = 1
                            , keys = newKeys
                            , seed = seed
                          }
                        , Cmd.none
                        )
                    else
                        ( { model | keys = newKeys }, Cmd.none )

                Playing ->
                    ( { model | keys = newKeys }, Cmd.none )

        KeyUp key ->
            ( { model | keys = updateKeys False key model.keys }, Cmd.none )

        Tick _ ->
            case model.state of
                Start ->
                    let
                        updatedModel = { model | frameCount = model.frameCount + 1 }
                        movedAsteroids = List.map moveAsteroid updatedModel.asteroids
                    in
                    ( { updatedModel | asteroids = movedAsteroids }
                    , render (encodeScene updatedModel)
                    )

                GameOver ->
                    ( model, render (encodeScene model) )

                Playing ->
                    let
                        m1 = tickPlaying model
                    in
                    ( m1, render (encodeScene m1) )


updateKeys : Bool -> String -> Keys -> Keys
updateKeys pressed key keys =
    case key of
        "ArrowLeft" -> { keys | left = pressed }
        "ArrowRight" -> { keys | right = pressed }
        "ArrowUp" -> { keys | up = pressed }
        " " -> { keys | space = pressed }
        _ -> keys


tickPlaying : Model -> Model
tickPlaying model =
    let
        -- Rotate ship
        newAngle =
            if model.keys.left then
                model.ship.angle - shipRotationSpeed
            else if model.keys.right then
                model.ship.angle + shipRotationSpeed
            else
                model.ship.angle

        -- Thrust
        angleRad = degrees newAngle
        thrustX = if model.keys.up then cos angleRad * shipThrust else 0
        thrustY = if model.keys.up then sin angleRad * shipThrust else 0

        oldShip = model.ship
        newVel =
            { x = (oldShip.vel.x + thrustX) * shipFriction
            , y = (oldShip.vel.y + thrustY) * shipFriction
            }

        newPos = wrapPosition { x = oldShip.pos.x + newVel.x, y = oldShip.pos.y + newVel.y }

        newInvincible = max 0 (oldShip.invincible - 1)

        newShip =
            { oldShip
                | pos = newPos
                , vel = newVel
                , angle = newAngle
                , thrusting = model.keys.up
                , invincible = newInvincible
            }

        -- Thrust particles
        ( thrustParticles, seedAfterThrust ) =
            if model.keys.up then
                generateThrustParticles newShip model.seed
            else
                ( [], model.seed )

        -- Shooting
        ( newBullets, newCooldown ) =
            if model.keys.space && model.shootCooldown <= 0 && List.length model.bullets < maxBullets then
                let
                    bVel =
                        { x = cos angleRad * bulletSpeed + newVel.x * 0.5
                        , y = sin angleRad * bulletSpeed + newVel.y * 0.5
                        }
                    tipOffset = shipRadius + 5
                    bPos =
                        { x = newPos.x + cos angleRad * tipOffset
                        , y = newPos.y + sin angleRad * tipOffset
                        }
                    bullet = { pos = bPos, vel = bVel, life = bulletLifetime }
                in
                ( bullet :: model.bullets, 8 )
            else
                ( model.bullets, max 0 (model.shootCooldown - 1) )

        -- Move bullets
        movedBullets =
            newBullets
                |> List.map moveBullet
                |> List.filter (\b -> b.life > 0)

        -- Move asteroids
        movedAsteroids = List.map moveAsteroid model.asteroids

        -- Bullet-asteroid collisions
        collisionResult =
            checkBulletAsteroidCollisions movedBullets movedAsteroids seedAfterThrust

        allAsteroids = collisionResult.asteroids ++ collisionResult.spawned

        -- Ship-asteroid collision
        ( hitShip, shipCollisionParticles, seedAfterShipCol ) =
            if newShip.invincible > 0 then
                ( False, [], collisionResult.seed )
            else
                checkShipAsteroidCollision newShip allAsteroids collisionResult.seed

        -- Update lives / respawn
        newLives = if hitShip then model.lives - 1 else model.lives

        ( finalShip, respawnTimer ) =
            if hitShip then
                if newLives > 0 then
                    ( { initShip | invincible = invincibilityDuration }, 0 )
                else
                    ( newShip, 0 )
            else
                ( newShip, 0 )

        -- Game over check
        newState =
            if hitShip && newLives <= 0 then
                GameOver
            else
                Playing

        -- Level up: spawn new wave when all asteroids destroyed
        ( finalAsteroids, newLevel, seedAfterLevel ) =
            if List.isEmpty allAsteroids then
                let
                    nextLevel = model.level + 1
                    count = 3 + nextLevel
                    ( asts, s ) = generateAsteroids count seedAfterShipCol
                in
                ( asts, nextLevel, s )
            else
                ( allAsteroids, model.level, seedAfterShipCol )

        -- Update particles
        updatedParticles =
            (model.particles ++ thrustParticles ++ collisionResult.particles ++ shipCollisionParticles)
                |> List.map (\p -> { p | pos = { x = p.pos.x + p.vel.x, y = p.pos.y + p.vel.y }, life = p.life - 1 })
                |> List.filter (\p -> p.life > 0)
    in
    { model
        | state = newState
        , ship = finalShip
        , bullets = collisionResult.bullets
        , asteroids = finalAsteroids
        , particles = updatedParticles
        , score = model.score + collisionResult.score
        , lives = newLives
        , level = newLevel
        , shootCooldown = newCooldown
        , respawnTimer = respawnTimer
        , seed = seedAfterLevel
        , frameCount = model.frameCount + 1
    }


wrapPosition : Vec2 -> Vec2
wrapPosition pos =
    let
        margin = 50
    in
    { x =
        if pos.x < -margin then pos.x + canvasWidth + margin * 2
        else if pos.x > canvasWidth + margin then pos.x - canvasWidth - margin * 2
        else pos.x
    , y =
        if pos.y < -margin then pos.y + canvasHeight + margin * 2
        else if pos.y > canvasHeight + margin then pos.y - canvasHeight - margin * 2
        else pos.y
    }


moveBullet : Bullet -> Bullet
moveBullet b =
    let
        newPos = wrapPosition { x = b.pos.x + b.vel.x, y = b.pos.y + b.vel.y }
    in
    { b | pos = newPos, life = b.life - 1 }


moveAsteroid : Asteroid -> Asteroid
moveAsteroid a =
    let
        newPos = wrapPosition { x = a.pos.x + a.vel.x, y = a.pos.y + a.vel.y }
    in
    { a | pos = newPos, angle = a.angle + a.rotSpeed }


distance : Vec2 -> Vec2 -> Float
distance a b =
    sqrt ((a.x - b.x)^2 + (a.y - b.y)^2)


type alias CollisionResult =
    { bullets : List Bullet
    , asteroids : List Asteroid
    , spawned : List Asteroid
    , score : Int
    , particles : List Particle
    , seed : Random.Seed
    }


checkBulletAsteroidCollisions :
    List Bullet -> List Asteroid -> Random.Seed
    -> CollisionResult
checkBulletAsteroidCollisions bullets asteroids seed0 =
    List.foldl
        (\bullet acc ->
            let
                ( hit, hitAsteroid, restAsteroids ) =
                    findHitAsteroid bullet acc.asteroids
            in
            if hit then
                case hitAsteroid of
                    Just ast ->
                        let
                            ( children, s2 ) = splitAsteroid ast acc.seed
                            ( explosionParticles, s3 ) = generateExplosionParticles ast.pos (asteroidColor ast.size) 8 s2
                        in
                        { acc
                            | asteroids = restAsteroids
                            , spawned = acc.spawned ++ children
                            , score = acc.score + asteroidScore ast.size
                            , particles = acc.particles ++ explosionParticles
                            , seed = s3
                        }

                    Nothing ->
                        { acc | bullets = bullet :: acc.bullets }
            else
                { acc | bullets = bullet :: acc.bullets }
        )
        { bullets = [], asteroids = asteroids, spawned = [], score = 0, particles = [], seed = seed0 }
        bullets


findHitAsteroid : Bullet -> List Asteroid -> ( Bool, Maybe Asteroid, List Asteroid )
findHitAsteroid bullet asteroids =
    case asteroids of
        [] ->
            ( False, Nothing, [] )

        ast :: rest ->
            if distance bullet.pos ast.pos < asteroidRadius ast.size + 3 then
                ( True, Just ast, rest )
            else
                let
                    ( found, hitAst, remaining ) = findHitAsteroid bullet rest
                in
                ( found, hitAst, ast :: remaining )


checkShipAsteroidCollision : Ship -> List Asteroid -> Random.Seed -> ( Bool, List Particle, Random.Seed )
checkShipAsteroidCollision ship asteroids seed0 =
    case asteroids of
        [] ->
            ( False, [], seed0 )

        ast :: rest ->
            if distance ship.pos ast.pos < asteroidRadius ast.size + shipRadius - 5 then
                let
                    ( particles, s ) = generateExplosionParticles ship.pos "#00ffff" 15 seed0
                in
                ( True, particles, s )
            else
                checkShipAsteroidCollision ship rest seed0


generateExplosionParticles : Vec2 -> String -> Int -> Random.Seed -> ( List Particle, Random.Seed )
generateExplosionParticles pos color count seed0 =
    List.range 1 count
        |> List.foldl
            (\_ ( acc, s ) ->
                let
                    ( angle, s1 ) = Random.step (Random.float 0 (2 * pi)) s
                    ( speed, s2 ) = Random.step (Random.float 1 4) s1
                    ( life, s3 ) = Random.step (Random.float 15 35) s2
                    particle =
                        { pos = pos
                        , vel = { x = cos angle * speed, y = sin angle * speed }
                        , life = life
                        , maxLife = life
                        , color = color
                        }
                in
                ( particle :: acc, s3 )
            )
            ( [], seed0 )


generateThrustParticles : Ship -> Random.Seed -> ( List Particle, Random.Seed )
generateThrustParticles ship seed0 =
    let
        angleRad = degrees ship.angle
        -- rear of ship
        rearX = ship.pos.x - cos angleRad * shipRadius
        rearY = ship.pos.y - sin angleRad * shipRadius
    in
    List.range 1 2
        |> List.foldl
            (\_ ( acc, s ) ->
                let
                    ( spread, s1 ) = Random.step (Random.float -0.3 0.3) s
                    ( speed, s2 ) = Random.step (Random.float 1 3) s1
                    ( life, s3 ) = Random.step (Random.float 8 18) s2
                    pAngle = angleRad + pi + spread
                    particle =
                        { pos = { x = rearX, y = rearY }
                        , vel = { x = cos pAngle * speed + ship.vel.x * 0.3, y = sin pAngle * speed + ship.vel.y * 0.3 }
                        , life = life
                        , maxLife = life
                        , color = "#ff6600"
                        }
                in
                ( particle :: acc, s3 )
            )
            ( [], seed0 )


asteroidColor : AsteroidSize -> String
asteroidColor size =
    case size of
        Large -> "#888888"
        Medium -> "#aa8866"
        Small -> "#cc9977"



-- ENCODE SCENE FOR RENDERING

encodeScene : Model -> Encode.Value
encodeScene model =
    Encode.object
        [ ( "state", Encode.string (gameStateString model.state) )
        , ( "ship", encodeShip model.ship )
        , ( "bullets", Encode.list encodeBullet model.bullets )
        , ( "asteroids", Encode.list encodeAsteroid model.asteroids )
        , ( "particles", Encode.list encodeParticle model.particles )
        , ( "score", Encode.int model.score )
        , ( "lives", Encode.int model.lives )
        , ( "level", Encode.int model.level )
        , ( "frameCount", Encode.int model.frameCount )
        ]


gameStateString : GameState -> String
gameStateString gs =
    case gs of
        Start -> "start"
        Playing -> "playing"
        GameOver -> "gameover"


encodeVec2 : Vec2 -> Encode.Value
encodeVec2 v =
    Encode.object [ ( "x", Encode.float v.x ), ( "y", Encode.float v.y ) ]


encodeShip : Ship -> Encode.Value
encodeShip ship =
    Encode.object
        [ ( "pos", encodeVec2 ship.pos )
        , ( "vel", encodeVec2 ship.vel )
        , ( "angle", Encode.float ship.angle )
        , ( "thrusting", Encode.bool ship.thrusting )
        , ( "invincible", Encode.float ship.invincible )
        ]


encodeBullet : Bullet -> Encode.Value
encodeBullet b =
    Encode.object
        [ ( "pos", encodeVec2 b.pos )
        , ( "life", Encode.float b.life )
        ]


encodeAsteroid : Asteroid -> Encode.Value
encodeAsteroid a =
    Encode.object
        [ ( "pos", encodeVec2 a.pos )
        , ( "size", Encode.string (asteroidSizeString a.size) )
        , ( "angle", Encode.float a.angle )
        , ( "vertices", Encode.list encodeVec2 a.vertices )
        ]


asteroidSizeString : AsteroidSize -> String
asteroidSizeString size =
    case size of
        Large -> "large"
        Medium -> "medium"
        Small -> "small"


encodeParticle : Particle -> Encode.Value
encodeParticle p =
    Encode.object
        [ ( "pos", encodeVec2 p.pos )
        , ( "life", Encode.float p.life )
        , ( "maxLife", Encode.float p.maxLife )
        , ( "color", Encode.string p.color )
        ]



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame Tick
        , Browser.Events.onKeyDown (Decode.map KeyDown keyDecoder)
        , Browser.Events.onKeyUp (Decode.map KeyUp keyDecoder)
        ]


keyDecoder : Decode.Decoder String
keyDecoder =
    Decode.field "key" Decode.string



-- VIEW

view : Model -> Html Msg
view _ =
    Html.div
        [ Html.Attributes.style "margin" "0"
        , Html.Attributes.style "padding" "0"
        , Html.Attributes.style "display" "flex"
        , Html.Attributes.style "justify-content" "center"
        , Html.Attributes.style "align-items" "center"
        , Html.Attributes.style "min-height" "100vh"
        , Html.Attributes.style "background" "#111"
        ]
        [ Html.canvas
            [ Html.Attributes.id "game-canvas"
            , Html.Attributes.width 800
            , Html.Attributes.height 600
            ]
            []
        ]



-- MAIN

main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
