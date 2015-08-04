import Color exposing (Color)
import Graphics.Element exposing (..)
import Graphics.Input exposing (clickable, button)
import Math.Matrix4 as M4 exposing (Mat4)
import Math.Vector3 exposing (Vec3, vec3)
import Signal exposing (Mailbox, Address)
import Time exposing (Time, fps)
import WebGL exposing (Triangle, Entity, Shader, webgl, entity)
import Window

type alias Model =
  { cubes : List Cube
  , animation : Maybe Animation
  }

type alias Cube =
  { mesh : List (Triangle Vertex)
  , transformation : Mat4
  , sides : List Side
  }

type alias Animation =
  { side : Side
  , direction : Direction
  , theta : Float -- how many radians the side has rotated. changes every frame.
  }

type Action
  = Move Side Direction

type Direction
  = Clockwise
  | Counterclockwise

type Side
  = Up
  | Down
  | Left
  | Right
  | Front
  | Back

type alias Uniforms =
  { rotation : Mat4
  , transformation : Mat4
  , perspective : Mat4
  , camera : Mat4
  , shade : Float
  }

main : Signal Element
main =
  Signal.map2
    (view actions.address)
    Window.dimensions
    (Signal.foldp update initialModel input)

view : Signal.Address (Maybe Action) -> (Int, Int) -> Model -> Element
view address (w,h) model =
  let buttons = mkButtons w h address
      glSize = min 800 (min w h)
      cube =  scene model
              |> webgl (glSize,glSize)
              |> container w h middle
              |> color Color.black
  in flow outward [cube, buttons]

update : (Maybe Action, Float) -> Model -> Model
update (action, dt) model =
  case model.animation of
    Nothing ->
      case action of
        Nothing -> model -- do nothing
        Just act -> -- start animation
          { model | animation <- Just (actionToAnimation act) }
    Just a ->
      let lastFrame = a.theta == maxRot || a.theta == minRot
      in if lastFrame
         then -- animation over; apply rotation and reset
           {model | cubes <- List.map (applyRotation a) model.cubes
                  , animation <- Nothing }
         else -- next frame of animation
           {model | animation <- Just { a | theta <- newTheta a dt }}

input : Signal (Maybe Action, Float)
input =
  let tick = fps 60
      const x = (\_ -> x)
      moves = Signal.merge actions.signal (Signal.map (const Nothing) tick)
  in Signal.map2 (,) moves tick

actions : Mailbox (Maybe Action)
actions = Signal.mailbox Nothing

initialModel =
  { cubes = initialCubes
  , animation = Nothing
  }

mkButtons : Int -> Int -> Address (Maybe Action) -> Element
mkButtons w h addr =
  let
    mkButton action label = button (Signal.message addr (Just action)) label
    u = flow left
      [ mkButton (Move Up Clockwise) "Up"
      , mkButton (Move Up Counterclockwise) "Up'" ]
    d = flow left
      [ mkButton (Move Down Clockwise) "Down"
      , mkButton (Move Down Counterclockwise) "Down'" ]
    l = flow left
      [ mkButton (Move Left Clockwise) "Left"
      , mkButton (Move Left Counterclockwise) "Left'" ]
    r = flow right
      [ mkButton (Move Right Clockwise) "Right"
      , mkButton (Move Right Counterclockwise) "Right'" ]
    f = flow left
      [ mkButton (Move Front Clockwise) "Front"
      , mkButton (Move Front Counterclockwise) "Front'" ]
    b = flow right
      [ mkButton (Move Back Clockwise) "Back"
      , mkButton (Move Back Counterclockwise) "Back'" ]
  in flow inward
      [ container w h midTop u
      , container w h midBottom d
      , container w h middle
        (flow inward
          [ container w (h//2) topRight r
          , container w (h//2) bottomRight f
          , container w (h//2) topLeft b
          , container w (h//2) bottomLeft l
          ])
      ]

newTheta : Animation -> Time -> Float
newTheta { side, direction, theta } dt =
  let theta' =
    case direction of
      Clockwise        -> theta + (animationSpeed * dt)
      Counterclockwise -> theta - (animationSpeed * dt)
  in clamp minRot maxRot theta'

animationSpeed : Float
animationSpeed = 0.006

maxRot : Float
maxRot = pi/2

minRot : Float
minRot = (-1) * maxRot

actionToAnimation : Action -> Animation
actionToAnimation (Move side direction)
  = { side = side, direction = direction, theta = 0 }

initialCubes =
  {- this list was generated in ghci with:
    data Side = Up | Down | Left | Right | Front | Back deriving Show
    let xs = [(1,[Right]),(0,[]),(-1,[Left])]
    let ys = [(1,[Up]),(0,[]),(-1,[Down])]
    let zs = [(1,[Front]),(0,[]),(-1,[Back])]
    let munge ((x,sx),(y,sy),(z,sz)) = (x,y,z,sx++sy++sz)
    map munge [(x,y,z) | x <- xs, y <- ys, z <-zs]
  -}
  let cubes =
    [ (1,1,1,[Right,Up,Front])
    , (1,1,0,[Right,Up])
    , (1,1,-1,[Right,Up,Back])
    , (1,0,1,[Right,Front])
    , (1,0,0,[Right])
    , (1,0,-1,[Right,Back])
    , (1,-1,1,[Right,Down,Front])
    , (1,-1,0,[Right,Down])
    , (1,-1,-1,[Right,Down,Back])
    , (0,1,1,[Up,Front])
    , (0,1,0,[Up])
    , (0,1,-1,[Up,Back])
    , (0,0,1,[Front])
    , (0,0,0,[])
    , (0,0,-1,[Back])
    , (0,-1,1,[Down,Front])
    , (0,-1,0,[Down])
    , (0,-1,-1,[Down,Back])
    , (-1,1,1,[Left,Up,Front])
    , ( -1,1,0,[Left,Up])
    , (-1,1,-1,[Left,Up,Back])
    , (-1,0,1,[Left,Front])
    , (-1,0,0,[Left])
    , (-1,0,-1,[Left,Back])
    , (-1,-1,1,[Left,Down,Front])
    , (-1,-1,0,[Left,Down])
    , (-1,-1,-1,[Left,Down,Back])
    ]
      s = 0.45
  in List.map
    (\(x,y,z,sides) ->
      { transformation =
          M4.makeTranslate (vec3 x y z) `M4.mul` M4.makeScale (vec3 s s s)
      , mesh = cubeMesh
      , sides = sides })
    cubes

scene : Model -> List Entity
scene model =
  let entity' cube = entity vertexShader fragmentShader cube.mesh (uniforms model cube)
  in List.map entity' model.cubes

uniforms : Model -> Cube -> Uniforms
uniforms model cube =
  { rotation =
      case model.animation of
        Just anim ->
          if anim.side `List.member` cube.sides
          then M4.makeRotate anim.theta (axis anim)
          else M4.identity
        Nothing -> M4.identity
  , transformation = cube.transformation
  , perspective = M4.makePerspective 45 1 0.01 100
  , camera = M4.makeLookAt (vec3 -5 5 5) (vec3 0 0 0) (vec3 0 1 0)
  , shade = 1.0
  }

axis : Animation -> Vec3
axis {side, direction, theta} =
  case side of
    Left  -> vec3  1  0  0
    Right -> vec3 -1  0  0
    Down  -> vec3  0  1  0
    Up    -> vec3  0 -1  0
    Back  -> vec3  0  0  1
    Front -> vec3  0  0 -1

-- apply a 90 degree rotation (around the origin) to a cube.
-- we'll use this on the last frame of an animation so we can
-- reset the cube's rotation matrix
applyRotation : Animation -> Cube -> Cube
applyRotation anim cube =
  if anim.side `List.member` cube.sides
  then
    let theta = case anim.direction of
                  Clockwise        -> maxRot
                  Counterclockwise -> minRot
        rotation = M4.makeRotate theta (axis anim)
    in { cube | transformation <- M4.mul rotation cube.transformation
              , sides <- newSides anim.side anim.direction cube.sides }
  else cube

newSides : Side -> Direction -> List Side -> List Side
newSides side direction sides =
  let spin =
    case side of
      Up ->
        spinner direction
          [Left, Up, Back]
          [Right, Up, Back]
          [Right, Up, Front]
          [Left, Up, Front]
          [Left, Up]
          [Up, Back]
          [Right, Up]
          [Up, Front]
      Right ->
        spinner direction
          [Right, Up, Front]
          [Right, Up, Back]
          [Right, Down, Back]
          [Right, Down, Front]
          [Right, Up]
          [Right, Back]
          [Right, Down]
          [Right, Front]
      Front ->
        spinner direction
          [Left, Up, Front]
          [Right, Up, Front]
          [Right, Down, Front]
          [Left, Down, Front]
          [Up, Front]
          [Right, Front]
          [Down, Front]
          [Left, Front]
      Down ->
        spinner direction
          [Left, Down, Front]
          [Right, Down, Front]
          [Right, Down, Back]
          [Left, Down, Back]
          [Down, Front]
          [Right, Down]
          [Down, Back]
          [Left, Down]
      Left ->
        spinner direction
          [Left, Down, Front]
          [Left, Down, Back]
          [Left, Up, Back]
          [Left, Up, Front]
          [Left, Front]
          [Left, Down]
          [Left, Back]
          [Left, Up]
      Back ->
        spinner direction
          [Left, Down, Back]
          [Right, Down, Back]
          [Right, Up, Back]
          [Left, Up, Back]
          [Left, Back]
          [Down, Back]
          [Right, Back]
          [Up, Back]
  in spin sides

spinner : Direction
  -> List s -> List s -> List s -> List s -- corners. must be in clockwise order
  -> List s -> List s -> List s -> List s -- edges. must be in clockwise order
  -> (List s -> List s) -- a function that will update the sides of a cube
spinner direction a b c d p q r s =
  let spin x =
    case direction of
      Clockwise ->
          if | x == a -> b
             | x == b -> c
             | x == c -> d
             | x == d -> a
             | x == p -> q
             | x == q -> r
             | x == r -> s
             | x == s -> p
             | otherwise -> x
      Counterclockwise ->
          if | x == a -> d
             | x == d -> c
             | x == c -> b
             | x == b -> a
             | x == p -> s
             | x == s -> r
             | x == r -> q
             | x == q -> p
             | otherwise -> x
  in spin


-- MESHES

type alias Vertex =
    { color : Vec3
    , position : Vec3
    }

cubeMesh : List (Triangle Vertex)
cubeMesh =
  let
    rft = vec3  1  1  1   -- right, front, top
    lft = vec3 -1  1  1   -- left,  front, top
    lbt = vec3 -1  1 -1
    rbt = vec3  1  1 -1
    rbb = vec3  1 -1 -1
    rfb = vec3  1 -1  1
    lfb = vec3 -1 -1  1
    lbb = vec3 -1 -1 -1
  in
    List.concat
      [ face Color.green  rft rfb rbb rbt   -- right
      , face Color.white  rft rfb lfb lft   -- front
      , face Color.red    rft lft lbt rbt   -- top
      , face Color.orange rfb lfb lbb rbb   -- bottom
      , face Color.blue   lft lfb lbb lbt   -- left
      , face Color.yellow rbt rbb lbb lbt   -- back
      ]

face : Color -> Vec3 -> Vec3 -> Vec3 -> Vec3 -> List (Triangle Vertex)
face rawColor a b c d =
  let
    color =
      let c = Color.toRgb rawColor in
      vec3
          (toFloat c.red / 255)
          (toFloat c.green / 255)
          (toFloat c.blue / 255)

    vertex position =
        Vertex color position
  in
    [ (vertex a, vertex b, vertex c)
    , (vertex c, vertex d, vertex a)
    ]


-- SHADERS

vertexShader : Shader { attr | position:Vec3, color:Vec3 }
                      { unif | rotation:Mat4, transformation:Mat4, perspective:Mat4, camera:Mat4 }
                      { vcolor:Vec3 }
vertexShader = [glsl|

attribute vec3 position;
attribute vec3 color;
uniform mat4 perspective;
uniform mat4 camera;
uniform mat4 transformation;
uniform mat4 rotation;
varying vec3 vcolor;
void main () {
    gl_Position = perspective * camera * rotation * transformation * vec4(position, 1.0);
    vcolor = color;
}

|]

fragmentShader : Shader {} { u | shade:Float } { vcolor:Vec3 }
fragmentShader = [glsl|

precision mediump float;
uniform float shade;
varying vec3 vcolor;
void main () {
    gl_FragColor = shade * vec4(vcolor, 1.0);
}

|]
