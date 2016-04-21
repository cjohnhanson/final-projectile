{-- type definitions and global constants --}
module GameConstants where

import Random
import Color

{---------------------- type definitions -----------------------------}

type Screen = Active | Menu | Inactive

type alias GameState = { proj : Projectile,
                         tanks : List Tank,
                         turn : Int,
                         screen : Screen,
                         loser : Maybe Loser,
                         terrain : List (Float, Float) ,
                         seed : Random.Seed
                       } 

type alias Tank = { health : Int,
                    position : Vector2D,
                    size : (Float, Float),
                    color : Color.Color,
                    initVelocity : Vector2D }

type alias Projectile = { position : Vector2D,
                          velocity : Vector2D,
                          acceleration : Vector2D,
                          damage : Int }

type alias Vector2D = { x : Float,
                        y : Float }
    

type alias Input = { power : Int,
                     angle : Int,
                     space : Bool,
                     restart: Bool,
                     enter : Bool }

type alias Loser = Int

{---------------------- initial state values -----------------------------}

initState : GameState
initState = { proj = initProj,
              tanks = [tank1, tank2],
              turn = 0,
              screen = Menu,
              loser = Nothing,
              terrain = default,
              seed = Random.initialSeed 46837967489
            }

initProj : Projectile
initProj = { acceleration = gravity,
             damage = dmg,
             position = tank1.position,
             velocity = tank1.initVelocity}


tank1 : Tank
tank1 = { health = 100,
          position = { x = -350 , y = 100},
          size = (20, 8),
          color = ourYellow,
          initVelocity = {x = 10, y = 8}}


tank2 : Tank
tank2 = { health = 100,
          position = {x = 350, y = 100},
          size = (20, 8),
          color = ourBlue,
          initVelocity = {x = -10, y = 8}}

{---------------------- global constants -----------------------------}
darkGrey : Color.Color
darkGrey = Color.rgb 103 112 119

lightGrey : Color.Color 
lightGrey = Color.rgb 181 181 183

ourBlue : Color.Color
ourBlue = Color.rgb 37 40 57

ourYellow : Color.Color
ourYellow = Color.rgb 242 182 50

hitRadius : Float
hitRadius = 20.0

dmg : Int
dmg = 50

gravity: Vector2D
gravity = {x=0, y=-1}

{---------------------- terrain -----------------------------}

terrainMaps = [cliff, default, fort, hill]


default: List (Float, Float)
default = [(-1000, 0), (-500, -35), (-234, 56), (-30, 16), (0, -20), (50, 34), (250, -10), (400, -75), (1000, 0)]

hill: List (Float, Float)
hill = [(-1000, 0), 
        (-500, -100),
        (-450, -90),
        (-400, -75),
        (-325, -80),
        (-300, -60),
        (-200, -10),
        (-150, 10),
        (-100, 30),
        (-25, 45),
        (0, 50),
        (25, 45),
        (100, 30),
        (150, 10),
        (200, -10),
        (300, -60),
        (325,-80),
        (400,-75),
        (450,-90),
        (500,-100),
        (1000,0)]


cliff: List (Float, Float)
cliff = [(-1000, 100),
        
         (-500, 200), 
         (-450, 255),
         (-400, 252),
         (-350, 255),
         (-300, 251),
         (-250, 250),
         (-200, 0),
         (0, 10),
         (300, 0),
         (1000, -10)]

fort: List (Float, Float)
fort = [(-1000, 0),
        (-500, 10),
        (-425, 20),
        (-200, 10),
        (-100, -20),
       
        (-25, 0),
        (-20, 200),
        (20, 200),
        (20, 25),
        (30, -30),
        (50, -40),
        (200, -5),
        (1000, 0)]
