{- This module contains the main signal loop of the game and associated functions/signals -}

import Game exposing (..)
import GameConstants exposing (..)
import Signal
import Graphics.Element as E
import Graphics.Collage as C
import Color 
import Window
import Text 
import String

{---------------------------------- top level functions --------------------------------}

main =
  Signal.map2 view Window.dimensions stateOverTime

stateOverTime: Signal GameState
stateOverTime = Signal.foldp upstate initState input

view : (Int, Int) -> GameState -> E.Element
view (w, h) gs =
  let
    elementList = case (gs.loser, gs.screen) of
      (Nothing, Inactive) -> [ (projForm gs.proj), (statusString gs)  ] ++  (tankForm gs.tanks) ++ [terrainForm gs.terrain] ++ [restartString]
      (Nothing, Active) -> [ (projForm gs.proj), (statusString gs)  ] ++  (tankForm gs.tanks) ++ [terrainForm gs.terrain] ++ [restartString]
      (_ , Menu) -> [startScreen w h]
      (Just x, _) -> [ loserForm (x + 1), restartString]
  in
    C.collage w h ((C.filled lightGrey <| C.rect (toFloat w) (toFloat h))::elementList)


{---------------------------- view helpers (most of these just make various forms) ----------------------------}

--start screen
startScreen : Int -> Int->  C.Form
startScreen w h =
    C.toForm <|
    E.color lightGrey <| 
       E.container w h E.middle <|
       E.flow E.down <| [
      (E.container w (h //2) E.middle <|
       E.centered <|
       Text.line Text.Under <|
       Text.height ((toFloat h) / 5) <| 
       Text.color ourBlue <| 
       Text.bold <| 
       Text.fromString <|
       "Final Projectile"),
      (E.container w (h // 5) E.middle <|
       E.centered <|
       Text.line Text.Under <|
       Text.height ((toFloat h) / 10) <|
       Text.color ourYellow <|
       Text.bold <|
       Text.fromString <|
       "Press Enter to Play")]


--'press r to restart'
restartString: C.Form
restartString = C.move (0, -200) (C.toForm (E.centered(Text.fromString "CONTROLS: \n up/down to control power \n left/right to control angle \n spacebar to fire \n 'R' to restart") ))

--game end screen
loserForm : Int -> C.Form
loserForm x = 
  C.toForm (E.centered (Text.fromString("GAME OVER: Player "++ (toString x)++ " is a loser!!!!!"  )))  

--draw terrain
terrainForm : List (Float, Float) -> C.Form
terrainForm p =
  let style = (C.solid darkGrey) in
    C.traced {style | width = 10.0} (C.path p)

--draw tanks
tankForm : List (Tank) -> List C.Form
tankForm ts =
  case ts of
    [] -> []
    t::ts' -> let (x, y) = (t.initVelocity.x, t.initVelocity.y) in
              (C.move (( t.position.x), ( t.position.y)) (C.filled t.color (C.rect (fst t.size) (snd t.size))))::
              (C.move ((t.position.x), (t.position.y + (fst t.size * (1 / 3)))) (C.filled t.color (C.rect ((fst t.size) * (2/3)) ((snd t.size) * (14/13)))))::
              ((C.traced 
                        {color = t.color, width = ((fst t.size) * 0.2), cap = C.Flat, join = C.Smooth, dashing = [], dashOffset = 0}
                        (C.segment ((t.position.x), (t.position.y)) ((t.position.x + ((cos (atan2 y x)) * ((fst t.size) * 1.3))), 
                                                                     (t.position.y + ((sin (atan2 y x)) * ((fst t.size) * 1.3)))))))::
              (tankForm ts')

--draw projectile
projForm : Projectile -> C.Form
projForm p = 
  C.move (( p.position.x), ( p.position.y)) (C.filled darkGrey (C.circle 5.0))

--info about state in user-readable form
statusString : GameState -> C.Form
statusString gs =
  let
    pwrStr = if (gs.screen == Inactive )then ((String.left 6 (toString (sqrt <| ((gs.proj.velocity.x^2) + (gs.proj.velocity.y^2))))))
             else "Waiting..."
    angStr = if (gs.screen == Inactive) then ((String.left 6 (toString (57.2958*(atan2 gs.proj.velocity.y gs.proj.velocity.x)))))
             else "               "
  in
    C.move (0, -100) (C.toForm (E.centered (Text.fromString
           (("Power: " ++ pwrStr ++ "\n" ++ 
             " Angle: " ++ angStr ++ "\n" ++
             " Player: " ++ ((toString (gs.turn + 1))) ++ "\n" ++
             (tanksHealth 1 gs.tanks))))))

--statusString cont. 
tanksHealth : Int -> List Tank -> String
tanksHealth n ts =
  case ts of
    [] -> ""
    t::ts' -> " Health of tank " ++ (toString n) ++ ": " ++ (toString t.health) ++ "\n" ++ (tanksHealth (n+1) ts')

