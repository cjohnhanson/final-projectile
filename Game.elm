{- This module contains functions for dealing with events in the game -}
module Game where

import List
import Time
import Keyboard
import Signal
import Char
import Random
import GameConstants exposing (..)



{---------------------- input -----------------------------}

--clock
delta : Signal Time.Time
delta =
   Signal.map Time.inSeconds (Time.fps 35)


--check user input every gameclock cycle
input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map5 Input
       (Signal.map .y Keyboard.arrows)
       (Signal.map .x Keyboard.arrows)
       Keyboard.space
       (Keyboard.isDown (Char.toCode 'R'))
       (Keyboard.enter)


{---------------------- UPSTATE -----------------------------}

--Check if game is active (projectile fired) or passive (waiting for user input) and update things accordingly
upstate : Input -> GameState -> GameState
upstate input gs =
  case (findLoser 0 gs.tanks) of
    Just losingPlayer -> 
      if (Debug.watch "restart2: " input.restart) then
        initState
      else
        gameLost gs losingPlayer
    Nothing ->
      let 
        pro = gs.proj
        tnks = List.map (\t -> groundTank t gs.terrain) gs.tanks
        trn = gs.turn
        scr = gs.screen
        p = input.power
        a = (Debug.watch "input.angle:" input.angle)
        s = input.space
        e = input.enter
      in
        if (scr == Menu) then
          {gs | screen = (if e then Inactive else Menu)}
        else if input.restart then
          {initState | screen = Menu,
                       seed = (snd (xPair gs.seed)),
                       terrain = (getTerrain gs.seed terrainMaps),
                       tanks = (Debug.watch "newTanks" (moveTanks gs.tanks (fst(xPair gs.seed))))}
        else if (scr == Active) then
          let
            newp = updateProjectile pro
          in
            if (hitGround newp gs.terrain) then
              case (hitTank newp tnks 0) of
                Nothing -> 
                  let newPlayer = (changeTurn trn)
                      newVec = (getActiveTank newPlayer tnks).initVelocity
                      newProj = {position = (getActiveTank newPlayer tnks).position,
                                velocity = newVec,
                                acceleration = gravity,
                                damage = dmg }
                  in
                    {gs | proj = newProj,
                     turn = newPlayer,
                     screen = Inactive}
                Just n -> 
                  let newPlayer = (changeTurn trn)
                      newProj = {position = ((getActiveTank newPlayer tnks).position),
                                 velocity = ((getActiveTank newPlayer tnks).initVelocity),
                                 acceleration = gravity,
                                 damage = dmg}
                      newTnks = damageTank newp.damage gs.tanks n
                  in
                    {gs | proj = newProj,
                          turn = newPlayer,
                          screen = Inactive,
                          tanks = newTnks}
            else
              {gs | proj = newp, tanks = tnks}

       else
          let
            newScreen = if s then Active else Inactive
            newPos = (getActiveTank trn tnks).position
            newVec = translateInput p a pro.velocity
            newProj = {position=newPos, velocity=newVec, acceleration=gravity, damage=dmg}
            newTanks = changeInitVel trn newVec tnks
          in
            {gs | proj = newProj,
                  screen = newScreen,
                  tanks = newTanks}

{---------------------- input helpers -----------------------------}

--turn the user's key presses into an initial velocity
translateInput : Int -> Int -> Vector2D -> Vector2D
translateInput pow ang initVec =
  case (pow, ang) of
    (0,0) -> initVec
    _ ->
      let
        initMag = (sqrt ((initVec.x)^2 + (initVec.y)^2))
        initDeg = (atan2 ( initVec.y) ( initVec.x))
        newMag = 
          case pow of
            1 -> initMag + 0.1
            (-1) -> initMag - 0.1
            _ -> initMag
        newAng = 
          case ang of
             -1 -> (min 3.13 (initDeg + 0.01))
             1 -> (max 0.01 (initDeg - 0.01))
             _ -> initDeg
        newX = (newMag*(cos newAng))
        newY = (newMag*(sin newAng))
      in
        {x = newX,
         y = newY}

{---------------------- projectile helpers -----------------------------}

--move a projectile 1 step based on current velocity and acceleration           
updateProjectile : Projectile -> Projectile
updateProjectile p =
  { p | position = (addVector2D p.position p.velocity),
        velocity = (addVector2D p.velocity p.acceleration)}

addVector2D : Vector2D -> Vector2D -> Vector2D 
addVector2D a b = 
  {x = (a.x + b.x),
   y = (a.y + b.y)}


{---------------------- collision helpers -----------------------------}

hitGround : Projectile -> List (Float, Float) -> Bool
hitGround p grs =
  case grs of 
    (x1,y1)::(x2,y2)::grs' ->
        let 
          pX = p.position.x
          pY = p.position.y
        in
          if pX >= x1 && pX < x2 then
            let 
              newY = ((y2-y1)/(x2-x1))*(pX-x1) + y1
            in
              (pY <= newY)
          else  
            hitGround p ((x2, y2)::grs')
    _ -> True

--check if any tank was hit. NOTE: tanks cannot be w/in hitRadius of each other, since this will only catch 1 tank at a time
hitTank : Projectile -> List Tank -> Int -> Maybe Int
hitTank p ts i =
  case ts of
    []-> Nothing 
    hd::tl->
      if (abs(hd.position.x - p.position.x) < hitRadius) then
        let thing = Debug.watch "health of tank:" hd.health in
          Just i
      else
        hitTank p tl (i + 1)

--reduce the health of a tank 
damageTank : Int -> List Tank -> Int -> List Tank
damageTank dam ts tnk =
  case (tnk, ts) of
    (0, hd::tl) -> {hd | health = (hd.health - dam)}::tl
    (_, hd::tl) -> hd::(damageTank dam tl (tnk - 1))
    (_, []) -> Debug.crash "damageTank: tank does not exist"

--see if any tank is dead
findLoser : Int -> List Tank -> Maybe Int
findLoser n tnks =
  case tnks of
    [] -> Nothing
    hd::tnks' -> if (hd.health <= 0) then (Just n)
                 else (findLoser (n + 1) tnks')


--if any tank is dead, indicate to view that it should display the gameover screen
gameLost: GameState -> Loser -> GameState
gameLost gs l =
  {gs | loser = Just l}

{---------------------- tank helpers -----------------------------}

--place tanks at ground level
groundTank : Tank -> List (Float, Float) -> Tank
groundTank t grs =
  case grs of 
    (x1,y1)::(x2,y2)::grs' ->
        let 
          tankX = t.position.x
          tankY = t.position.y
        in
          if tankX >= x1 && tankX < x2 then
            let 
              newY = ((y2-y1)/(x2-x1))*(tankX-x1) + y1 + 10
            in
              {t | position = {x=tankX, y=newY}}
          else  
            groundTank t ((x2, y2)::grs')
    _ -> Debug.crash "tank off map"

--get the tank associated with a player #
getActiveTank : Int -> List Tank -> Tank 
getActiveTank n tnks =
  case tnks of
    [] -> Debug.crash "getActiveTanks: given empty list"
    hd::tnks' -> if (n == 0) then hd 
              else getActiveTank (n - 1) tnks'

--apply new initial velocity to a tank
changeInitVel : Int -> Vector2D ->  List Tank -> List Tank
changeInitVel n v tnks =
  case tnks of
    [] -> Debug.crash "changeInitVel: given empty list"
    hd::tnks' -> if (n == 0) then ({hd | initVelocity = v})::tnks'
                 else hd::(changeInitVel (n - 1) v tnks')

--change x location of tanks to new position
moveTanks : List Tank -> (Float, Float) -> List Tank
moveTanks ts (x1, x2) =
  List.map2 (\t -> \newX ->  {t|position = {x = newX, y = 100}}) ts [x1, x2]
  
{---------------------- misc. helpers -----------------------------}

--switch active player
changeTurn : Int -> Int
changeTurn n =
  case n of
    1 -> 0
    _ -> 1

--get 2 random x values. Used to randomize tank position. 
xPair : Random.Seed -> ((Float, Float), Random.Seed)
xPair seed1 = 
  let
   gen1 = Random.float -500 -10 
   (x1, seed2) = Random.generate gen1 seed1
   gen2 = Random.float 10 500 
   (x2, _) = Random.generate gen2 seed2
  in
    ((x1, (Debug.watch "x2gen:" x2)), seed2)

--pick a random terrain from the list of availible maps
getTerrain : Random.Seed -> List (List (Float, Float)) -> List (Float, Float)
getTerrain s mps =
    let
      g = Random.int 1 (List.length mps)
      i = fst (Random.generate g s)
      foo l index =
        case (l, index) of
          ((x::xs), _) ->
             if index == 1 then
              x
            else
              foo xs (index-1)
          ([], _) -> [(-1000, 0), (1000, 0)]
    in
      foo mps i  