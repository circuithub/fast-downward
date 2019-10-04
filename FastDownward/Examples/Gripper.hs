{-# language DisambiguateRecordFields #-}

-- | This module is an example of using @fast-downward@ to solve a problem of
-- transporting balls between rooms using a robot. See the source listing for
-- this file for the full example, and see
-- <https://ocharles.org.uk/blog/posts/2018-12-25-fast-downward.html> for a
-- detailed walk through.

module FastDownward.Examples.Gripper where

import Control.Monad
import qualified FastDownward.Exec as Exec
import FastDownward


data Room = RoomA | RoomB
  deriving (Eq, Ord, Show)


adjacent :: Room -> Room
adjacent RoomA = RoomB
adjacent RoomB = RoomA


data BallLocation = InRoom Room | InGripper
  deriving (Eq, Ord, Show)


data GripperState = Empty | HoldingBall
  deriving (Eq, Ord, Show)


type Ball = Var BallLocation


type Gripper = Var GripperState


data Action = PickUpBall | SwitchRooms | DropBall
  deriving (Show)


problem :: Problem (SolveResult Action)
problem = do
  balls <- replicateM 4 (newVar (InRoom RoomA))
  robotLocation <- newVar RoomA
  grippers <- replicateM 2 (newVar Empty)

  let
    pickUpBallWithGrippper :: Ball -> Gripper -> Effect Action
    pickUpBallWithGrippper b gripper = do
      Empty <- readVar gripper

      robotRoom <- readVar robotLocation
      ballLocation <- readVar b
      guard (ballLocation == InRoom robotRoom)

      writeVar b InGripper
      writeVar gripper HoldingBall

      return PickUpBall


    moveRobotToAdjacentRoom :: Effect Action
    moveRobotToAdjacentRoom = do
      modifyVar robotLocation adjacent
      return SwitchRooms


    dropBall :: Ball -> Gripper -> Effect Action
    dropBall b gripper = do
      HoldingBall <- readVar gripper
      InGripper <- readVar b

      robotRoom <- readVar robotLocation
      writeVar b (InRoom robotRoom)

      writeVar gripper Empty

      return DropBall


  solve
    Exec.bjolp
    ( [ pickUpBallWithGrippper b g | b <- balls, g <- grippers ]
        ++ [ dropBall b g | b <- balls, g <- grippers ]
        ++ [ moveRobotToAdjacentRoom ]
    )
    [ b ?= InRoom RoomB | b <- balls ]


main :: IO ()
main = do
  res <- runProblem problem
  case res of
    Solved plan -> do
      putStrLn "Found a plan!"
      zipWithM_
        ( \i step -> putStrLn ( show i ++ ": " ++ show step ) )
        [ 1::Int .. ]
        ( totallyOrderedPlan plan )

    _ ->
      putStrLn "Couldn't find a plan!"
