module Part1.PostSingletons where

import Data.Singletons (SingI (sing))
import Data.Singletons.TH (genSingletons)

newtype Door (s :: DoorState) = UnsafeMkDoor {material :: String}
  deriving (Show)

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

genSingletons [''DoorState]

-- Exercises
unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n (UnsafeMkDoor m)
  | odd n = Just $ UnsafeMkDoor m
  | otherwise = Nothing

openDoor :: Door 'Closed -> Door 'Opened
openDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockedDoor :: Door 'Locked
lockedDoor = UnsafeMkDoor "Oak"

openAnyDoor :: SingI s => Int -> Door s -> Maybe (Door 'Opened)
openAnyDoor n = openAnyDoor' sing
  where
    openAnyDoor' :: SDoorState s -> Door s -> Maybe (Door 'Opened)
    openAnyDoor' = \case
      SOpened -> Just
      SClosed -> Just . openDoor
      SLocked -> fmap openDoor . unlockDoor n
