module PostSingletons where

import Data.Singletons.TH (genSingletons)

data DoorState = Opened | Closed | Locked
  deriving (Show, Eq)

genSingletons [''DoorState]

newtype Door (s :: DoorState) = UnsafeMkDoor {material :: String}

unlockDoor :: Int -> Door 'Locked -> Maybe (Door 'Closed)
unlockDoor n d
  | odd n = Just d
  | otherwise = Nothing
