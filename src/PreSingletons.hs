module PreSingletons where

import Data.Kind (Type)

-- The phantom type
data Foo a = MkFoo

data DoorState
  = Opened
  | Closed
  | Locked
  deriving (Show, Eq)

-- Here, "DoorState" refers to the *kind* "DoorState" with the
-- type constructors : 'Opened, 'Closed and 'Locked, as opposed
-- to the *type* "DoorState" with the value constructors:
-- Opened, Closed and Locked.
-- data Door (s :: DoorState) = UnsafeMkDoor {doorMaterial :: String}

-- Defining door with GADT syntax.
data Door :: DoorState -> Type where
  UnsafeMkDoor :: {doorMaterial :: String} -> Door s

{-
With GADTs, we define types by giving the type of its
constructors e.g.

UnsafeMkDoor :: {doorMaterial :: String} -> Door s
-}

closeDoor :: Door 'Opened -> Door 'Closed
closeDoor (UnsafeMkDoor m) = UnsafeMkDoor m

{-

Imagine if we called (closeDoor . closeDoor) on something of type
Door 'Opened. We would get a type error, which would be a good
thing, as one oughtn't be able to close a closed door! This
demonstrates how phantom types are useful for catching logic
errors at compile-time, rather than run-time.

We are encoding pre-conditions and post-conditions directly into
the type!

-}

-- Singleton pattern for `DoorState`.
data SDoorState :: DoorState -> Type where
  SOpened :: SDoorState 'Opened
  SClosed :: SDoorState 'Closed
  SLocked :: SDoorState 'Locked

lockDoor :: Door 'Closed -> Door 'Locked
lockDoor (UnsafeMkDoor m) = UnsafeMkDoor m

lockAnyDoor :: SDoorState s -> Door s -> Door 'Locked
lockAnyDoor = \case
  SOpened -> lockDoor . closeDoor
  SClosed -> lockDoor
  SLocked -> id

-- doorStatus :: SDoorState s -> Door s -> DoorState
-- doorStatus SOpened _ = Opened
-- doorStatus SClosed _ = Closed
-- doorStatus SLocked _ = Locked

fromSDoorState :: SDoorState s -> DoorState
fromSDoorState SOpened = Opened
fromSDoorState SClosed = Closed
fromSDoorState SLocked = Locked

doorStatus :: SDoorState s -> Door s -> DoorState
doorStatus s _ = fromSDoorState s

{-
Recovering implicit passing.

Isn't it a pain to have to pass `SDoorState s` explicitly every
time? We can recover implicit passing with typeclasses.
-}

class SingDSI s where
  singDS :: SDoorState s

instance SingDSI 'Opened where
  singDS = SOpened
instance SingDSI 'Closed where
  singDS = SClosed
instance SingDSI 'Locked where
  singDS = SLocked

lockAnyDoor_ :: SingDSI s => Door s -> Door 'Locked
lockAnyDoor_ = lockAnyDoor singDS

doorStatus_ :: SingDSI s => Door s -> DoorState
doorStatus_ = doorStatus singDS
