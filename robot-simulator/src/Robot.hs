module Robot
    ( Bearing(East, North, South, West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

type Coordinates = (Integer, Integer)

data Bearing     = North
                 | East
                 | South
                 | West 
                 deriving (Eq, Show)

data Robot       = Robot Bearing Coordinates

bearing :: Robot -> Bearing
bearing     (Robot b      _    ) = b

coordinates :: Robot -> Coordinates
coordinates (Robot _      c    ) = c

mkRobot :: Bearing -> Coordinates -> Robot
mkRobot                          = Robot

turnLeft :: Robot -> Robot
turnLeft    (Robot North  c    ) = Robot West  c
turnLeft    (Robot East   c    ) = Robot North c
turnLeft    (Robot South  c    ) = Robot East  c
turnLeft    (Robot West   c    ) = Robot South c

turnRight :: Robot -> Robot
turnRight   (Robot North  c    ) = Robot East  c
turnRight   (Robot East   c    ) = Robot South c
turnRight   (Robot South  c    ) = Robot West  c
turnRight   (Robot West   c    ) = Robot North c

advance :: Robot -> Robot
advance     (Robot North (x, y)) = Robot North (x    , y + 1)
advance     (Robot East  (x, y)) = Robot East  (x + 1, y    )
advance     (Robot South (x, y)) = Robot South (x    , y - 1)
advance     (Robot West  (x, y)) = Robot West  (x - 1, y    )

move :: Robot -> String -> Robot
move        robot  []            =                 robot
move        robot  ('L':xs)      = move (turnLeft  robot) xs
move        robot  ('R':xs)      = move (turnRight robot) xs
move        robot  ('A':xs)      = move (advance   robot) xs
move        robot  _             =                 robot
