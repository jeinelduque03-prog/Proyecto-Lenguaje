type Coord = (Int, Int) -- (Fila, Columna)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord]) -- (Robot, CajaObjetivo, CajasDeBloqueo)
err = ((-1,-1), (-1,-1), [])
--1 
initialState :: (Coord) -> (Coord) -> [Coord] -> State
initialState robot obj bloqs
    | valid robot && valid obj && all valid bloqs && noSolapa = (robot, obj, bloqs)
    | otherwise = err
    where
        valid (f,c)
            | f >= 0 && f <= 5 && c >= 0 && c <= 5 = True
            | otherwise = False
        noSolapa = robot /= obj && all (/= robot) bloqs && all (/= obj) bloqs

--2
isValidMove :: State -> Move -> Bool -- (2,2) validU 
isValidMove (robot, obj, bloqs) mov
    | mov == U = valid (a - 1 , b) _ _
    | mov == D = valid (a + 1 , b) _ _
    | mov == L = valid (a , b - 1) _ _
    | mov == R = valid (a , b + 1) _ _
    | otherwise = err

    where
        valid :: State -> Bool
        valid sta
            | initialState (robot)(obj)[bloqs]

{-
isValidMove :: State -> Move -> Bool
isValidMove (robot, obj, bloqs) mov
    | not (within newRobot) = False
    | newRobot == obj = within behindObj && not (occupied behindObj)
    | newRobot `elem` bloqs = within behindBox && not (occupied behindBox)
    | otherwise = True
    where
        (r, c) = robot
        newRobot = case mov of
            U -> (r-1, c)
            D -> (r+1, c)
            L -> (r, c-1)
            R -> (r, c+1)
        behindObj = case mov of
            U -> (r-2, c)
            D -> (r+2, c)
            L -> (r, c-2)
            R -> (r, c+2)
        behindBox = behindObj
        within (f, col) = f >= 0 && f <= 5 && col >= 0 && col <= 5
        occupied coord = coord == obj || elem coord bloqs


-}
