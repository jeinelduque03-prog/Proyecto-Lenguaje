type Coord = (Int, Int) -- (Fila, Columna)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord]) -- (Robot, CajaObjetivo, CajasDeBloqueo)
--1 
initialState :: (Coord) -> (Coord) -> [Coord] -> State
initialState robot obj bloqs
    | valid robot && valid obj && all valid bloqs && noSolapa = (robot, obj, bloqs)
    | otherwise = ((-1,-1), (-1,-1), [])
    where
        valid (f,c)
            | f >= 0 && f <= 5 && c >= 0 && c <= 5 = True
            | otherwise = False
        noSolapa = robot /= obj && all (/= robot) bloqs && all (/= obj) bloqs

--2
