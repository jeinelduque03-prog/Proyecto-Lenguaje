type Coord = (Int, Int)
data Move = U | D | L | R deriving (Show, Eq)
type State = (Coord, Coord, [Coord])

initialState :: Coord -> Coord -> [Coord] -> State
initialState robot obj bloqs
    | valid robot && valid obj && all valid bloqs && noSolapa = (robot, obj, bloqs)
    | otherwise = ((-1,-1), (-1,-1), [])
    where
        valid (f, c) = f >= 0 && f <= 5 && c >= 0 && c <= 5
        noSolapa = robot /= obj && all (/= robot) bloqs && all (/= obj) bloqs

isValidMove :: State -> Move -> Bool
isValidMove (robot, obj, bloqs) mov
    | dentro nuevaPos == False = False
    | nuevaPos == obj          = dentro detras && (detras /= obj && all (/= detras) bloqs)
    | elem nuevaPos bloqs      = dentro detras && (detras /= obj && all (/= detras) bloqs)
    | otherwise                = True
    where
        (f, c) = robot
        nuevaPos = case mov of
            U -> (f-1, c)
            D -> (f+1, c)
            L -> (f, c-1)
            R -> (f, c+1)
        detras = case mov of
            U -> (f-2, c)
            D -> (f+2, c)
            L -> (f, c-2)
            R -> (f, c+2)
        dentro (fil, col) = fil >= 0 && fil <= 5 && col >= 0 && col <= 5

applyMove :: State -> Move -> State
applyMove (robot, obj, bloqs) mov
    | nuevaPos == obj     = (nuevaPos, sigObj, bloqs)
    | elem nuevaPos bloqs = (nuevaPos, obj, reemplazar nuevaPos sigCaja bloqs)
    | otherwise           = (nuevaPos, obj, bloqs)
    where
        (f, c) = robot
        nuevaPos = case mov of
            U -> (f-1, c)
            D -> (f+1, c)
            L -> (f, c-1)
            R -> (f, c+1)
        sigObj = case mov of
            U -> (f-2, c)
            D -> (f+2, c)
            L -> (f, c-2)
            R -> (f, c+2)
        sigCaja = sigObj
        reemplazar old new [] = []
        reemplazar old new (x:xs)
            | x == old  = new : xs
            | otherwise = x : reemplazar old new xs

solveWarehouse :: State -> (Int, [State])
solveWarehouse estado
    | estado == ((-1,-1), (-1,-1), []) = (0, [])
    | otherwise                        = buscar [(estado, [estado])] []
    where
        buscar [] _ = (0, [])
        buscar ((e, camino):cola) visitados
            | meta e           = (longitud camino - 1, camino)
            | elem e visitados = buscar cola visitados
            | otherwise        = buscar (cola ++ generarNuevos e camino (e:visitados)) (e : visitados)
        
        meta (_, obj, _) = obj == (5,5)
        
        longitud []     = 0
        longitud (x:xs) = 1 + longitud xs

        generarNuevos estadoActual caminoActual visitadosTotales = 
            procesarMovimientos [U, D, L, R] estadoActual caminoActual visitadosTotales

        procesarMovimientos [] _ _ _ = []
        procesarMovimientos (m:ms) e cam vis
            | isValidMove e m = verificarInclusion (applyMove e m) m ms e cam vis
            | otherwise       = procesarMovimientos ms e cam vis

        verificarInclusion nuevoE m ms e cam vis
            | elem nuevoE vis = procesarMovimientos ms e cam vis
            | otherwise       = (nuevoE, cam ++ [nuevoE]) : procesarMovimientos ms e cam vis
