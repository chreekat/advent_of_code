{-# LANGUAGE PartialTypeSignatures #-}
import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "ex4-1.txt")

dat = unsafePerformIO (readFile "day4.txt")


data MyState = MyState { remaining :: [Int], boards :: [Board] }
    deriving (Eq, Show)

data Board = Board { lines' :: [[Cell]], allCells :: [Cell] }
    deriving (Eq, Show)

data Cell = Cell { num :: Int, marked :: Bool }
    deriving (Eq, Show)

-- UNFOLDING
--

mkState s =
    let (p:ps) = pars s
        rem = map read (splitOn "," p)
        boards = map mkBoard ps
    in MyState rem boards


mkCell n = Cell n False

mkBoard :: String -> Board
mkBoard s =
    let ls   = map (map (mkCell . read) . words) (lines s)
        cols = map (map (mkCell . read)) $ transpose $ map words (lines s)
        all = map (mkCell . read) (words s)
    in Board (ls <> cols) all


-- FOLDING
--

winner :: Board -> Bool
winner = any (getAll . foldMap (All . marked)) . lines'

newBoard :: Int -> Board -> (Board, Maybe Int)
newBoard n oldBoard =
    let newb = mark oldBoard n
        s = if winner newb
                then Just (score n newb)
                else Nothing
    in (newb, s)

score n = (n *) .  sum . map num . filter (not . marked) . allCells

mark (Board l a) n =
    let l' = map (markLine n) l
        a' = map (markCell n) a
    in Board l' a'

markLine n = map (markCell n)

markCell n c@(Cell num' m) 
    | num' == n = Cell num' True
    | otherwise = c

gameOver = getFirst . foldMap (First . snd)

step (MyState ns bs)
    | [] <- ns = error "Game over!"
    | (n:rem) <- ns = 
        let bs' = map (newBoard n) bs
        in (rem, bs')

playGame st = 
    let (rem, bs') = step st
    in case gameOver  bs' of
        Just s -> {-pTraceShow (MyState rem (map fst bs')) $-} Just s
        Nothing -> playGame (MyState rem (map fst bs'))

-- WRAP UP
--

ans1 :: _
ans1 = playGame (mkState dat)

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
