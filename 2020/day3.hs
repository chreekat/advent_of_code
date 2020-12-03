import Control.Monad.Trans.State.Lazy

main = do
    z <- readFile "day3-input.txt"
    let x = map (map (\c -> case c of{ '#' -> 1; _ -> 0})) (lines z)

    let cases = [ (1, 1)
                , (3, 1)
                , (5, 1)
                , (7, 1)
                , (1, 2)
                ]

    print $ product $ map snd $ map (\(h, v) -> execState (mkState h v x) (0,0)) $ cases

mkState h v = traverse (\l -> (modify (\s -> step h s l)))  . map head . chunksOf v

step horiz (i, ct) row = (i + horiz, ct + (row !! (i `mod` length row)))
{-



    Right 1, down 1.
    Right 3, down 1. (This is the slope you already checked.)
    Right 5, down 1.
    Right 7, down 1.
    Right 1, down 2.


-}

-- Stolen from split package
build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
build g = g (:) []

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls)) where
  splitter :: [e] -> ([e] -> a -> a) -> a -> a
  splitter [] _ n = n
  splitter l c n  = l `c` splitter (drop i l) c n
