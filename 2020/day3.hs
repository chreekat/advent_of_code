import Control.Monad.Trans.State.Lazy
import Data.List.Split

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
