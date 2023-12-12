module TwoD where
import Data.Array (Array)
import qualified Data.Array as Array

type TwoD a = Array (Int, Int) a

twoD' :: (Char -> a) -> String -> TwoD a
twoD' f s =
    let ls = lines s
        h = length ls
        w = length (head ls)
     in Array.listArray ((0, 0), (h - 1, w - 1)) (map f (concat ls))

twoD = twoD' id

showTwoD :: (a -> Char) -> TwoD a -> String
showTwoD f g =
    let ((x1, y1), (x2, y2)) = Array.bounds g
     in unlines
            [ [ f (g Array.! (x, y)) | y <- [y1 .. y2] ]
            | x <- [x1 .. x2]
            ]
