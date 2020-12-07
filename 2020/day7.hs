import Data.Char
import Data.Bifunctor
import Data.Function
import Data.List
import Data.List.Split
import Data.Foldable
import Data.Functor.Compose
import qualified Data.Map.Lazy as Map
import Control.Monad.Trans.State.Lazy
import System.IO.Unsafe

import Debug.Pretty.Simple

main = print (part1, part2)


pRule x =
    let [label, contains'] = splitOn " bags contain " x
        contains = takeWhile (/= '.') contains'
        entries =
            if contains == "no other bags"
            then []
            else map pBag (splitOn ", " contains)
    in (label, entries)

pBag :: String -> (Int, String)
pBag b =
    let (barf : _) = splitOn " bag" b
        (count:rest) = words barf
    in (read count, unwords rest)

type Rule = (String,[(Int, String)])

reaches' :: String -> Map.Map String Bool -> Rule -> (String, Bool)
reaches' targ st (label, entries') =
    let entries = map snd entries'
    in (label 
        , or
            (Compose
                ( pure (targ `elem` entries)
                : map (Map.lookup `flip` st) entries
                )
            )
        )

proc
    :: Ord k
    => [Rule]
    -> (Map.Map k a -> Rule -> (k, a))
    -> (Map.Map k a -> result)
    -> result 
proc rules step final =
    let m = fix f
        f m' = Map.fromList ((map (step m')) rules)
    in final m


reaches targ rules = proc rules (reaches' targ) (Map.size . Map.filter id)


test = unsafePerformIO (readFile "day7-test.txt")
input = unsafePerformIO (readFile "day7-input.txt")

part1 = reaches "shiny gold" (map pRule (lines input))


prod (a,b) = a * b

holds' :: Map.Map String Int -> Rule -> (String, Int)
holds' st (label, entries) =
    (label 
    , sum $ fmap prod $ getCompose $ fmap (+ 1)
        (Compose (Compose
            (map (sequence . second (Map.lookup `flip` st)) entries)
        ))
    )

holds targ rules = proc rules holds' (Map.lookup targ)

part2 = holds "shiny gold" (map pRule (lines input))
