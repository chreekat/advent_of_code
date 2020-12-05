import Control.Monad.Trans.State.Lazy
import Data.Bifunctor
import Data.List
import System.IO.Unsafe

seatId (r, c) = r * 8 + c

step f
    | f `elem` "FL" = modify (\(m,n) -> (m, (n + m) `div` 2))
    | f `elem` "BR" = modify (\(m,n) -> (m + ((n - m) `div` 2) + 1, n))
    -- Intentionally non-total due to runtime considerations


rowSeat = bimap (go 127) (go 7) . splitAt 7
    where
        go n l = fst (execState (traverse step l) (0, n))


input = unsafePerformIO (readFile "day5-input.txt")
allRowSeats = map rowSeat (lines input)

part1 = maximum (map seatId allRowSeats)

step2 ez ea = do
    z <- ez
    a <- ea
    go z a 
  where go z a = if a == z + 1 then Right a else Left (z + 1)

part2 = foldl1' step2 (map pure (sort (map seatId allRowSeats)))

main = print (part1, part2)
