{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day14-ex1.txt")
dat = unsafePerformIO (readFile "day14.txt")

data I = I {is :: String, im :: Map String Char}
    deriving (Eq, Show)

instance Semigroup I where
    I s m <> I s2 m2 = I (s <> s2) (m <> m2)

instance Monoid I where
    mempty = I mempty mempty

pTmpl s = I s mempty

pRule s =
    let [k, [v]] = splitOn " -> " s
     in I "" (mapSingleton k v)

pI s =
    let [[tmpl], rules] = splitOn [""] $ lines s
     in mconcat (pTmpl tmpl : map pRule rules)

step :: I -> _
step I{is, im} = I newIs im
  where
    newIs = head is : mconcat (zipWith (expand im) is (tail is))

expand :: Map String Char -> Char -> Char -> String
expand m c1 c2 = maybe [c2] (: [c2]) (mapLookup [c1, c2] m)

score I{is} =
    let m = mapUnionsWith (<>) (map (\c -> mapSingleton c (Sum 1)) is)
        (j, k) = (minimum &&& maximum) m
     in getSum $ k - j

scoreSteps n s = score $ last $ take (n + 1) $ iterate step (pI s)

ans1 :: _
ans1 = scoreSteps 10 dat

counts I{is, im} n = mapUnionsWith (<>) (mapSingleton (head is) (Sum 1) : zipWith (aoeu im n) is (tail is))

aoeu _ 0 _ c2 = mapSingleton c2 (Sum 1)
aoeu m n c1 c2 =
    let me = mapLookup [c1, c2] m
     in case me of
            Nothing -> mapSingleton c2 (Sum 1)
            Just e ->
                let m1 = aoeu m (n - 1) c1 e
                    m2 = aoeu m (n - 1) e c2
                 in m1 `seq` m2 `seq` mapUnionsWith (<>) [m1, m2]

-- @oeui m n c1 c2@ builds a count of all digits by expanding the string
-- @[c1,c2]@ @n@ times.
--
-- If n = 0, then it's just 1 each for c1 and c2.
-- Otherwise, if [c1,c2] don't expand, it's still just 1 each for c1 and c2.
-- Finally, we first expand [c1,e] (n-1) times. Then, we expand [e,c2] (n-1)
-- times. Then we sum them up.
--
-- In all cases, we have to avoid overcounting. To do that, oeui doesn't
-- actually count c1! That's what qjkx is for.
qjkx m n c1 c2 = do
    plop <- oeui m n c1 c2
    let res = mapUnionWith (<>) (mapSingleton c1 (Sum 1)) plop
    res <$ modify (mapInsert ([c1, c2], n) res)

oeui m 0 c1 c2 = pure (mapSingleton c2 (Sum 1))
oeui m n c1 c2 = do
    st <- get
    case mapLookup ([c1, c2], n) st of
        Just r -> pure r
        Nothing -> case mapLookup [c1, c2] m of
            Nothing ->
                let r = mapSingleton c2 (Sum 1)
                 in r <$ modify (mapInsert ([c1, c2], n) r)
            Just e -> do
                r1 <- oeui m (n - 1) c1 e -- c1 is accounted for outside of oeui
                r2 <- oeui m (n - 1) e c2 -- e is accounted for by r1.
                let res = mapUnionWith (<>) r1 r2
                res <$ modify (mapInsert ([c1, c2], n) res)

counts2 I{is, im} n = mapUnionsWith (<>) (evalState (zipWithM (oeui im n) is (tail is)) mempty)

score2 s n =
    let m = counts2 (pI s) n
        (j, k) = (minimum &&& maximum) m
     in getSum $ k - j

ans2 :: _
ans2 = score2 dat 40

-- ghcid needs this?
main = undefined
