{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "day12-ex1.txt")
dat = unsafePerformIO (readFile "day12.txt")

data V a = V
    { edges :: Set a
    , visited :: Bool
    }
    deriving (Eq, Show)

emptyV = V setEmpty False

-- (<>) preserves visited-ness
instance (Ord a, Semigroup a) => Semigroup (V a) where
    V s1 b1 <> V s2 b2 = V (s1 <> s2) (b1 || b2)

data G = G
    { gm :: Map String (V String)
    , canRevisit :: Bool
    }
    deriving (Eq, Show)

mkSegs = map (splitOn "-") . lines

mkG s = G (mapUnionsWith (<>) . map f . mkSegs $ s) True
  where
    f [a, b] =
        mapFromList
            [ (a, emptyV{edges = (setFromList [b])})
            , (b, emptyV{edges = (setFromList [a])})
            ]

data St = St
    { stg :: G
    }
    deriving (Eq, Show)

-- proc g final v returns all paths starting at v that end at final.
-- That's a list, you knucklehead.
proc :: G -> String -> String -> [[String]]
proc og@(gm -> g) final v =
    let big = all isUpper v
        newG = og{gm = if big then g else mapDelete v g}
        es :: Maybe [String]
        es = toList . edges <$> mapLookup v g
        finished = v == final
     in if finished
            then [[v]]
            else maybe [] (map (v :) . concatMap (proc newG final)) es

ans1 :: _
ans1 = length $ proc (mkG dat) "end" "start"

visit v = v{visited = True}

proc2 g final initial = proc2' g final initial initial

proc2' :: G -> String -> String -> String -> [[String]]
proc2' og@(G{gm, canRevisit}) final initial v =
    let big = all isUpper v
        V (toList -> es) vis = gm ! v
        newGs =
            case (big, not vis, canRevisit) of
                -- big
                (True, _, _) -> [og]
                -- small, not visited
                (_, True, _) -> [og{gm = mapAdjust visit v gm}]
                -- small, visited, canRevisit
                (_, _, True) ->
                    [ og
                        { gm = mapAdjust visit v gm
                        , canRevisit = False
                        }
                    ]
                -- otherwise
                _ -> []
        finished = v == final
        backAtStart = v == initial && vis
     in case (finished, backAtStart) of
            (True, _) -> [[v]]
            (_, True) -> []
            _ ->
                concatMap
                    ( \theG ->
                        map
                            (v :)
                            (concatMap (proc2' theG final initial) es)
                    )
                    newGs

ans2 :: _
ans2 = length $ proc2 (mkG dat) "end" "start"

-- ghcid needs this?
main = undefined
