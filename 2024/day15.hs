{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# HLINT ignore "Use if" #-}
{-# HLINT ignore "Use infix" #-}
{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

import Array qualified
import Control.Monad.State qualified as State
import Control.Monad.Writer.CPS as Writer
import Data.Foldable qualified as Fold
import Data.List qualified as List
import Data.List.Split qualified as Split
import Data.Maybe qualified as Maybe
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Vector qualified as Vector
import HMap qualified
import Map qualified
import Seq qualified
import Set qualified
import Tropes hiding (traceShow, traceShowId, range)
import Tropes qualified
import TwoD qualified

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day15-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day15.txt")

ans1 :: _
ans1 =
    let [map_, moves] = pars dat
        Just me = Array.find (== '@') m
        m = TwoD.twoD map_
        final = snd $ List.foldl' move (me,m) $ concat $ lines moves -- TwoD.showTwoD (snd (List.foldl' move moves (me,m)))
        gps ((x,y),c) = if c == 'O' then 100 * x + y else 0
    in show $ sum $ map gps $ Array.assocs final

move (i,m) d =
    let ups = updates dir i m
        Just newI = dir i m
        dir = case d of
            '>' -> TwoD.east
            '<' -> TwoD.west
            '^' -> TwoD.north
            'v' -> TwoD.south
            _ -> error "bad direction"
    in case ups of
        Nothing -> (i,m)
        Just ups' -> (newI, m Array.// ((i,'.') : ups'))
-- We moved i. Now we create the list of other nodes that need updating.
-- If we're a wall, fail!
-- If we're an empty space, no more updates
-- Otherwise, put i's cell in i', and update i'.
updates dir i m =
    let cell = m Array.! i
        newI = dir i m
    in case cell of
        '#' -> Nothing
        '.' -> pure []
        _ -> case newI of
            Nothing -> Nothing
            Just i' -> fmap ((i', cell):) (updates dir i' m)

ans2 :: _
ans2 =
    let [map_, moves] = pars dat
        m = TwoD.twoD' blowup map_
        n = TwoD.twoD' id map_
        blown = TwoD.twoD (TwoD.showTwoD'' id m)
        objects = Set.fromList [(i, c) | (i, c) <- Array.assocs blown, c `elem` "[#@"]
        Just robot = Array.find (=='@') blown
        range = Array.bounds blown
        gps ((x,y),c) = if c == '[' then 100 * x + y else 0
        final = reTwoD range $ snd $ allMoves range ((robot,'@'),objects) (concat $ lines moves)
    in show $ sum $ map gps $ Array.assocs final
    -- in TwoD.showTwoD final <> 

allMoves range (i,objects) [] = (i,objects)
allMoves range (i,objects) (d:ds) =
    let (i',objects') = bigmove range objects i d
    in allMoves range (i',objects') ds
    -- in Tropes.trace (TwoD.showTwoD $ reTwoD range objects') $ allMoves range (i',objects') ds

-- Rebuild a twod out of the known range and the list of known objects
reTwoD :: ((Int,Int),(Int,Int)) -> Set.Set ((Int,Int),Char) -> TwoD.TwoD Char
reTwoD range objects = Array.array range $ do
    i <- Array.range range
    pure $ case Map.lookup i mobjects of
        Just c -> (i,c)
        Nothing -> case Map.lookup (TwoD.west' i) mobjects of
            Just '[' -> (i,']')
            _ -> (i,'.')
    where mobjects = Map.fromList $ Set.toList objects
-- at each step, move us up.
--
-- then iterate (or fail).
--
-- to iterate moving the robot up, look at what's above it. if there's a boulder at (north i) or
-- (northwest i), iterate on moving the boulder up.
--
-- if there's a wall at (north i), fail.
--
-- if there's an empty space at (north i), succeed.
--
-- to move a boulder up, look at what's above it. If there's a boulder at (north
-- i) or (northeast i) or (northwest i), iterate on moving those boulders up.
--
-- todo: walls and empty spaces
--
-- fundamentally, how do we finalize a move? Well, if we keep (i,c) in a set, we
-- can lookup a boulder at i with Set.member (i,'['). And we can move a bolder
-- with Set.delete (i,'[') . Set.insert (i','[') .

data Update i c = Update i i c
    deriving (Eq, Show)

moveThing (Update i i' c) = Set.delete (i,c) . Set.insert (i',c)


bigmove range s (i,c) dirr =
    let ups = fromMaybe [] $ flip State.evalState Set.empty $ bigupdates s range dirr (i,c)
        newLoc = if List.null ups then i else dir i
        dir = case dirr of
            '^' -> TwoD.north'
            'v' -> TwoD.south'
            '>' -> TwoD.east'
            '<' -> TwoD.west'
            _ -> error "bad newLoc"
    in ((newLoc,c), foldr moveThing s ups)

bigupdates :: State.MonadState (Set.Set (Int,Int)) m => _ -> _ -> _ -> _ -> m (Maybe [Update (Int,Int) Char])
bigupdates s range dirr (i,thing) = go where
        myUpdate = Update i newLoc thing
        newLoc = case dirr of
            '^' -> TwoD.north' i
            'v' -> TwoD.south' i
            '>' -> TwoD.east' i
            '<' -> TwoD.west' i
            _ -> error "bad newLoc"
        maybeDir (dx,dy) (i_x,i_y) = let
            i' = (i_x + dx, i_y + dy)
            in if Array.inRange range i' then Just i' else Nothing

        north' = maybeDir (-1,0)
        south' = maybeDir (1,0)
        east' = maybeDir (0,1)
        west' = maybeDir (0,-1)
        luode' = maybeDir (-1,-1)
        lounas' = maybeDir (1,-1)
        koillis' = maybeDir (-1,1)
        kaakko' = maybeDir (1,1)

        boulderDirs = case (dirr,thing) of
            ('^','@') -> [north', luode']
            ('^','[') -> [north', luode', koillis']
            ('v','@') -> [south', lounas']
            ('v','[') -> [south', lounas', kaakko']
            ('>','@') -> [east']
            ('>','[') -> [east' <=< east']
            ('<','@') -> [west' <=< west']
            ('<','[') -> [west' <=< west']
            _ -> error "bad boulderDirs"
        wallDirs = case (dirr, thing) of
            ('^', '@') -> [north']
            ('^', '[') -> [north', koillis']
            ('v', '@') -> [south']
            ('v', '[') -> [south', kaakko']
            ('>', '@') -> [east']
            ('>', '[') -> [east' <=< east']
            ('<', '@') -> [west']
            ('<', '[') -> [west']
            _ -> error $ "bad wallDirs: " <> show (dirr, thing)
        go = do
            seen <- State.get
            case Set.member i seen of
                True -> pure $ Just []
                False -> fmap (myUpdate :) <$> do 
                    State.modify (Set.insert i)
                    case mapMaybe ($ i) wallDirs of
                        -- Fail, out of space
                        [] -> pure Nothing
                        xs -> case findWall xs s of
                            -- Fail, ran into wall
                            (_:_) -> pure Nothing
                            -- No wall, continue
                            [] -> let bdirs = mapMaybe ($ i) boulderDirs
                                in case findBoulders bdirs s of
                                    -- No boulders, we're done
                                    [] -> pure $ Just []
                                    -- Boulders, move them
                                    bs -> do
                                        updatedBs <- traverse (bigupdates s range dirr) bs
                                        pure $ fmap concat $ sequence updatedBs

findWall is s = mapMaybe (flip Set.lookup s . (\i -> (i,'#'))) is
findBoulders is s = mapMaybe (flip Set.lookup s . (\i -> (i,'['))) is

blowup '#' = "##"
blowup '.' = ".."
blowup '@' = "@."
blowup 'O' = "[]"
blowup _ = error "bad blowup"

main = putStrLn ans2
