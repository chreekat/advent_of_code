{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Redundant map" #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
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
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Seq qualified
import Data.Sequence (Seq ((:<|), (:|>)))
import Data.Vector qualified as Vector
import TwoD qualified
import Tropes hiding (traceShow, traceShowId)
import Tropes qualified
import HMap qualified

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "day9-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "day9.txt")

ans1 :: _
ans1 = g 0 0 $ Seq.fromList $ f 0 [] $ map (read @Int . (:[])) dat

f _ z [] = reverse z
f i z (d:s:fs) = f (succ i) (Left s : Right (i,d) : z) fs
f i z (d:fs) = f (succ i) (Right (i,d) : z) fs

g sum_ _ Seq.Empty = sum_
-- Ignore empty space at end; ignore sole remaining empty space
g sum_ ptr (rest :|> Left _) = g sum_ ptr rest
g sum_ _ (Left _ :<| Seq.Empty) = sum_

g sum_ ptr (Right (i,sz) :<| rest) =
    let newSum = sum_ + sum [ i * j | j <- [ptr..newPtr - 1]]
        newPtr = ptr + sz
    in g newSum newPtr rest

-- if j takes up more space than is available, fill all space, drop i, and keep
-- j
-- if j takes up less space than available, fill up as much as possible, drop j,
-- keep i
-- equivalently, drop i or j if they have zero size, and only remove (mn sz_t
-- sz_i) from the front.

g sum_ ptr (Left sz_i :<| (rest :|> Right (t, sz_t)))
    | sz_i == 0 = g sum_ ptr (rest :|> Right (t, sz_t))
    | sz_t == 0 = g sum_ ptr (Left sz_i :<| rest)
    | otherwise =
    let diff = min sz_t sz_i
        sz_i' = sz_i - diff
        sz_t' = sz_t - diff
        ptr' = ptr + diff
        newSum = sum_ + sum [ t * j | j <- [ptr..ptr'-1]]
    in g newSum ptr' (Left sz_i' :<| (rest :|> Right (t, sz_t')))
g _ _ _ = error "Wat"

main = print ans2


-- for part 2, i have to look up the next available free space big enough for a
-- file.
-- Then I have to do that for each file.
-- Using a Sequence might be too slow - have to traverse the whole Sequence for
-- each file. Plus bookkeeping to edit a Sequence in the middle.
--  Have to keep track of files that have been processed cause you only process
--  them once.


f' _ z [] = reverse z
f' i z (d:s:fs) = f' (succ i) (Left s : Right (i,d, False) : z) fs
f' i z (d:fs) = f' (succ i) (Right (i,d,False) : z) fs

-- we can't go lift to right directly this time. first go right to left and find
-- new place for files if possible. Do this on a zipper
h (Seq.Zip Seq.Empty focus r) = focus:<|r
h zz@(Seq.Zip _ (Left _) _) = h $ Seq.goLeft zz
-- Skip processed files
h zz@(Seq.Zip _ (Right (_,_,True)) _) = h $ Seq.goLeft zz
h zz@(Seq.Zip lss focus@(Right (i,d,_)) rs) = case findSpace (i,d) lss of
    Nothing -> h $ Seq.goLeft zz
    Just Seq.Empty -> error "Impossible"
    -- We found new space. focus has been splopped somewhere in newLs.
    -- We could optimise by stitching together empty space, but we don't need
    -- it.
    -- But we *do* need to replace focus with empty space.
    Just newLs -> h $ Seq.goLeft $ Seq.Zip newLs (Left d) rs

findSpace :: (Int,Int) -> Seq.Seq (Either Int (Int,Int,Bool)) -> Maybe (Seq.Seq (Either Int (Int,Int,Bool)))
findSpace _ Seq.Empty = Nothing
findSpace (i,d) (l :<| ls) = case l of
    Right _ -> fmap (l :<|) $ findSpace (i,d) ls
    Left spc
        | spc < d -> fmap (l :<|) $ findSpace (i,d) ls
        | spc == d -> Just (Right (i,d,True) :<| ls)
        | otherwise -> Just (Right (i,d,True) :<| Left (spc - d) :<| ls)

-- For summing this time, we've already processed everybody so just go left
-- to right
zsum sum_ _ Seq.Empty = sum_
zsum sum_ ptr (Left sz :<| rest) = zsum sum_ (ptr+sz) rest
zsum sum_ ptr (Right (i,sz,_) :<| rest) =
    let newSum = sum_ + sum [ i * j | j <- [ptr..newPtr - 1]]
        newPtr = ptr + sz
    in zsum newSum newPtr rest


-- Left j is j spaces
-- Right (i,d,_) is d number of i
-- Concatenate in one long string
printDisk :: Seq.Seq (Either Int (Int,Int,Bool)) -> String
printDisk = List.concatMap f . toList
    where f (Left j) = replicate j '.'
          f (Right (i,d,_)) = replicate d (head $ show i)

ans2 :: _
ans2 = let  sq = Seq.fromList $ f' 0 [] $ map (read @Int . (:[])) dat
            funsq = h <$> Seq.toZip (fromIntegral $ length sq) sq
    in snd $ (printDisk <$> funsq, zsum 0 0 <$> funsq)
