{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Unused LANGUAGE pragma" #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}
{-# HLINT ignore "Use tuple-section" #-}
{-# HLINT ignore "Redundant map" #-}
{-# OPTIONS_GHC -Wno-deprecations #-}
{-# HLINT ignore "Move brackets to avoid $" #-}
{-# HLINT ignore "Use infix" #-}

import Tropes

ex1, dat :: String
{-# NOINLINE ex1 #-}
ex1 = unsafePerformIO (readFile "XXX-ex1.txt")
{-# NOINLINE dat #-}
dat = unsafePerformIO (readFile "XXX.txt")

ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

main = print ans1
