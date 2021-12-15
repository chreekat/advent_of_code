{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}

import Tropes

ex1, dat :: String
ex1 = unsafePerformIO (readFile "dayXXX-ex1.txt")
dat = unsafePerformIO (readFile "dayXXX.txt")

ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
