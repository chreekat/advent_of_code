{-# LANGUAGE PartialTypeSignatures #-}
import Tropes

ex1 = []

dat :: String
dat = unsafePerformIO (readFile "dayXXX.txt")

ans1 :: _
ans1 = undefined

ans2 :: _
ans2 = undefined

-- ghcid needs this?
main = undefined
