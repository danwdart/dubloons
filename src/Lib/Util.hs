{-# LANGUAGE UnicodeSyntax #-}
module Lib.Util where

indexList ∷ [a] → [(Int, a)]
indexList x = indexList' x $ length x

indexList' ∷ [a] → Int → [(Int, a)]
indexList' [] _         = []
indexList' (a:as) total = (total - length as, a):indexList' as total
