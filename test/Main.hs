--------------------------------------------------------------------------------

-- |
-- Module      : Main
-- Description : Tests for Comonad laws tests
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
module Main where

--------------------------------------------------------------------------------

import Data.Proxy
import Test.Tasty
import Test.Tasty.QuickCheck.Laws.Comonad

--------------------------------------------------------------------------------

main :: IO ()
main =
  defaultMain $
    testGroup
      "Comonad Laws"
      [ testComonadLaws (pP pI) pU pU pB pI (const (==)) (const (==))
      ]

--------------------------------------------------------------------------------

pU :: Proxy ()
pU = Proxy

pB :: Proxy Bool
pB = Proxy

pI :: Proxy Int
pI = Proxy

pP :: Proxy a -> Proxy ((,) a)
pP = const Proxy

--------------------------------------------------------------------------------
