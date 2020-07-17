{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Test.Tasty.QuickCheck.Laws.Comonad
-- Description : Tasty trees of QuickCheck properties for Comonad laws
-- Copyright   : (c) Boris Buliga, 2020
-- License     : MIT
-- Maintainer  : boris@d12frosted.io
-- Stability   : experimental
-- Portability : POSIX
--
-- Ready to use tasty trees of QuickCheck properties for @Comonad@ laws. To get
-- started, take a look at @testComonadLaws@.
module Test.Tasty.QuickCheck.Laws.Comonad
  ( testComonadLaws,
    testComonadLawRightIdentity,
    testComonadLawLeftIdentity,
    testComonadLawAssociativity,
  )
where

--------------------------------------------------------------------------------

import Control.Comonad
import Data.Data (Proxy)
import Data.Typeable (Typeable, typeRep)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.QuickCheck
  ( Arbitrary (..),
    CoArbitrary (..),
    testProperty,
  )
import Text.Show.Functions ()

--------------------------------------------------------------------------------

-- | Constructs a @TestTree@ checking that the @Comonad@ class laws hold for @m@
-- with value types @a@, @b@, and @c@, using a given equality test for values of
-- type @forall u. m u@. The equality context type @t@ is for constructors @m@
-- from which we can only extract a value within a context, such as reader-like
-- constructors.
testComonadLaws ::
  ( Comonad m,
    Eq a,
    Eq b,
    Eq c,
    Show t,
    Show (m a),
    Arbitrary t,
    Arbitrary (m a),
    Arbitrary b,
    Arbitrary c,
    CoArbitrary (m a),
    CoArbitrary (m b),
    Typeable m,
    Typeable a,
    Typeable b,
    Typeable c
  ) =>
  -- | Type constructor under test
  Proxy m ->
  -- | Equality context for @m@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Value type
  Proxy b ->
  -- | Value type
  Proxy c ->
  -- | Equality test
  (forall u. (Eq u) => t -> m u -> m u -> Bool) ->
  -- | Equality test
  (forall u. (Eq u) => t -> u -> u -> Bool) ->
  TestTree
testComonadLaws pm pt pa pb pc eq eq0 =
  let label =
        "Comonad Laws for " ++ show (typeRep pm) ++ " with "
          ++ "a :: "
          ++ show (typeRep pa)
          ++ ", "
          ++ "b :: "
          ++ show (typeRep pb)
          ++ ", "
          ++ "c :: "
          ++ show (typeRep pc)
   in testGroup
        label
        [ testComonadLawRightIdentity pm pt pa eq,
          testComonadLawLeftIdentity pm pt pa pb eq0,
          testComonadLawAssociativity pm pt pa pb pc eq
        ]

--------------------------------------------------------------------------------

-- | @extend extract === id@
testComonadLawRightIdentity ::
  ( Comonad m,
    Eq a,
    Show t,
    Show (m a),
    Arbitrary t,
    Arbitrary (m a)
  ) =>
  -- | Type constructor under test
  Proxy m ->
  -- | Equality context for @m@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Equality test
  (forall u. (Eq u) => t -> m u -> m u -> Bool) ->
  TestTree
testComonadLawRightIdentity pm pt pa eq =
  testProperty "extend extract === id" $ comonadLawRightIdentity pm pt pa eq

comonadLawRightIdentity ::
  (Comonad m, Eq a) =>
  -- | Type constructor under test
  Proxy m ->
  -- | Equality context for @m@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Equality test
  (forall u. (Eq u) => t -> m u -> m u -> Bool) ->
  t ->
  m a ->
  Bool
comonadLawRightIdentity _ _ _ eq t x = eq t (extend extract x) x

--------------------------------------------------------------------------------

-- | @extract . extend f === f@
testComonadLawLeftIdentity ::
  ( Comonad m,
    Eq b,
    Show t,
    Show (m a),
    Arbitrary t,
    Arbitrary (m a),
    Arbitrary b,
    CoArbitrary (m a)
  ) =>
  -- | Type constructor under test
  Proxy m ->
  -- | Equality context for @m@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Equality test
  Proxy b ->
  -- | Equality test
  (forall u. (Eq u) => t -> u -> u -> Bool) ->
  TestTree
testComonadLawLeftIdentity pm pt pa pb eq =
  testProperty "extract . extend f === f" $ comonadLawLeftIdentity pm pt pa pb eq

comonadLawLeftIdentity ::
  (Comonad m, Eq b) =>
  -- | Type constructor under test
  Proxy m ->
  -- | Equality context for @m@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Value type
  Proxy b ->
  -- | Equality test
  (forall u. (Eq u) => t -> u -> u -> Bool) ->
  t ->
  m a ->
  (m a -> b) ->
  Bool
comonadLawLeftIdentity _ _ _ _ eq t x f = eq t (extract . extend f $ x) (f x)

--------------------------------------------------------------------------------
--

-- | @extend f . extend g = extend (f . extend g)@
testComonadLawAssociativity ::
  ( Comonad m,
    Eq c,
    Show t,
    Show (m a),
    Arbitrary t,
    Arbitrary (m a),
    Arbitrary b,
    Arbitrary c,
    CoArbitrary (m a),
    CoArbitrary (m b)
  ) =>
  -- | Type constructor under test
  Proxy m ->
  -- | Equality context for @m@
  Proxy t ->
  -- | Value type
  Proxy a ->
  -- | Value type
  Proxy b ->
  -- | Value type
  Proxy c ->
  -- | Equality test
  (forall u. (Eq u) => t -> m u -> m u -> Bool) ->
  TestTree
testComonadLawAssociativity pm pt pa pb pc eq =
  testProperty "extend f . extend g = extend (f . extend g)" $
    comonadLawAssociativity pm pt pa pb pc eq

comonadLawAssociativity ::
  ( Comonad m,
    Eq c
  ) =>
  Proxy m ->
  Proxy t ->
  Proxy a ->
  Proxy b ->
  Proxy c ->
  (forall u. (Eq u) => t -> m u -> m u -> Bool) ->
  t ->
  m a ->
  (m b -> c) ->
  (m a -> b) ->
  Bool
comonadLawAssociativity _ _ _ _ _ eq t x f g =
  eq t (extend f . extend g $ x) (extend (f . extend g) x)

--------------------------------------------------------------------------------
