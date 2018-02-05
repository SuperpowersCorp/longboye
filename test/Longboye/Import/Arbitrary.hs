{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Longboye.Import.Arbitrary () where

import Longboye.Prelude

import Data.Text.Arbitrary       ()
import Longboye.Import           ( Import( Import ) )
import Longboye.Member.Arbitrary ()
import Test.QuickCheck           ( Arbitrary
                                 , arbitrary
                                 )

instance Arbitrary Import where
  arbitrary = do
    qual     <- arbitrary
    impMod   <- arbitrary
    asClause <- arbitrary
    hiding   <- arbitrary
    members  <- arbitrary
    return $ Import qual impMod asClause hiding members
