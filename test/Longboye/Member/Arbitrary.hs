{-# LANGUAGE NoImplicitPrelude    #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Longboye.Member.Arbitrary () where

import Data.Text.Arbitrary ()
import Longboye.Member     ( Member( NamedMember
                                   , OpMember
                                   )
                           )
import Longboye.Prelude
import Test.QuickCheck     ( Arbitrary
                           , arbitrary
                           )

instance Arbitrary Member where
  arbitrary = do
    coin <- arbitrary
    name <- arbitrary -- TODO: valid names only
    if coin
      then arbNamed name
      else arbOp name
    where
      arbNamed name = do
        b <- arbitrary
        return $ NamedMember name b
      arbOp name = do
        ops <- arbitrary
        return $ OpMember name ops
