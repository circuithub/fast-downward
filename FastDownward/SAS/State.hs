{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.State ( State(..), toSAS ) where

import Data.Sequence ( Seq )
import qualified Data.Text.Lazy.Builder
import FastDownward.SAS.DomainIndex ( DomainIndex )
import qualified FastDownward.SAS.DomainIndex as DomainIndex


newtype State =
  State { initialValues :: Seq DomainIndex }
  deriving
    ( Show )


toSAS :: State -> Data.Text.Lazy.Builder.Builder
toSAS State{..} =
     "begin_state\n"
  <> foldMap ( \x -> DomainIndex.toSAS x <> "\n" ) initialValues
  <> "end_state"
