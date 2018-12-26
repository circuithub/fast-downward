{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Effect ( Effect(..), toSAS ) where

import qualified Data.Text.Lazy
import FastDownward.SAS.DomainIndex ( DomainIndex )
import qualified FastDownward.SAS.DomainIndex as DomainIndex
import FastDownward.SAS.VariableIndex ( VariableIndex )
import qualified FastDownward.SAS.VariableIndex as VariableIndex


data Effect =
  Effect
    { variable :: VariableIndex
    , pre :: Maybe DomainIndex
    , post :: DomainIndex
    }
  deriving
    ( Eq, Ord, Show )


toSAS :: Effect -> Data.Text.Lazy.Text
toSAS Effect{..} =
  Data.Text.Lazy.intercalate
    " "
    [ "0"
    , VariableIndex.toSAS variable
    , case pre of
        Nothing ->
          "-1"

        Just x ->
          DomainIndex.toSAS x
    , DomainIndex.toSAS post
    ]
