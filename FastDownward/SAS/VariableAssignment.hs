{-# language OverloadedStrings #-}

module FastDownward.SAS.VariableAssignment ( VariableAssignment(..), toSAS ) where

import qualified Data.Text.Lazy.Builder
import FastDownward.SAS.DomainIndex ( DomainIndex )
import qualified FastDownward.SAS.DomainIndex as DomainIndex
import FastDownward.SAS.VariableIndex ( VariableIndex )
import qualified FastDownward.SAS.VariableIndex as VariableIndex


data VariableAssignment =
  VariableAssignment VariableIndex DomainIndex
  deriving
    ( Eq, Ord, Show )


toSAS :: VariableAssignment -> Data.Text.Lazy.Builder.Builder
toSAS ( VariableAssignment x y ) =
  VariableIndex.toSAS x <> " " <> DomainIndex.toSAS y
