{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Axiom ( Axiom(..), toSAS ) where

import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int
import FastDownward.SAS.DomainIndex ( DomainIndex )
import qualified FastDownward.SAS.DomainIndex as DomainIndex
import FastDownward.SAS.VariableAssignment ( VariableAssignment )
import qualified FastDownward.SAS.VariableAssignment as VariableAssignment
import FastDownward.SAS.VariableIndex ( VariableIndex )
import qualified FastDownward.SAS.VariableIndex as VariableIndex


data Axiom =
  Axiom
    { variable :: VariableIndex
    , conditions :: Seq VariableAssignment
    , pre :: DomainIndex
    , post :: DomainIndex
    }
  deriving
    ( Show )


toSAS :: Axiom -> Data.Text.Lazy.Builder.Builder
toSAS Axiom{..} =
     "begin_rule\n"
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length conditions ) <> "\n"
  <> foldMap ( \x -> VariableAssignment.toSAS x <> "\n" ) conditions
  <> VariableIndex.toSAS variable <> " " <> DomainIndex.toSAS pre <> " " <> DomainIndex.toSAS post <> "\n"
  <> "end_rule"
