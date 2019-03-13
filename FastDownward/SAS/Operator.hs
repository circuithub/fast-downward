{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Operator ( Operator(..), toSAS ) where

import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int
import FastDownward.SAS.Effect ( Effect )
import qualified FastDownward.SAS.Effect as Effect
import FastDownward.SAS.VariableAssignment ( VariableAssignment )
import qualified FastDownward.SAS.VariableAssignment as VariableAssignment


data Operator =
  Operator
    { name :: Data.Text.Lazy.Text
    , prevail :: Seq VariableAssignment
    , effects :: Seq Effect
    }
  deriving
    ( Show )


toSAS :: Operator -> Data.Text.Lazy.Builder.Builder
toSAS Operator{..} =
     "begin_operator\n"
  <> Data.Text.Lazy.Builder.fromLazyText name <> "\n"
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length prevail ) <> "\n"
  <> foldMap ( \x -> VariableAssignment.toSAS x <> "\n" ) prevail
  <> Data.Text.Lazy.Builder.Int.decimal ( length effects ) <> "\n"
  <> foldMap ( \x -> Effect.toSAS x <> "\n" ) effects
  <> "0\n"
  <> "end_operator"
