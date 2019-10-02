{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Goal ( Goal(..), toSAS ) where

import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int
import FastDownward.SAS.VariableAssignment ( VariableAssignment )
import qualified FastDownward.SAS.VariableAssignment as VariableAssignment


newtype Goal =
  Goal { finalAssignments :: Seq VariableAssignment }
  deriving
    ( Show )


toSAS :: Goal -> Data.Text.Lazy.Builder.Builder
toSAS Goal{..} =
     "begin_goal\n"
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length finalAssignments ) <> "\n"
  <> foldMap ( \x -> VariableAssignment.toSAS x <> "\n" ) finalAssignments
  <> "end_goal"
