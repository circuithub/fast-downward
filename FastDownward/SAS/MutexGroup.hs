{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.MutexGroup ( MutexGroup(..), toSAS ) where

import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int
import FastDownward.SAS.VariableAssignment ( VariableAssignment )
import qualified FastDownward.SAS.VariableAssignment as VariableAssignment


newtype MutexGroup =
  MutexGroup { assignments :: Seq VariableAssignment }
  deriving
    ( Show )


toSAS :: MutexGroup -> Data.Text.Lazy.Builder.Builder
toSAS MutexGroup{..} =
     "begin_mutex_group\n"
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length assignments ) <> "\n"
  <> foldMap ( \x -> VariableAssignment.toSAS x <> "\n" ) assignments
  <> "end_mutex_group"
