{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Plan ( Plan(..), toSAS ) where

import qualified Data.Text.Lazy
import qualified Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Builder.Int
import Data.Sequence ( Seq )
import qualified Data.Sequence as Seq
import FastDownward.SAS.Axiom ( Axiom )
import qualified FastDownward.SAS.Axiom as Axiom
import FastDownward.SAS.Goal ( Goal )
import qualified FastDownward.SAS.Goal as Goal
import FastDownward.SAS.MutexGroup ( MutexGroup )
import FastDownward.SAS.Operator ( Operator )
import qualified FastDownward.SAS.Operator as Operator
import FastDownward.SAS.State ( State )
import qualified FastDownward.SAS.State as State
import FastDownward.SAS.UseCosts ( UseCosts )
import qualified FastDownward.SAS.UseCosts as UseCosts
import FastDownward.SAS.Variable ( Variable )
import qualified FastDownward.SAS.Variable as Variable
import FastDownward.SAS.Version ( Version )
import qualified FastDownward.SAS.Version as Version


data Plan =
  Plan
    { version :: Version
    , useCosts :: UseCosts
    , variables :: Seq Variable
    , mutexGroups :: Seq MutexGroup
    , initialState :: State
    , goal :: Goal
    , operators :: Seq Operator
    , axioms :: Seq Axiom
    }
  deriving
    ( Show )


toSAS :: Plan -> Data.Text.Lazy.Text
toSAS Plan{..} =
     Data.Text.Lazy.Builder.toLazyText
   $ Version.toSAS version                                        <> "\n"
  <> UseCosts.toSAS useCosts                                       <> "\n"
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length variables )   <> "\n"
  <> foldMap ( \v -> Variable.toSAS v <> "\n" ) variables
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length mutexGroups ) <> "\n"
  <> State.toSAS initialState                                      <> "\n"
  <> Goal.toSAS goal                                               <> "\n"
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length operators )   <> "\n"
  <> foldMap ( \x -> Operator.toSAS x <> "\n" ) operators
  <> Data.Text.Lazy.Builder.Int.decimal ( Seq.length axioms )      <> "\n"
  <> foldMap ( \x -> Axiom.toSAS x <> "\n" ) axioms
