{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Plan ( Plan(..), toSAS ) where

import Data.Function ( (&) )
import Data.String ( fromString )
import qualified Data.Text.Lazy
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
    , variables :: [ Variable ]
    , mutexGroups :: [ MutexGroup ]
    , initialState :: State
    , goal :: Goal
    , operators :: [ Operator ]
    , axioms :: [ Axiom ]
    }
  deriving
    ( Show )


toSAS :: Plan -> Data.Text.Lazy.Text
toSAS Plan{..} =
  [ Version.toSAS version
  , UseCosts.toSAS useCosts
  , fromString ( show ( length variables ) )
  , Data.Text.Lazy.intercalate "\n" ( map Variable.toSAS variables )
  , fromString ( show ( length mutexGroups ) )
  , Data.Text.Lazy.intercalate "\n" [] -- ( map MutexGroup.toSAS mutexGroups )
  , State.toSAS initialState
  , Goal.toSAS goal
  , fromString ( show ( length operators ) )
  , Data.Text.Lazy.intercalate "\n" ( map Operator.toSAS operators )
  , fromString ( show ( length axioms ) )
  , Data.Text.Lazy.intercalate "\n" ( map Axiom.toSAS axioms )
  ]
    & filter ( not . Data.Text.Lazy.null )
    & Data.Text.Lazy.intercalate "\n"
