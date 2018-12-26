{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Goal ( Goal(..), toSAS ) where

import Data.String ( fromString )
import qualified Data.Text.Lazy
import FastDownward.SAS.VariableAssignment ( VariableAssignment )
import qualified FastDownward.SAS.VariableAssignment as VariableAssignment


newtype Goal =
  Goal { finalAssignments :: [ VariableAssignment ] }
  deriving
    ( Show )


toSAS :: Goal -> Data.Text.Lazy.Text
toSAS Goal{..} =
  Data.Text.Lazy.intercalate
    "\n"
    [ "begin_goal"
    , fromString ( show ( length finalAssignments ) )
    , Data.Text.Lazy.intercalate
        "\n"
        ( map VariableAssignment.toSAS finalAssignments )
    , "end_goal"
    ]
