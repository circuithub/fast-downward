{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.MutexGroup ( MutexGroup(..), toSAS ) where

import Data.String ( fromString )
import qualified Data.Text.Lazy
import FastDownward.SAS.VariableAssignment ( VariableAssignment )
import qualified FastDownward.SAS.VariableAssignment as VariableAssignment


newtype MutexGroup =
  MutexGroup { assignments :: [ VariableAssignment ] }
  deriving
    ( Show )


toSAS :: MutexGroup -> Data.Text.Lazy.Text
toSAS MutexGroup{..} =
  Data.Text.Lazy.intercalate
    "\n"
    [ "begin_mutex_group"
    , fromString ( show ( length assignments ) )
    , Data.Text.Lazy.intercalate
        "\n"
        ( map VariableAssignment.toSAS assignments )
    , "end_mutex_group"
    ]
