{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Operator ( Operator(..), toSAS ) where

import Data.Function ( (&) )
import Data.String ( fromString )
import qualified Data.Text.Lazy
import FastDownward.SAS.Effect ( Effect )
import qualified FastDownward.SAS.Effect as Effect
import FastDownward.SAS.VariableAssignment ( VariableAssignment )
import qualified FastDownward.SAS.VariableAssignment as VariableAssignment


data Operator =
  Operator
    { name :: Data.Text.Lazy.Text
    , prevail :: [ VariableAssignment ]
    , effects :: [ Effect ]
    }
  deriving
    ( Show )


toSAS :: Operator -> Data.Text.Lazy.Text
toSAS Operator{..} =
  [ "begin_operator"
  , name
  , fromString ( show ( length prevail ) )
  , Data.Text.Lazy.intercalate "\n" ( map VariableAssignment.toSAS prevail )
  , fromString ( show ( length effects ) )
  , Data.Text.Lazy.intercalate "\n" ( map Effect.toSAS effects )
  , "0"
  , "end_operator"
  ]
    & filter ( not . Data.Text.Lazy.null )
    & Data.Text.Lazy.intercalate "\n"
