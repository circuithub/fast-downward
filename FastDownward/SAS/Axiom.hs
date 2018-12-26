{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.Axiom ( Axiom(..), toSAS ) where

import Data.String ( fromString )
import qualified Data.Text.Lazy
import FastDownward.SAS.DomainIndex ( DomainIndex )
import qualified FastDownward.SAS.DomainIndex as DomainIndex
import FastDownward.SAS.VariableAssignment ( VariableAssignment )
import qualified FastDownward.SAS.VariableAssignment as VariableAssignment
import FastDownward.SAS.VariableIndex ( VariableIndex )
import qualified FastDownward.SAS.VariableIndex as VariableIndex


data Axiom =
  Axiom
    { variable :: VariableIndex
    , conditions :: [ VariableAssignment ]
    , pre :: DomainIndex
    , post :: DomainIndex
    }
  deriving
    ( Show )


toSAS :: Axiom -> Data.Text.Lazy.Text
toSAS Axiom{..} =
  Data.Text.Lazy.intercalate
    "\n"
    [ "begin_rule"
    , fromString ( show ( length conditions ) )
    , Data.Text.Lazy.intercalate "\n" ( map VariableAssignment.toSAS conditions )
    , Data.Text.Lazy.intercalate
        " "
        [ VariableIndex.toSAS variable
        , DomainIndex.toSAS pre
        , DomainIndex.toSAS post
        ]
    , "end_rule"
    ]
