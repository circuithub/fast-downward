{-# language OverloadedStrings #-}
{-# language RecordWildCards #-}

module FastDownward.SAS.State ( State(..), toSAS ) where

import qualified Data.Text.Lazy
import FastDownward.SAS.DomainIndex ( DomainIndex )
import qualified FastDownward.SAS.DomainIndex as DomainIndex


newtype State =
  State { initialValues :: [ DomainIndex ] }
  deriving
    ( Show )


toSAS :: State -> Data.Text.Lazy.Text
toSAS State{..} =
  Data.Text.Lazy.intercalate
    "\n"
    [ "begin_state"
    , Data.Text.Lazy.intercalate "\n" ( map DomainIndex.toSAS initialValues )
    , "end_state"
    ]
