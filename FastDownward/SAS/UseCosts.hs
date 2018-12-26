{-# language OverloadedStrings #-}

module FastDownward.SAS.UseCosts ( UseCosts(..), toSAS ) where

import qualified Data.Text.Lazy


data UseCosts =
  UseCosts | NoCosts
  deriving
    ( Show )


toSAS :: UseCosts -> Data.Text.Lazy.Text
toSAS a =
  Data.Text.Lazy.intercalate
    "\n"
    [ "begin_metric"
    , case a of
        NoCosts ->
          "0"

        UseCosts ->
          "1"
    , "end_metric"
    ]
