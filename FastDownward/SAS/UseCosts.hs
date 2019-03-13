{-# language OverloadedStrings #-}

module FastDownward.SAS.UseCosts ( UseCosts(..), toSAS ) where

import qualified Data.Text.Lazy.Builder


data UseCosts =
  UseCosts | NoCosts
  deriving
    ( Show )


toSAS :: UseCosts -> Data.Text.Lazy.Builder.Builder
toSAS a =
     "begin_metric\n"
  <> case a of
      NoCosts ->
        "0\n"

      UseCosts ->
        "1\n"
  <> "end_metric"
