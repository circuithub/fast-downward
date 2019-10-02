{-# language OverloadedStrings #-}

module FastDownward.SAS.Version ( Version(..), toSAS ) where

import qualified Data.Text.Lazy.Builder


data Version =
  SAS3
  deriving
    ( Show )


toSAS :: Version -> Data.Text.Lazy.Builder.Builder
toSAS v =
     "begin_version\n"
  <> case v of
      SAS3 ->
        "3\n"
  <> "end_version"
